r"""Field-dependent second-order derivative expansion for the Ising bulk.

This module solves the fixed-point Wetterich equation for

.. math::

   \Gamma_k[\phi]=\int d^D x\left[
      U_k(\rho)+\frac{1}{2}Z_k(\rho)(\partial_\mu\phi)^2
   \right],\qquad \rho=\phi^2/2 .

Unlike LPA-prime, ``Z_k`` is retained as a function of the field.  The flow of
``Z_k`` is obtained directly by taking the external-momentum derivative of the
two-point Wetterich flow.  The momentum derivative is evaluated at several
small nonzero momenta and extrapolated to zero.  This makes the implementation
longer than the usual closed threshold-function formula, but it also leaves a
transparent numerical audit trail for the projection.

The potential and wave-function factor are represented by Taylor polynomials
about the running minimum.  Their fixed-point equations are imposed at
Chebyshev--Lobatto collocation points.  Regulator, polynomial, momentum-
projection, and quadrature dependencies can therefore be varied independently.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from math import factorial, gamma, log, pi

import numpy as np
from numpy.polynomial.legendre import leggauss
from scipy.optimize import least_squares

from .bulk_lpa import convergence_sequence
from .surface_frg import _integrate_fixed_point, solve_global_fixed_point


@dataclass(frozen=True)
class DE2Config:
    dimension: float
    regulator_alpha: float
    regulator_family: str = "wetterich"
    potential_order: int = 6
    wavefunction_order: int = 3
    potential_rho_max: float | None = None
    wavefunction_rho_max: float | None = None
    wavefunction_normalization_rho: float = 0.0
    radial_nodes: int = 96
    angular_nodes: int = 24
    radial_cutoff: float = 35.0
    momentum_samples: tuple[float, ...] = (0.012, 0.018, 0.025, 0.034)

    def resolved_potential_rho_max(self) -> float:
        if self.potential_rho_max is not None:
            return float(self.potential_rho_max)
        return 0.30 if abs(self.dimension - 2.0) < 1.0e-12 else 0.15

    def resolved_wavefunction_rho_max(self) -> float:
        if self.wavefunction_rho_max is not None:
            return float(self.wavefunction_rho_max)
        return 0.25 if abs(self.dimension - 2.0) < 1.0e-12 else 0.13


@dataclass(frozen=True)
class DE2Solution:
    dimension: float
    regulator: str
    regulator_alpha: float
    potential_order: int
    wavefunction_order: int
    kappa: float
    potential_derivatives: tuple[float, ...]
    wavefunction_derivatives: tuple[float, ...]
    eta: float
    residual_norm: float
    function_evaluations: int
    radial_nodes: int
    angular_nodes: int
    radial_cutoff: float
    momentum_samples: tuple[float, ...]
    potential_rho_max: float
    wavefunction_rho_max: float
    minimum_inverse_propagator: float

    def to_dict(self) -> dict:
        result = asdict(self)
        result["potential_derivatives"] = list(self.potential_derivatives)
        result["wavefunction_derivatives"] = list(self.wavefunction_derivatives)
        result["momentum_samples"] = list(self.momentum_samples)
        return result

    def vector(self) -> np.ndarray:
        return np.asarray(
            [
                self.kappa,
                *self.potential_derivatives,
                *self.wavefunction_derivatives,
                self.eta,
            ],
            dtype=float,
        )


@dataclass(frozen=True)
class SpectralDE2Config:
    """Configuration for the Chebyshev global-field representation."""

    dimension: float
    regulator_alpha: float
    regulator_family: str = "wetterich"
    collocation_order: int = 10
    rho_max: float | None = None
    radial_nodes: int = 96
    angular_nodes: int = 24
    radial_cutoff: float = 35.0
    momentum_samples: tuple[float, ...] = (0.012, 0.018, 0.025, 0.034)

    def resolved_rho_max(self) -> float:
        if self.rho_max is not None:
            return float(self.rho_max)
        return 0.30 if abs(self.dimension - 2.0) < 1.0e-12 else 0.15


@dataclass(frozen=True)
class SpectralDE2Solution:
    dimension: float
    regulator: str
    regulator_alpha: float
    collocation_order: int
    rho_max: float
    rho_nodes: tuple[float, ...]
    potential_values: tuple[float, ...]
    wavefunction_values: tuple[float, ...]
    eta: float
    kappa: float
    residual_norm: float
    function_evaluations: int
    radial_nodes: int
    angular_nodes: int
    radial_cutoff: float
    momentum_samples: tuple[float, ...]
    minimum_inverse_propagator: float

    def to_dict(self) -> dict:
        result = asdict(self)
        for key in (
            "rho_nodes",
            "potential_values",
            "wavefunction_values",
            "momentum_samples",
        ):
            result[key] = list(result[key])
        return result


def _chebyshev_lobatto(count: int, maximum: float) -> np.ndarray:
    angles = np.linspace(0.0, pi, count)
    return maximum * (1.0 - np.cos(angles)) / 2.0


def _chebyshev_differentiation(
    order: int, maximum: float
) -> tuple[np.ndarray, np.ndarray]:
    """Return ascending rho nodes and the first-derivative matrix."""
    indices = np.arange(order + 1)
    cosine_nodes = np.cos(pi * indices / order)
    rho = maximum * (1.0 - cosine_nodes) / 2.0
    barycentric = (-1.0) ** indices
    barycentric[[0, -1]] *= 0.5
    difference = cosine_nodes[:, None] - cosine_nodes[None, :]
    matrix = np.zeros_like(difference)
    off_diagonal = ~np.eye(order + 1, dtype=bool)
    ratio = barycentric[None, :] / barycentric[:, None]
    matrix[off_diagonal] = ratio[off_diagonal] / difference[off_diagonal]
    matrix[np.diag_indices_from(matrix)] = -np.sum(matrix, axis=1)
    return rho, -2.0 * matrix / maximum


class _DE2Flow:
    def __init__(self, config: DE2Config):
        self.config = config
        self.dimension = float(config.dimension)
        self.alpha = float(config.regulator_alpha)
        radial_x, radial_weight = leggauss(config.radial_nodes)
        self.y = (radial_x + 1.0) * config.radial_cutoff / 2.0
        self.radial_weight = radial_weight * config.radial_cutoff / 2.0

        angular_x, angular_weight = leggauss(config.angular_nodes)
        theta = (angular_x + 1.0) * pi / 2.0
        weights = angular_weight * pi / 2.0 * np.sin(theta) ** (self.dimension - 2.0)
        self.angular_cosine = np.cos(theta)
        self.angular_weight = weights / np.sum(weights)

        self.shell_measure = (
            2.0
            * pi ** (self.dimension / 2.0)
            / gamma(self.dimension / 2.0)
            / (2.0 * pi) ** self.dimension
        )
        self.regulator, self.regulator_prime = self._regulator_shape(self.y)

        self.potential_points = _chebyshev_lobatto(
            config.potential_order + 1,
            config.resolved_potential_rho_max(),
        )
        self.wavefunction_points = _chebyshev_lobatto(
            config.wavefunction_order + 1,
            config.resolved_wavefunction_rho_max(),
        )

    def _regulator_shape(self, argument: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
        """Return the dimensionless cutoff ``b(y)=R_k/(Z_k k^2)``."""
        argument = np.asarray(argument, dtype=float)
        if self.config.regulator_family == "exponential":
            shape = self.alpha * np.exp(-argument)
            return shape, -shape
        if self.config.regulator_family != "wetterich":
            raise ValueError(
                "regulator_family must be 'wetterich' or 'exponential'"
            )
        exponential_minus_one = np.expm1(argument)
        exponential = exponential_minus_one + 1.0
        with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
            shape = self.alpha * argument / exponential_minus_one
            derivative = (
                self.alpha
                * (exponential_minus_one - argument * exponential)
                / exponential_minus_one**2
            )
        small = np.abs(argument) < 1.0e-5
        if np.any(small):
            value = argument[small]
            shape = np.array(shape, copy=True)
            derivative = np.array(derivative, copy=True)
            shape[small] = self.alpha * (
                1.0 - value / 2.0 + value**2 / 12.0 - value**4 / 720.0
            )
            derivative[small] = self.alpha * (
                -0.5 + value / 6.0 - value**3 / 180.0
            )
        return shape, derivative

    def _derivatives(
        self,
        rho: np.ndarray,
        kappa: float,
        potential_derivatives: np.ndarray,
        wavefunction_derivatives: np.ndarray,
    ) -> tuple[list[np.ndarray], list[np.ndarray]]:
        displacement = np.asarray(rho, dtype=float) - kappa
        potential: list[np.ndarray] = []
        for derivative_order in range(5):
            value = np.zeros_like(displacement)
            for power, coefficient in enumerate(potential_derivatives, start=2):
                if power >= derivative_order:
                    value += (
                        coefficient
                        * displacement ** (power - derivative_order)
                        / factorial(power - derivative_order)
                    )
            potential.append(value)

        wavefunction: list[np.ndarray] = []
        reference_displacement = (
            self.config.wavefunction_normalization_rho - kappa
        )
        for derivative_order in range(3):
            value = (
                np.ones_like(displacement)
                if derivative_order == 0
                else np.zeros_like(displacement)
            )
            for power, coefficient in enumerate(wavefunction_derivatives, start=1):
                if power >= derivative_order:
                    term = displacement ** (power - derivative_order)
                    if derivative_order == 0:
                        term = term - reference_displacement**power
                    value += coefficient * term / factorial(
                        power - derivative_order
                    )
            wavefunction.append(value)
        return potential, wavefunction

    def _loops(
        self,
        rho: np.ndarray,
        kappa: float,
        potential_derivatives: np.ndarray,
        wavefunction_derivatives: np.ndarray,
        eta: float,
    ) -> tuple[np.ndarray, np.ndarray, float]:
        rho = np.atleast_1d(np.asarray(rho, dtype=float))
        potential, wavefunction = self._derivatives(
            rho,
            kappa,
            potential_derivatives,
            wavefunction_derivatives,
        )
        return self._loops_from_evaluated(rho, potential, wavefunction, eta)

    def _loops_from_evaluated(
        self,
        rho: np.ndarray,
        potential: list[np.ndarray],
        wavefunction: list[np.ndarray],
        eta: float,
    ) -> tuple[np.ndarray, np.ndarray, float]:
        """Evaluate loop terms from functions and their field derivatives."""
        rho = np.atleast_1d(np.asarray(rho, dtype=float))
        _, first, second, third, fourth = potential
        z_value, z_first, z_second = wavefunction

        cutoff_derivative = (
            (2.0 - eta) * self.regulator - 2.0 * self.y * self.regulator_prime
        )
        field = np.sqrt(2.0 * np.maximum(rho, 0.0))
        mass = first + 2.0 * rho * second
        vertex_three = field * (3.0 * second + 2.0 * rho * third)
        vertex_four = (
            3.0 * second + 12.0 * rho * third + 4.0 * rho**2 * fourth
        )
        z_phi = field * z_first
        z_phi_phi = z_first + 2.0 * rho * z_second

        potential_loop: list[float] = []
        wavefunction_loop: list[float] = []
        minimum_denominator = np.inf
        momentum_samples = np.asarray(self.config.momentum_samples, dtype=float)

        radial_measure = (
            self.radial_weight * self.y ** (self.dimension / 2.0 - 1.0)
        )
        for index in range(len(rho)):
            denominator = (
                z_value[index] * self.y + self.regulator + mass[index]
            )
            minimum_denominator = min(minimum_denominator, float(np.min(denominator)))
            if np.min(denominator) <= 2.0e-3:
                raise FloatingPointError("The regularized inverse propagator crossed zero")
            propagator = 1.0 / denominator
            potential_loop.append(
                self.shell_measure
                / 4.0
                * float(np.sum(radial_measure * cutoff_derivative * propagator))
            )

            two_point_measure = (
                self.shell_measure
                / 2.0
                * radial_measure
                * cutoff_derivative
                * propagator**2
            )
            cubic_at_zero = vertex_three[index] + z_phi[index] * self.y
            quartic_at_zero = vertex_four[index] + z_phi_phi[index] * self.y
            two_point_zero = float(
                np.sum(
                    two_point_measure
                    * (propagator * cubic_at_zero**2 - 0.5 * quartic_at_zero)
                )
            )

            projected_slopes: list[float] = []
            for momentum in momentum_samples:
                momentum_squared = momentum**2
                shifted_y = (
                    self.y[:, None]
                    + momentum_squared
                    + 2.0
                    * np.sqrt(self.y)[:, None]
                    * momentum
                    * self.angular_cosine[None, :]
                )
                shifted_regulator, _ = self._regulator_shape(shifted_y)
                shifted_denominator = (
                    z_value[index] * shifted_y
                    + shifted_regulator
                    + mass[index]
                )
                minimum_denominator = min(
                    minimum_denominator,
                    float(np.min(shifted_denominator)),
                )
                if np.min(shifted_denominator) <= 2.0e-3:
                    raise FloatingPointError(
                        "The shifted regularized inverse propagator crossed zero"
                    )
                shifted_propagator = 1.0 / shifted_denominator
                cubic = vertex_three[index] + 0.5 * z_phi[index] * (
                    momentum_squared + self.y[:, None] + shifted_y
                )
                quartic = vertex_four[index] + z_phi_phi[index] * (
                    momentum_squared + self.y[:, None]
                )
                angular_average = np.sum(
                    self.angular_weight[None, :]
                    * (shifted_propagator * cubic**2 - 0.5 * quartic),
                    axis=1,
                )
                two_point_at_momentum = float(
                    np.sum(two_point_measure * angular_average)
                )
                projected_slopes.append(
                    (two_point_at_momentum - two_point_zero) / momentum_squared
                )

            extrapolation = np.polyfit(
                momentum_samples**2,
                np.asarray(projected_slopes),
                deg=min(2, len(momentum_samples) - 1),
            )
            wavefunction_loop.append(float(extrapolation[-1]))

        return (
            np.asarray(potential_loop),
            np.asarray(wavefunction_loop),
            minimum_denominator,
        )

    def unpack(
        self, values: np.ndarray
    ) -> tuple[float, np.ndarray, np.ndarray, float]:
        potential_stop = self.config.potential_order
        wavefunction_stop = potential_stop + self.config.wavefunction_order
        return (
            float(values[0]),
            np.asarray(values[1:potential_stop]),
            np.asarray(values[potential_stop:wavefunction_stop]),
            float(values[-1]),
        )

    def residual(self, values: np.ndarray) -> np.ndarray:
        kappa, potential_derivatives, wavefunction_derivatives, eta = self.unpack(
            values
        )
        size = (
            self.config.potential_order
            + self.config.wavefunction_order
            + 1
        )
        if (
            kappa <= 1.0e-4
            or kappa > 1.5 * self.config.resolved_potential_rho_max()
            or eta < 0.0
            or eta > 0.6
        ):
            return np.full(size, 1.0e5)

        try:
            potential_loop, _, _ = self._loops(
                self.potential_points,
                kappa,
                potential_derivatives,
                wavefunction_derivatives,
                eta,
            )
            potential, _ = self._derivatives(
                self.potential_points,
                kappa,
                potential_derivatives,
                wavefunction_derivatives,
            )
            potential_flow = (
                -self.dimension * potential[0]
                + (self.dimension - 2.0 + eta)
                * self.potential_points
                * potential[1]
                + potential_loop
            )

            _, wavefunction_loop, _ = self._loops(
                self.wavefunction_points,
                kappa,
                potential_derivatives,
                wavefunction_derivatives,
                eta,
            )
            _, wavefunction = self._derivatives(
                self.wavefunction_points,
                kappa,
                potential_derivatives,
                wavefunction_derivatives,
            )
            wavefunction_flow = (
                eta * wavefunction[0]
                + (self.dimension - 2.0 + eta)
                * self.wavefunction_points
                * wavefunction[1]
                + wavefunction_loop
            )
        except (FloatingPointError, OverflowError):
            return np.full(size, 1.0e5)

        potential_scale = 10.0 if self.dimension < 2.5 else 30.0
        result = np.concatenate(
            (
                potential_scale * (potential_flow[1:] - potential_flow[0]),
                wavefunction_flow,
            )
        )
        if result.shape != (size,) or not np.all(np.isfinite(result)):
            return np.full(size, 1.0e5)
        return result

    def minimum_inverse_propagator(self, values: np.ndarray) -> float:
        kappa, potential_derivatives, wavefunction_derivatives, eta = self.unpack(
            values
        )
        all_points = np.unique(
            np.concatenate((self.potential_points, self.wavefunction_points))
        )
        return self._loops(
            all_points,
            kappa,
            potential_derivatives,
            wavefunction_derivatives,
            eta,
        )[2]


def _initial_potential(config: DE2Config) -> tuple[float, np.ndarray, float]:
    dimension = float(config.dimension)
    order = config.potential_order
    if abs(dimension - 3.0) < 1.0e-12:
        lpa_solution = convergence_sequence(
            dimension=3.0,
            minimum_order=2,
            maximum_order=order,
        )[-1]
        return (
            lpa_solution.kappa,
            np.asarray(lpa_solution.lambdas, dtype=float),
            lpa_solution.eta,
        )

    if abs(dimension - 2.0) < 1.0e-12:
        fixed_point = solve_global_fixed_point(2.0)
        dense_solution = _integrate_fixed_point(
            log(fixed_point.asymptotic_amplitude),
            2.0,
            fixed_point.eta,
            fixed_point.rho_large,
            fixed_point.rho_epsilon,
            dense_output=True,
        )
        if dense_solution.sol is None:
            raise RuntimeError("Could not construct the D=2 LPA-prime initial guess")
        rho = np.linspace(
            fixed_point.rho_epsilon,
            config.resolved_potential_rho_max(),
            600,
        )
        potential = dense_solution.sol(rho)[0]
        coefficients = np.polynomial.polynomial.polyfit(
            rho - fixed_point.kappa,
            potential,
            order,
        )
        derivatives = np.asarray(
            [coefficients[power] * factorial(power) for power in range(2, order + 1)]
        )
        return fixed_point.kappa, derivatives, fixed_point.eta

    raise ValueError("The audited DE2 solver currently supports D=2 or D=3")


def solve_de2_fixed_point(
    config: DE2Config,
    initial: DE2Solution | np.ndarray | None = None,
) -> DE2Solution:
    """Solve one field-dependent ``O(partial^2)`` fixed point."""
    flow = _DE2Flow(config)
    expected_size = config.potential_order + config.wavefunction_order + 1
    if isinstance(initial, DE2Solution):
        values = initial.vector()
    elif initial is not None:
        values = np.asarray(initial, dtype=float)
    else:
        kappa, potential, eta = _initial_potential(config)
        values = np.asarray(
            [
                kappa,
                *potential,
                *np.zeros(config.wavefunction_order),
                eta,
            ],
            dtype=float,
        )
    if values.shape != (expected_size,):
        raise ValueError(
            f"Initial vector has shape {values.shape}; expected {(expected_size,)}"
        )

    result = least_squares(
        flow.residual,
        values,
        xtol=2.0e-10,
        ftol=2.0e-10,
        gtol=2.0e-10,
        max_nfev=3000,
        x_scale="jac",
    )
    residual_norm = float(np.linalg.norm(result.fun))
    if not result.success or residual_norm > 2.0e-7:
        raise RuntimeError(
            "DE2 fixed-point solve failed: "
            f"success={result.success}, residual={residual_norm:.3e}"
        )

    kappa, potential, wavefunction, eta = flow.unpack(result.x)
    return DE2Solution(
        dimension=float(config.dimension),
        regulator=config.regulator_family,
        regulator_alpha=float(config.regulator_alpha),
        potential_order=config.potential_order,
        wavefunction_order=config.wavefunction_order,
        kappa=kappa,
        potential_derivatives=tuple(float(value) for value in potential),
        wavefunction_derivatives=tuple(float(value) for value in wavefunction),
        eta=eta,
        residual_norm=residual_norm,
        function_evaluations=int(result.nfev),
        radial_nodes=config.radial_nodes,
        angular_nodes=config.angular_nodes,
        radial_cutoff=config.radial_cutoff,
        momentum_samples=config.momentum_samples,
        potential_rho_max=config.resolved_potential_rho_max(),
        wavefunction_rho_max=config.resolved_wavefunction_rho_max(),
        minimum_inverse_propagator=flow.minimum_inverse_propagator(result.x),
    )


class _SpectralDE2Flow:
    def __init__(self, config: SpectralDE2Config):
        self.config = config
        rho_max = config.resolved_rho_max()
        self.rho, first_matrix = _chebyshev_differentiation(
            config.collocation_order,
            rho_max,
        )
        self.derivative_matrices = [np.eye(config.collocation_order + 1)]
        for _ in range(4):
            self.derivative_matrices.append(
                first_matrix @ self.derivative_matrices[-1]
            )
        loop_config = DE2Config(
            dimension=config.dimension,
            regulator_alpha=config.regulator_alpha,
            regulator_family=config.regulator_family,
            radial_nodes=config.radial_nodes,
            angular_nodes=config.angular_nodes,
            radial_cutoff=config.radial_cutoff,
            momentum_samples=config.momentum_samples,
            potential_rho_max=rho_max,
            wavefunction_rho_max=rho_max,
        )
        self.loop_flow = _DE2Flow(loop_config)

    def unpack(self, values: np.ndarray) -> tuple[np.ndarray, np.ndarray, float]:
        order = self.config.collocation_order
        potential = np.concatenate(([0.0], np.asarray(values[:order])))
        wavefunction = np.concatenate(
            ([1.0], np.asarray(values[order : 2 * order]))
        )
        return potential, wavefunction, float(values[-1])

    def evaluated_functions(
        self, values: np.ndarray
    ) -> tuple[list[np.ndarray], list[np.ndarray], float]:
        potential, wavefunction, eta = self.unpack(values)
        potential_derivatives = [
            matrix @ potential for matrix in self.derivative_matrices
        ]
        wavefunction_derivatives = [
            self.derivative_matrices[order] @ wavefunction
            for order in range(3)
        ]
        return potential_derivatives, wavefunction_derivatives, eta

    def residual(self, values: np.ndarray) -> np.ndarray:
        potential, wavefunction, eta = self.evaluated_functions(values)
        size = 2 * self.config.collocation_order + 1
        if (
            eta < 0.0
            or eta > 0.6
            or np.min(wavefunction[0]) <= 0.05
            or not np.all(np.isfinite(values))
        ):
            return np.full(size, 1.0e5)
        try:
            potential_loop, wavefunction_loop, _ = (
                self.loop_flow._loops_from_evaluated(
                    self.rho,
                    potential,
                    wavefunction,
                    eta,
                )
            )
        except (FloatingPointError, OverflowError):
            return np.full(size, 1.0e5)

        dimension = self.config.dimension
        potential_flow = (
            -dimension * potential[0]
            + (dimension - 2.0 + eta) * self.rho * potential[1]
            + potential_loop
        )
        wavefunction_flow = (
            eta * wavefunction[0]
            + (dimension - 2.0 + eta) * self.rho * wavefunction[1]
            + wavefunction_loop
        )
        potential_scale = 10.0 if dimension < 2.5 else 30.0
        result = np.concatenate(
            (
                potential_scale * (potential_flow[1:] - potential_flow[0]),
                wavefunction_flow,
            )
        )
        if result.shape != (size,) or not np.all(np.isfinite(result)):
            return np.full(size, 1.0e5)
        return result

    def minimum_inverse_propagator(self, values: np.ndarray) -> float:
        potential, wavefunction, eta = self.evaluated_functions(values)
        return self.loop_flow._loops_from_evaluated(
            self.rho,
            potential,
            wavefunction,
            eta,
        )[2]

    def kappa(self, values: np.ndarray) -> float:
        potential, _, _ = self.evaluated_functions(values)
        first = potential[1]
        crossings = np.flatnonzero((first[:-1] <= 0.0) & (first[1:] > 0.0))
        if len(crossings) != 1:
            return float("nan")
        index = int(crossings[0])
        left, right = self.rho[index : index + 2]
        first_left, first_right = first[index : index + 2]
        return float(
            left
            - first_left * (right - left) / (first_right - first_left)
        )


def _spectral_initial_vector(
    flow: _SpectralDE2Flow,
    initial: SpectralDE2Solution | np.ndarray | None,
) -> np.ndarray:
    order = flow.config.collocation_order
    if isinstance(initial, SpectralDE2Solution):
        old_rho = np.asarray(initial.rho_nodes)
        potential = np.interp(
            flow.rho,
            old_rho,
            np.asarray(initial.potential_values),
        )
        wavefunction = np.interp(
            flow.rho,
            old_rho,
            np.asarray(initial.wavefunction_values),
        )
        return np.concatenate(
            (potential[1:], wavefunction[1:], [initial.eta])
        )
    if initial is not None:
        values = np.asarray(initial, dtype=float)
        expected = 2 * order + 1
        if values.shape != (expected,):
            raise ValueError(
                f"Initial vector has shape {values.shape}; expected {(expected,)}"
            )
        return values

    polynomial_config = DE2Config(
        dimension=flow.config.dimension,
        regulator_alpha=flow.config.regulator_alpha,
        regulator_family=flow.config.regulator_family,
        potential_order=6,
        wavefunction_order=3,
        potential_rho_max=flow.config.resolved_rho_max(),
        wavefunction_rho_max=min(
            flow.config.resolved_rho_max(),
            0.25 if flow.config.dimension < 2.5 else 0.13,
        ),
        radial_nodes=flow.config.radial_nodes,
        angular_nodes=flow.config.angular_nodes,
        radial_cutoff=flow.config.radial_cutoff,
        momentum_samples=flow.config.momentum_samples,
    )
    polynomial = solve_de2_fixed_point(polynomial_config)
    potential, wavefunction = flow.loop_flow._derivatives(
        flow.rho,
        polynomial.kappa,
        np.asarray(polynomial.potential_derivatives),
        np.asarray(polynomial.wavefunction_derivatives),
    )
    potential_values = potential[0] - potential[0][0]
    return np.concatenate(
        (potential_values[1:], wavefunction[0][1:], [polynomial.eta])
    )


def solve_spectral_de2_fixed_point(
    config: SpectralDE2Config,
    initial: SpectralDE2Solution | np.ndarray | None = None,
) -> SpectralDE2Solution:
    """Solve the global field-dependent ``O(partial^2)`` fixed point."""
    flow = _SpectralDE2Flow(config)
    initial_values = _spectral_initial_vector(flow, initial)
    result = least_squares(
        flow.residual,
        initial_values,
        xtol=2.0e-10,
        ftol=2.0e-10,
        gtol=2.0e-10,
        max_nfev=5000,
        x_scale="jac",
    )
    residual_norm = float(np.linalg.norm(result.fun))
    if not result.success or residual_norm > 5.0e-7:
        raise RuntimeError(
            "Spectral DE2 fixed-point solve failed: "
            f"success={result.success}, residual={residual_norm:.3e}"
        )
    potential, wavefunction, eta = flow.unpack(result.x)
    kappa = flow.kappa(result.x)
    if not np.isfinite(kappa):
        raise RuntimeError("Spectral DE2 solution did not have one physical minimum")
    return SpectralDE2Solution(
        dimension=float(config.dimension),
        regulator=config.regulator_family,
        regulator_alpha=float(config.regulator_alpha),
        collocation_order=config.collocation_order,
        rho_max=config.resolved_rho_max(),
        rho_nodes=tuple(float(value) for value in flow.rho),
        potential_values=tuple(float(value) for value in potential),
        wavefunction_values=tuple(float(value) for value in wavefunction),
        eta=eta,
        kappa=kappa,
        residual_norm=residual_norm,
        function_evaluations=int(result.nfev),
        radial_nodes=config.radial_nodes,
        angular_nodes=config.angular_nodes,
        radial_cutoff=config.radial_cutoff,
        momentum_samples=config.momentum_samples,
        minimum_inverse_propagator=flow.minimum_inverse_propagator(result.x),
    )


def spectral_regulator_scan(
    dimension: float,
    alphas: tuple[float, ...],
    *,
    regulator_family: str = "wetterich",
    collocation_order: int = 10,
    initial_alpha: float | None = None,
    initial_solution: SpectralDE2Solution | None = None,
    **config_overrides,
) -> list[SpectralDE2Solution]:
    """Run a global DE2 regulator scan using fixed-order continuation."""
    unique_alphas = sorted({float(value) for value in alphas})
    if not unique_alphas:
        return []
    if initial_alpha is None:
        if regulator_family == "wetterich":
            initial_alpha = 2.0 if abs(dimension - 2.0) < 1.0e-12 else 2.6
        else:
            initial_alpha = 1.0
    center = min(unique_alphas, key=lambda value: abs(value - initial_alpha))

    def configuration(alpha: float, order: int) -> SpectralDE2Config:
        return SpectralDE2Config(
            dimension=dimension,
            regulator_alpha=alpha,
            regulator_family=regulator_family,
            collocation_order=order,
            **config_overrides,
        )

    seed_order = min(collocation_order, 8)
    center_solution = solve_spectral_de2_fixed_point(
        configuration(center, seed_order),
        initial_solution,
    )
    for order in range(seed_order + 2, collocation_order + 1, 2):
        center_solution = solve_spectral_de2_fixed_point(
            configuration(center, order),
            center_solution,
        )
    if center_solution.collocation_order != collocation_order:
        center_solution = solve_spectral_de2_fixed_point(
            configuration(center, collocation_order),
            center_solution,
        )

    solutions: dict[float, SpectralDE2Solution] = {center: center_solution}
    previous = center_solution
    for alpha in sorted(value for value in unique_alphas if value > center):
        previous = solve_spectral_de2_fixed_point(
            configuration(alpha, collocation_order),
            previous,
        )
        solutions[alpha] = previous
    previous = center_solution
    for alpha in sorted(
        (value for value in unique_alphas if value < center),
        reverse=True,
    ):
        previous = solve_spectral_de2_fixed_point(
            configuration(alpha, collocation_order),
            previous,
        )
        solutions[alpha] = previous
    return [solutions[alpha] for alpha in unique_alphas]


def quadratic_pms(
    solutions: list[SpectralDE2Solution],
    *,
    neighborhood: int = 5,
) -> dict[str, float | list[float]]:
    """Locate a local minimum of eta(alpha) by a quadratic PMS fit."""
    if len(solutions) < 3:
        raise ValueError("At least three regulator points are required for PMS")
    ordered = sorted(solutions, key=lambda solution: solution.regulator_alpha)
    alpha = np.asarray([solution.regulator_alpha for solution in ordered])
    eta = np.asarray([solution.eta for solution in ordered])
    minimum = int(np.argmin(eta))
    half_width = max(1, neighborhood // 2)
    left = max(0, minimum - half_width)
    right = min(len(ordered), left + neighborhood)
    left = max(0, right - neighborhood)
    if right - left < 3:
        raise ValueError("The PMS minimum is not bracketed by the scan")
    coefficients = np.polyfit(alpha[left:right], eta[left:right], 2)
    curvature, linear, _constant = coefficients
    if curvature <= 0.0:
        raise RuntimeError("The fitted PMS stationary point is not a minimum")
    optimum_alpha = -linear / (2.0 * curvature)
    if not alpha[left] <= optimum_alpha <= alpha[right - 1]:
        raise RuntimeError("The fitted PMS point lies outside its local scan window")
    optimum_eta = float(np.polyval(coefficients, optimum_alpha))
    fitted = np.polyval(coefficients, alpha[left:right])
    fit_residual = float(np.max(np.abs(fitted - eta[left:right])))
    return {
        "alpha": float(optimum_alpha),
        "eta": optimum_eta,
        "curvature": float(curvature),
        "local_fit_residual": fit_residual,
        "fit_alpha": [float(value) for value in alpha[left:right]],
        "fit_eta": [float(value) for value in eta[left:right]],
    }


def regulator_scan(
    dimension: float,
    alphas: tuple[float, ...],
    *,
    potential_order: int = 6,
    wavefunction_order: int = 3,
    initial_alpha: float | None = None,
    **config_overrides,
) -> list[DE2Solution]:
    """Solve a regulator scan by continuation away from a central alpha."""
    unique_alphas = sorted({float(value) for value in alphas})
    if not unique_alphas:
        return []
    if initial_alpha is None:
        initial_alpha = 1.0 if abs(dimension - 2.0) < 1.0e-12 else 2.0
    center = min(unique_alphas, key=lambda value: abs(value - initial_alpha))

    solutions: dict[float, DE2Solution] = {}
    center_config = DE2Config(
        dimension=dimension,
        regulator_alpha=center,
        potential_order=potential_order,
        wavefunction_order=wavefunction_order,
        **config_overrides,
    )
    solutions[center] = solve_de2_fixed_point(center_config)

    previous = solutions[center]
    for alpha in sorted(value for value in unique_alphas if value > center):
        config = DE2Config(
            dimension=dimension,
            regulator_alpha=alpha,
            potential_order=potential_order,
            wavefunction_order=wavefunction_order,
            **config_overrides,
        )
        previous = solve_de2_fixed_point(config, initial=previous)
        solutions[alpha] = previous

    previous = solutions[center]
    for alpha in sorted(
        (value for value in unique_alphas if value < center),
        reverse=True,
    ):
        config = DE2Config(
            dimension=dimension,
            regulator_alpha=alpha,
            potential_order=potential_order,
            wavefunction_order=wavefunction_order,
            **config_overrides,
        )
        previous = solve_de2_fixed_point(config, initial=previous)
        solutions[alpha] = previous

    return [solutions[alpha] for alpha in unique_alphas]
