r"""Complete scalar :math:`\mathcal O(\partial^4)` NPRG derivative expansion.

This module implements the five-function ansatz

.. math::

   \Gamma_k[\phi] = \int d^D x \left\{
      U_k(\rho)
      + \frac{1}{2} Z_k(\rho) (\partial_\mu\phi)^2
      + \frac{1}{2} W_{a,k}(\rho)
        (\partial_\mu\partial_\nu\phi)^2
      + \frac{1}{2}\phi W_{b,k}(\rho)
        (\partial^2\phi)(\partial_\mu\phi)^2
      + \frac{1}{2} W_{c,k}(\rho)
        [(\partial_\mu\phi)^2]^2
   \right\},\qquad \rho=\phi^2/2 .

For a single Ising field these are all independent operators through four
derivatives.  The flow is generated from the exact Wetterich vertex hierarchy,
not from hard-coded beta functions.  Ordered set partitions generate every
one-loop topology contributing to the flow of an ``n``-point vertex.  Momentum
Taylor coefficients of the two-, three-, and four-point flows project,
respectively, ``Z/Wa``, ``Wb``, and ``Wc``.

The implementation deliberately keeps the projection machinery separate from
the nonlinear fixed-point solver.  This permits three stringent audits:

* setting ``Wa=Wb=Wc=0`` reproduces the field-dependent DE2 vertex flow;
* analytic tree-level momentum projectors recover all five input functions;
* radial, angular, external-momentum, field, and regulator dependencies can be
  varied independently.

The primary convention is the strict momentum-polynomial DE used in Balog
et al. and De Polsi et al.: products of vertices are graded by their total
number of derivatives and terms above the declared DE order are removed.
The older standard convention, which retains such incomplete higher-order
products, remains available as a truncation diagnostic.
"""

from __future__ import annotations

from collections.abc import Callable, Iterable
from dataclasses import asdict, dataclass, replace
from functools import cache
from itertools import permutations
from math import gamma, pi

import numpy as np
from numpy.polynomial.legendre import leggauss
from scipy.interpolate import BarycentricInterpolator
from scipy.optimize import least_squares, root

from .bulk_de2 import (
    SpectralDE2Config,
    SpectralDE2Solution,
    _chebyshev_differentiation,
    solve_spectral_de2_fixed_point,
)


@dataclass(frozen=True)
class DE4Config:
    """Numerical and truncation parameters for one scalar DE4 fixed point."""

    dimension: int
    regulator_alpha: float
    regulator_family: str = "exponential"
    collocation_order: int = 6
    rho_max: float | None = None
    radial_nodes: int = 28
    radial_cutoff: float = 24.0
    polar_nodes: int = 10
    azimuthal_nodes: int = 12
    planar_angle_nodes: int = 48
    momentum_samples: tuple[float, ...] = (
        0.050,
        0.070,
        0.095,
        0.125,
        0.160,
        0.205,
    )
    momentum_fit_degree: int = 4
    high_derivative_feedback: float = 1.0
    strict_vertex_products: bool = True
    strict_product_order: int = 4

    def resolved_rho_max(self) -> float:
        if self.rho_max is not None:
            return float(self.rho_max)
        return 0.30 if self.dimension == 2 else 0.15

    def validate(self) -> None:
        if self.dimension not in (2, 3):
            raise ValueError("The DE4 implementation supports integer D=2 or D=3")
        if self.collocation_order < 6:
            raise ValueError(
                "collocation_order must be at least six because Gamma^(4) "
                "contains the sixth field derivative of U"
            )
        if self.regulator_family not in (
            "exponential",
            "wetterich",
            "theta3",
        ):
            raise ValueError(
                "regulator_family must be 'exponential', 'wetterich', "
                "or 'theta3'"
            )
        if not 0.0 <= self.high_derivative_feedback <= 1.0:
            raise ValueError("high_derivative_feedback must lie in [0, 1]")
        if self.strict_product_order not in (2, 4):
            raise ValueError("strict_product_order must be two or four")
        if len(self.momentum_samples) < self.momentum_fit_degree + 1:
            raise ValueError(
                "momentum_samples must contain at least fit_degree + 1 values"
            )
        if min(self.momentum_samples) <= 0.0:
            raise ValueError("All momentum samples must be positive")


@dataclass(frozen=True)
class DE4Solution:
    """One global field-dependent DE4 fixed-point solution."""

    dimension: int
    regulator: str
    regulator_alpha: float
    collocation_order: int
    rho_max: float
    rho_nodes: tuple[float, ...]
    potential_values: tuple[float, ...]
    wavefunction_values: tuple[float, ...]
    wa_values: tuple[float, ...]
    wb_values: tuple[float, ...]
    wc_values: tuple[float, ...]
    eta: float
    kappa: float
    residual_norm: float
    function_evaluations: int
    radial_nodes: int
    polar_nodes: int
    azimuthal_nodes: int
    planar_angle_nodes: int
    radial_cutoff: float
    momentum_samples: tuple[float, ...]
    momentum_fit_degree: int
    minimum_inverse_propagator: float
    high_derivative_feedback: float
    strict_vertex_products: bool = True
    strict_product_order: int = 4

    def to_dict(self) -> dict:
        result = asdict(self)
        for key in (
            "rho_nodes",
            "potential_values",
            "wavefunction_values",
            "wa_values",
            "wb_values",
            "wc_values",
            "momentum_samples",
        ):
            result[key] = list(result[key])
        return result

    @classmethod
    def from_dict(cls, payload: dict) -> DE4Solution:
        """Reconstruct a checkpoint written by :meth:`to_dict`."""

        converted = dict(payload)
        converted.setdefault("strict_vertex_products", False)
        converted.setdefault("strict_product_order", 4)
        for key in (
            "rho_nodes",
            "potential_values",
            "wavefunction_values",
            "wa_values",
            "wb_values",
            "wc_values",
            "momentum_samples",
        ):
            converted[key] = tuple(converted[key])
        return cls(**converted)


@dataclass(frozen=True)
class DE4ProjectionAudit:
    """Re-evaluation of a fixed point with an independent numerical setup."""

    source_eta: float
    projected_eta: float
    eta_shift: float
    residual_norm: float
    minimum_inverse_propagator: float
    configuration: dict

    def to_dict(self) -> dict:
        return asdict(self)


@dataclass(frozen=True)
class _FieldJets:
    """Field derivatives with respect to the dimensionless scalar field."""

    potential: tuple[float, ...]
    wavefunction: tuple[float, ...]
    wa: tuple[float, ...]
    b: tuple[float, ...]
    wc: tuple[float, ...]


def _set_partitions(items: tuple[int, ...]) -> Iterable[tuple[tuple[int, ...], ...]]:
    """Yield each unordered set partition once in canonical block order."""

    if not items:
        yield ()
        return
    first = items[0]
    for partition in _set_partitions(items[1:]):
        yield ((first,), *partition)
        for index in range(len(partition)):
            augmented = list(partition)
            augmented[index] = tuple(sorted((first, *augmented[index])))
            yield tuple(augmented)


@cache
def ordered_set_partitions(
    count: int,
) -> tuple[tuple[tuple[int, ...], ...], ...]:
    """Return ordered nonempty set partitions of ``range(count)``."""

    unique: set[tuple[tuple[int, ...], ...]] = set()
    for partition in _set_partitions(tuple(range(count))):
        for ordered in permutations(partition):
            unique.add(tuple(ordered))
    return tuple(
        sorted(
            unique,
            key=lambda partition: (
                len(partition),
                tuple(len(block) for block in partition),
                partition,
            ),
        )
    )


def _as_batch(momentum: np.ndarray, count: int) -> np.ndarray:
    momentum = np.asarray(momentum, dtype=float)
    if momentum.ndim == 1:
        return np.broadcast_to(momentum, (count, momentum.size))
    if momentum.shape[0] != count:
        raise ValueError("Momentum batch has an inconsistent leading dimension")
    return momentum


def _dot(left: np.ndarray, right: np.ndarray) -> np.ndarray:
    return np.einsum("ij,ij->i", left, right, optimize=True)


def _vertex_basis(momentums: tuple[np.ndarray, ...]) -> dict[str, np.ndarray]:
    """Return momentum polynomials multiplying the five field functions."""

    count = momentums[0].shape[0]
    leg_count = len(momentums)
    pair_dots: dict[tuple[int, int], np.ndarray] = {}
    for left in range(leg_count):
        for right in range(left + 1, leg_count):
            pair_dots[(left, right)] = _dot(
                momentums[left],
                momentums[right],
            )

    z_basis = np.zeros(count)
    wa_basis = np.zeros(count)
    for value in pair_dots.values():
        z_basis -= value
        wa_basis += value**2

    b_basis = np.zeros(count)
    if leg_count >= 3:
        squared = [_dot(momentum, momentum) for momentum in momentums]
        for laplacian_leg in range(leg_count):
            remaining = [
                index for index in range(leg_count) if index != laplacian_leg
            ]
            for left_position, left in enumerate(remaining):
                for right in remaining[left_position + 1 :]:
                    key = (min(left, right), max(left, right))
                    b_basis += squared[laplacian_leg] * pair_dots[key]

    wc_basis = np.zeros(count)
    if leg_count >= 4:
        for first in range(leg_count):
            for second in range(first + 1, leg_count):
                remaining_after_first_pair = [
                    index
                    for index in range(leg_count)
                    if index not in (first, second)
                ]
                for third_position, third in enumerate(
                    remaining_after_first_pair
                ):
                    for fourth in remaining_after_first_pair[
                        third_position + 1 :
                    ]:
                        selected = tuple(sorted((first, second, third, fourth)))
                        if len(set(selected)) != 4:
                            continue
                        # Every four-leg subset has three pairings.  Requiring
                        # the first leg of the subset in the first pair counts
                        # each pairing once.
                        if first != selected[0]:
                            continue
                        first_key = (first, second)
                        second_key = (min(third, fourth), max(third, fourth))
                        wc_basis += (
                            4.0
                            * pair_dots[first_key]
                            * pair_dots[second_key]
                        )

    return {
        "z": z_basis,
        "wa": wa_basis,
        "b": b_basis,
        "wc": wc_basis,
    }


def vertex_from_jets(
    momentums: tuple[np.ndarray, ...],
    jets: _FieldJets,
) -> np.ndarray:
    """Evaluate a uniform-background vertex from field derivatives."""

    count = momentums[0].shape[0]
    leg_count = len(momentums)
    basis = _vertex_basis(momentums)
    result = np.full(count, jets.potential[leg_count], dtype=float)
    if leg_count >= 2:
        result += jets.wavefunction[leg_count - 2] * basis["z"]
        result += jets.wa[leg_count - 2] * basis["wa"]
    if leg_count >= 3:
        result += jets.b[leg_count - 3] * basis["b"]
    if leg_count >= 4:
        result += jets.wc[leg_count - 4] * basis["wc"]
    return result


def _rho_to_phi_derivatives(
    rho: np.ndarray,
    rho_derivatives: list[np.ndarray],
    maximum_order: int,
) -> list[np.ndarray]:
    """Convert derivatives of ``F(rho)`` to derivatives of ``F(phi)``."""

    rho = np.asarray(rho, dtype=float)
    field = np.sqrt(2.0 * np.maximum(rho, 0.0))
    terms: dict[tuple[int, int], float] = {(0, 0): 1.0}
    result = [np.asarray(rho_derivatives[0], dtype=float)]
    for _ in range(maximum_order):
        differentiated: dict[tuple[int, int], float] = {}
        for (field_power, rho_order), coefficient in terms.items():
            if field_power:
                key = (field_power - 1, rho_order)
                differentiated[key] = differentiated.get(key, 0.0) + (
                    coefficient * field_power
                )
            key = (field_power + 1, rho_order + 1)
            differentiated[key] = differentiated.get(key, 0.0) + coefficient
        terms = differentiated
        value = np.zeros_like(rho)
        for (field_power, rho_order), coefficient in terms.items():
            value += (
                coefficient
                * field**field_power
                * rho_derivatives[rho_order]
            )
        result.append(value)
    return result


class _MomentumGeometry:
    """Precomputed momentum routing and vertex bases for one projection."""

    def __init__(
        self,
        external: tuple[np.ndarray, ...],
        loop_momenta: np.ndarray,
        regulator_values,
    ):
        self.external = tuple(np.asarray(value, dtype=float) for value in external)
        self.count = len(self.external)
        self.loop_momenta = np.asarray(loop_momenta, dtype=float)
        self.batch_size = self.loop_momenta.shape[0]
        self.partitions = ordered_set_partitions(self.count)
        subset_count = 1 << self.count
        self.momentum_by_subset: list[np.ndarray] = []
        self.y_by_subset: list[np.ndarray] = []
        self.regulator_by_subset: list[np.ndarray] = []
        for mask in range(subset_count):
            shift = np.zeros(self.loop_momenta.shape[1])
            for index, momentum in enumerate(self.external):
                if mask & (1 << index):
                    shift += momentum
            shifted = self.loop_momenta + shift[None, :]
            y = _dot(shifted, shifted)
            self.momentum_by_subset.append(shifted)
            self.y_by_subset.append(y)
            self.regulator_by_subset.append(regulator_values(y))

        self.transition_basis: dict[
            tuple[int, tuple[int, ...]], dict[str, np.ndarray]
        ] = {}
        for mask in range(subset_count):
            for block_mask in range(1, subset_count):
                if mask & block_mask:
                    continue
                block = tuple(
                    index
                    for index in range(self.count)
                    if block_mask & (1 << index)
                )
                next_mask = mask | block_mask
                momentums = (
                    self.momentum_by_subset[mask],
                    *(
                        _as_batch(self.external[index], self.batch_size)
                        for index in block
                    ),
                    -self.momentum_by_subset[next_mask],
                )
                self.transition_basis[(mask, block)] = _vertex_basis(momentums)

    def flow(
        self,
        jets: _FieldJets,
        mass: float,
        wavefunction: float,
        wa: float,
        cutoff_derivative: np.ndarray,
        integration_weights: np.ndarray,
        *,
        strict_vertex_products: bool,
        strict_product_order: int,
    ) -> tuple[float, float]:
        """Evaluate the Wetterich flow of one external vertex."""

        propagators: list[np.ndarray] = []
        minimum_denominator = np.inf
        for y, regulator in zip(
            self.y_by_subset,
            self.regulator_by_subset,
            strict=True,
        ):
            denominator = mass + wavefunction * y + wa * y**2 + regulator
            minimum_denominator = min(
                minimum_denominator,
                float(np.min(denominator)),
            )
            if np.min(denominator) <= 1.0e-5:
                raise FloatingPointError(
                    "The DE4 regularized inverse propagator crossed zero"
                )
            propagators.append(1.0 / denominator)

        total = np.zeros(self.batch_size)
        for partition in self.partitions:
            if strict_vertex_products:
                maximum_order = strict_product_order // 2
                term_by_order = [
                    propagators[0].copy(),
                    np.zeros(self.batch_size),
                    np.zeros(self.batch_size),
                ]
            else:
                term = propagators[0].copy()
            mask = 0
            for block in partition:
                block_mask = sum(1 << index for index in block)
                next_mask = mask | block_mask
                leg_count = len(block) + 2
                basis = self.transition_basis[(mask, block)]
                vertex_zero = np.full(
                    self.batch_size,
                    jets.potential[leg_count],
                    dtype=float,
                )
                vertex_two = (
                    jets.wavefunction[leg_count - 2] * basis["z"]
                )
                vertex_four = jets.wa[leg_count - 2] * basis["wa"]
                if leg_count >= 3:
                    vertex_four = (
                        vertex_four
                        + jets.b[leg_count - 3] * basis["b"]
                    )
                if leg_count >= 4:
                    vertex_four = (
                        vertex_four
                        + jets.wc[leg_count - 4] * basis["wc"]
                    )
                if strict_vertex_products:
                    updated = [
                        np.zeros(self.batch_size),
                        np.zeros(self.batch_size),
                        np.zeros(self.batch_size),
                    ]
                    vertices = (vertex_zero, vertex_two, vertex_four)
                    for left_order, left_value in enumerate(term_by_order):
                        for right_order, right_value in enumerate(vertices):
                            combined_order = left_order + right_order
                            if combined_order <= maximum_order:
                                updated[combined_order] += (
                                    left_value * right_value
                                )
                    term_by_order = [
                        value * propagators[next_mask] for value in updated
                    ]
                else:
                    vertex = vertex_zero + vertex_two + vertex_four
                    term *= vertex * propagators[next_mask]
                mask = next_mask
            if strict_vertex_products:
                term = sum(term_by_order)
            total += (-1.0) ** len(partition) * term

        value = 0.5 * float(
            np.sum(integration_weights * cutoff_derivative * total)
        )
        return value, minimum_denominator


class DE4VertexFlow:
    """Wetterich-loop evaluator and momentum projectors at fixed field."""

    def __init__(self, config: DE4Config):
        config.validate()
        self.config = config
        self.dimension = config.dimension
        self.alpha = float(config.regulator_alpha)
        (
            self.loop_momenta,
            self.integration_weights,
        ) = self._quadrature()
        self.unshifted_y = _dot(self.loop_momenta, self.loop_momenta)
        self.unshifted_regulator = self._regulator(self.unshifted_y)
        self.unshifted_regulator_prime = self._regulator_prime(
            self.unshifted_y
        )
        self.geometries: dict[
            tuple[int, str, float], _MomentumGeometry
        ] = {}
        for momentum in config.momentum_samples:
            variants = {
                2: ("default",),
                3: ("orthogonal", "collinear"),
                4: ("cross", "collinear", "asymmetric"),
            }
            for point_count, names in variants.items():
                for name in names:
                    external = self._external_configuration(
                        point_count,
                        momentum,
                        name,
                    )
                    self.geometries[
                        (point_count, name, momentum)
                    ] = _MomentumGeometry(
                        external,
                        self.loop_momenta,
                        self._regulator,
                    )

    def _quadrature(self) -> tuple[np.ndarray, np.ndarray]:
        radial_x, radial_weight = leggauss(self.config.radial_nodes)
        radial_upper = (
            1.0
            if self.config.regulator_family == "theta3"
            else self.config.radial_cutoff
        )
        y = (radial_x + 1.0) * radial_upper / 2.0
        radial_weight = radial_weight * radial_upper / 2.0
        shell_measure = (
            2.0
            * pi ** (self.dimension / 2.0)
            / gamma(self.dimension / 2.0)
            / (2.0 * pi) ** self.dimension
        )
        radial_measure = (
            shell_measure
            / 2.0
            * radial_weight
            * y ** (self.dimension / 2.0 - 1.0)
        )

        if self.dimension == 2:
            angles = (
                2.0
                * pi
                * (np.arange(self.config.planar_angle_nodes) + 0.5)
                / self.config.planar_angle_nodes
            )
            directions = np.column_stack((np.cos(angles), np.sin(angles)))
            angular_weight = np.full(
                self.config.planar_angle_nodes,
                1.0 / self.config.planar_angle_nodes,
            )
        else:
            cosine, polar_weight = leggauss(self.config.polar_nodes)
            azimuth = (
                2.0
                * pi
                * (np.arange(self.config.azimuthal_nodes) + 0.5)
                / self.config.azimuthal_nodes
            )
            cosine_grid, azimuth_grid = np.meshgrid(
                cosine,
                azimuth,
                indexing="ij",
            )
            sine_grid = np.sqrt(np.maximum(0.0, 1.0 - cosine_grid**2))
            directions = np.column_stack(
                (
                    (sine_grid * np.cos(azimuth_grid)).ravel(),
                    (sine_grid * np.sin(azimuth_grid)).ravel(),
                    cosine_grid.ravel(),
                )
            )
            angular_weight = np.repeat(
                polar_weight / 2.0 / self.config.azimuthal_nodes,
                self.config.azimuthal_nodes,
            )

        momenta = (
            np.sqrt(y)[:, None, None] * directions[None, :, :]
        ).reshape(-1, self.dimension)
        weights = (
            radial_measure[:, None] * angular_weight[None, :]
        ).ravel()
        return momenta, weights

    def _regulator(self, argument: np.ndarray) -> np.ndarray:
        argument = np.asarray(argument, dtype=float)
        if self.config.regulator_family == "exponential":
            return self.alpha * np.exp(-argument)
        if self.config.regulator_family == "theta3":
            distance = np.maximum(1.0 - argument, 0.0)
            return self.alpha * distance**3
        denominator = np.expm1(argument)
        with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
            value = self.alpha * argument / denominator
        small = np.abs(argument) < 1.0e-5
        if np.any(small):
            value = np.array(value, copy=True)
            x = argument[small]
            value[small] = self.alpha * (
                1.0 - x / 2.0 + x**2 / 12.0 - x**4 / 720.0
            )
        return value

    def _regulator_prime(self, argument: np.ndarray) -> np.ndarray:
        argument = np.asarray(argument, dtype=float)
        if self.config.regulator_family == "exponential":
            return -self.alpha * np.exp(-argument)
        if self.config.regulator_family == "theta3":
            distance = np.maximum(1.0 - argument, 0.0)
            return -3.0 * self.alpha * distance**2
        denominator = np.expm1(argument)
        exponential = denominator + 1.0
        with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
            value = (
                self.alpha
                * (denominator - argument * exponential)
                / denominator**2
            )
        small = np.abs(argument) < 1.0e-5
        if np.any(small):
            value = np.array(value, copy=True)
            x = argument[small]
            value[small] = self.alpha * (
                -0.5 + x / 6.0 - x**3 / 180.0
            )
        return value

    def _external_configuration(
        self,
        point_count: int,
        momentum: float,
        variant: str,
    ) -> tuple[np.ndarray, ...]:
        first = np.zeros(self.dimension)
        second = np.zeros(self.dimension)
        first[0] = momentum
        second[1] = momentum
        if point_count == 2:
            if variant != "default":
                raise ValueError("The two-point projector has one configuration")
            return (first, -first)
        if point_count == 3:
            if variant == "orthogonal":
                return (first, second, -first - second)
            if variant == "collinear":
                return (first, first, -2.0 * first)
            raise ValueError(f"Unknown three-point projector {variant!r}")
        if point_count == 4:
            if variant == "cross":
                return (first, -first, second, -second)
            if variant == "collinear":
                return (first, first, -first, -first)
            if variant == "asymmetric":
                return (first, first, second, -2.0 * first - second)
            raise ValueError(f"Unknown four-point projector {variant!r}")
        raise ValueError("Only two-, three-, and four-point projectors exist")

    def _cutoff_derivative(self, eta: float) -> np.ndarray:
        return (
            (2.0 - eta) * self.unshifted_regulator
            - 2.0 * self.unshifted_y * self.unshifted_regulator_prime
        )

    def potential_loop(
        self,
        mass: float,
        wavefunction: float,
        wa: float,
        eta: float,
    ) -> tuple[float, float]:
        denominator = (
            mass
            + wavefunction * self.unshifted_y
            + wa * self.unshifted_y**2
            + self.unshifted_regulator
        )
        minimum = float(np.min(denominator))
        if minimum <= 1.0e-5:
            raise FloatingPointError(
                "The DE4 regularized inverse propagator crossed zero"
            )
        value = 0.5 * float(
            np.sum(
                self.integration_weights
                * self._cutoff_derivative(eta)
                / denominator
            )
        )
        return value, minimum

    def vertex_loop(
        self,
        point_count: int,
        variant: str,
        momentum: float,
        jets: _FieldJets,
        mass: float,
        wavefunction: float,
        wa: float,
        eta: float,
    ) -> tuple[float, float]:
        return self.geometries[(point_count, variant, momentum)].flow(
            jets,
            mass,
            wavefunction,
            wa,
            self._cutoff_derivative(eta),
            self.integration_weights,
            strict_vertex_products=self.config.strict_vertex_products,
            strict_product_order=self.config.strict_product_order,
        )

    def _momentum_coefficients(
        self,
        values: np.ndarray,
    ) -> np.ndarray:
        momentum = np.asarray(self.config.momentum_samples)
        scale = float(np.max(momentum**2))
        coefficients = np.polynomial.polynomial.polyfit(
            momentum**2 / scale,
            np.asarray(values),
            self.config.momentum_fit_degree,
        )
        return coefficients / scale ** np.arange(coefficients.size)

    def projected_loops(
        self,
        jets: _FieldJets,
        mass: float,
        wavefunction: float,
        wa: float,
        eta: float,
    ) -> tuple[float, float, float, float, float, float]:
        """Project all five loop functions without numerical field derivatives.

        Two independent three-point momentum configurations determine
        ``d Wa/d phi`` and ``B=phi Wb``.  Three independent four-point
        configurations determine ``d²Wa/dphi²``, ``dB/dphi``, and ``Wc``.
        The derivative projections are redundant with differentiation of the
        two-point flow and therefore also provide a stringent internal check.
        """

        potential, minimum = self.potential_loop(
            mass,
            wavefunction,
            wa,
            eta,
        )
        point_values: dict[tuple[int, str], list[float]] = {
            (2, "default"): [],
            (3, "orthogonal"): [],
            (3, "collinear"): [],
            (4, "cross"): [],
            (4, "collinear"): [],
            (4, "asymmetric"): [],
        }
        for momentum in self.config.momentum_samples:
            for (point_count, variant), values in point_values.items():
                value, local_minimum = self.vertex_loop(
                    point_count,
                    variant,
                    momentum,
                    jets,
                    mass,
                    wavefunction,
                    wa,
                    eta,
                )
                values.append(value)
                minimum = min(minimum, local_minimum)
        coefficients_two = self._momentum_coefficients(
            np.asarray(point_values[(2, "default")])
        )
        three_orthogonal = self._momentum_coefficients(
            np.asarray(point_values[(3, "orthogonal")])
        )[2]
        three_collinear = self._momentum_coefficients(
            np.asarray(point_values[(3, "collinear")])
        )[2]
        wa_phi_loop = three_collinear / 9.0
        b_loop = wa_phi_loop - three_orthogonal / 2.0

        four_rows = np.asarray(
            [
                self._momentum_coefficients(
                    np.asarray(point_values[(4, variant)])
                )[2]
                for variant in ("cross", "collinear", "asymmetric")
            ]
        )
        four_projector = np.asarray(
            [
                [2.0, -4.0, 4.0],
                [6.0, -4.0, 12.0],
                [10.0, -4.0, -4.0],
            ]
        )
        _, b_phi_loop, wc_loop = np.linalg.solve(
            four_projector,
            four_rows,
        )
        return (
            potential,
            float(coefficients_two[1]),
            float(coefficients_two[2]),
            float(b_loop),
            float(b_phi_loop),
            float(wc_loop),
        )

    def projected_de2_loops(
        self,
        jets: _FieldJets,
        mass: float,
        wavefunction: float,
        eta: float,
    ) -> tuple[float, float]:
        """Project only the potential and wave-function loops."""

        potential, _ = self.potential_loop(
            mass,
            wavefunction,
            0.0,
            eta,
        )
        values = []
        for momentum in self.config.momentum_samples:
            value, _ = self.vertex_loop(
                2,
                "default",
                momentum,
                jets,
                mass,
                wavefunction,
                0.0,
                eta,
            )
            values.append(value)
        coefficients = self._momentum_coefficients(np.asarray(values))
        return potential, float(coefficients[1])


class _SpectralDE4Flow:
    """Global Chebyshev collocation equations for the five DE4 functions."""

    def __init__(self, config: DE4Config):
        config.validate()
        self.config = config
        self.rho, first_matrix = _chebyshev_differentiation(
            config.collocation_order,
            config.resolved_rho_max(),
        )
        self.derivative_matrices = [
            np.eye(config.collocation_order + 1)
        ]
        for _ in range(6):
            self.derivative_matrices.append(
                first_matrix @ self.derivative_matrices[-1]
            )
        self.vertex_flow = DE4VertexFlow(config)
        self.last_minimum_inverse_propagator = float("nan")

    def unpack(
        self,
        values: np.ndarray,
    ) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, float]:
        order = self.config.collocation_order
        count = order + 1
        cursor = 0
        potential = np.concatenate(([0.0], values[cursor : cursor + order]))
        cursor += order
        wavefunction = np.concatenate(([1.0], values[cursor : cursor + order]))
        cursor += order
        wa = np.asarray(values[cursor : cursor + count])
        cursor += count
        wb = np.asarray(values[cursor : cursor + count])
        cursor += count
        wc = np.asarray(values[cursor : cursor + count])
        cursor += count
        eta = float(values[cursor])
        return potential, wavefunction, wa, wb, wc, eta

    def pack(
        self,
        potential: np.ndarray,
        wavefunction: np.ndarray,
        wa: np.ndarray,
        wb: np.ndarray,
        wc: np.ndarray,
        eta: float,
    ) -> np.ndarray:
        return np.concatenate(
            (
                np.asarray(potential)[1:],
                np.asarray(wavefunction)[1:],
                np.asarray(wa),
                np.asarray(wb),
                np.asarray(wc),
                [eta],
            )
        )

    def _rho_derivatives(self, values: np.ndarray) -> list[np.ndarray]:
        return [matrix @ values for matrix in self.derivative_matrices]

    def _jets(
        self,
        potential: np.ndarray,
        wavefunction: np.ndarray,
        wa: np.ndarray,
        wb: np.ndarray,
        wc: np.ndarray,
    ) -> tuple[list[_FieldJets], dict[str, list[np.ndarray]]]:
        rho_derivatives = {
            "potential": self._rho_derivatives(potential),
            "wavefunction": self._rho_derivatives(wavefunction),
            "wa": self._rho_derivatives(wa),
            "wb": self._rho_derivatives(wb),
            "wc": self._rho_derivatives(wc),
        }
        phi_derivatives = {
            "potential": _rho_to_phi_derivatives(
                self.rho,
                rho_derivatives["potential"],
                6,
            ),
            "wavefunction": _rho_to_phi_derivatives(
                self.rho,
                rho_derivatives["wavefunction"],
                4,
            ),
            "wa": _rho_to_phi_derivatives(
                self.rho,
                rho_derivatives["wa"],
                4,
            ),
            "wb": _rho_to_phi_derivatives(
                self.rho,
                rho_derivatives["wb"],
                3,
            ),
            "wc": _rho_to_phi_derivatives(
                self.rho,
                rho_derivatives["wc"],
                2,
            ),
        }
        field = np.sqrt(2.0 * np.maximum(self.rho, 0.0))
        b_derivatives = [field * phi_derivatives["wb"][0]]
        for derivative_order in range(1, 4):
            b_derivatives.append(
                field * phi_derivatives["wb"][derivative_order]
                + derivative_order
                * phi_derivatives["wb"][derivative_order - 1]
            )

        jets: list[_FieldJets] = []
        for index in range(len(self.rho)):
            jets.append(
                _FieldJets(
                    potential=tuple(
                        float(values[index])
                        for values in phi_derivatives["potential"]
                    ),
                    wavefunction=tuple(
                        float(values[index])
                        for values in phi_derivatives["wavefunction"]
                    ),
                    wa=tuple(
                        float(values[index])
                        for values in phi_derivatives["wa"]
                    ),
                    b=tuple(
                        float(values[index])
                        for values in b_derivatives
                    ),
                    wc=tuple(
                        float(values[index])
                        for values in phi_derivatives["wc"]
                    ),
                )
            )
        phi_derivatives["b"] = b_derivatives
        return jets, phi_derivatives

    def loop_functions(
        self,
        values: np.ndarray,
        high_derivative_feedback: float | None = None,
    ) -> tuple[dict[str, np.ndarray], float]:
        potential, wavefunction, wa, wb, wc, eta = self.unpack(values)
        jets, _ = self._jets(potential, wavefunction, wa, wb, wc)
        feedback = (
            self.config.high_derivative_feedback
            if high_derivative_feedback is None
            else float(high_derivative_feedback)
        )
        projected = []
        minimum = np.inf
        for index, local_jets in enumerate(jets):
            if feedback < 1.0:
                local_jets = _FieldJets(
                    potential=local_jets.potential,
                    wavefunction=local_jets.wavefunction,
                    wa=tuple(feedback * value for value in local_jets.wa),
                    b=tuple(feedback * value for value in local_jets.b),
                    wc=tuple(feedback * value for value in local_jets.wc),
                )
            if feedback == 0.0:
                potential_loop, wavefunction_loop = (
                    self.vertex_flow.projected_de2_loops(
                        local_jets,
                        local_jets.potential[2],
                        wavefunction[index],
                        eta,
                    )
                )
                result = (
                    potential_loop,
                    wavefunction_loop,
                    0.0,
                    0.0,
                    0.0,
                    0.0,
                )
            else:
                result = self.vertex_flow.projected_loops(
                    local_jets,
                    local_jets.potential[2],
                    wavefunction[index],
                    feedback * wa[index],
                    eta,
                )
            projected.append(result)
            denominator = (
                local_jets.potential[2]
                + wavefunction[index] * self.vertex_flow.unshifted_y
                + feedback
                * wa[index]
                * self.vertex_flow.unshifted_y**2
                + self.vertex_flow.unshifted_regulator
            )
            minimum = min(minimum, float(np.min(denominator)))

        projected_array = np.asarray(projected)
        potential_loop = projected_array[:, 0]
        wavefunction_loop = projected_array[:, 1]
        wa_loop = feedback * projected_array[:, 2]
        b_loop = feedback * projected_array[:, 3]
        b_phi_loop = feedback * projected_array[:, 4]
        wc_loop = feedback * projected_array[:, 5]

        field = np.sqrt(2.0 * np.maximum(self.rho, 0.0))
        wb_loop = np.empty_like(b_loop)
        positive = field > 0.0
        wb_loop[positive] = b_loop[positive] / field[positive]
        if np.any(~positive):
            # B(phi)=phi Wb(rho) is odd.  The origin value is B'(0).
            wb_loop[~positive] = b_phi_loop[~positive]
        return (
            {
                "potential": potential_loop,
                "wavefunction": wavefunction_loop,
                "wa": wa_loop,
                "wb": wb_loop,
                "wc": wc_loop,
            },
            minimum,
        )

    def residual(
        self,
        values: np.ndarray,
        high_derivative_feedback: float | None = None,
    ) -> np.ndarray:
        (
            potential,
            wavefunction,
            wa,
            wb,
            wc,
            eta,
        ) = self.unpack(np.asarray(values, dtype=float))
        size = values.size
        feedback = (
            self.config.high_derivative_feedback
            if high_derivative_feedback is None
            else float(high_derivative_feedback)
        )
        if (
            eta < 0.0
            or eta > 0.6
            or feedback < -0.02
            or feedback > 1.05
            or np.min(wavefunction) <= 0.02
            or not np.all(np.isfinite(values))
        ):
            return np.full(size, 1.0e5)
        try:
            loops, minimum = self.loop_functions(
                values,
                feedback,
            )
        except (FloatingPointError, OverflowError, np.linalg.LinAlgError):
            return np.full(size, 1.0e5)
        self.last_minimum_inverse_propagator = minimum

        derivatives = {
            "potential": self._rho_derivatives(potential),
            "wavefunction": self._rho_derivatives(wavefunction),
            "wa": self._rho_derivatives(wa),
            "wb": self._rho_derivatives(wb),
            "wc": self._rho_derivatives(wc),
        }
        dimension = float(self.config.dimension)
        field_dimension = (dimension - 2.0 + eta) / 2.0
        radial_scaling = 2.0 * field_dimension * self.rho
        potential_flow = (
            -dimension * potential
            + radial_scaling * derivatives["potential"][1]
            + loops["potential"]
        )
        wavefunction_flow = (
            eta * wavefunction
            + radial_scaling * derivatives["wavefunction"][1]
            + loops["wavefunction"]
        )
        wa_flow = (
            (2.0 + eta) * wa
            + radial_scaling * derivatives["wa"][1]
            + loops["wa"]
        )
        four_field_scaling = 4.0 - dimension + 4.0 * field_dimension
        wb_flow = (
            four_field_scaling * wb
            + radial_scaling * derivatives["wb"][1]
            + loops["wb"]
        )
        wc_flow = (
            four_field_scaling * wc
            + radial_scaling * derivatives["wc"][1]
            + loops["wc"]
        )
        potential_scale = 10.0 if dimension < 2.5 else 30.0
        result = np.concatenate(
            (
                potential_scale
                * (potential_flow[1:] - potential_flow[0]),
                wavefunction_flow,
                wa_flow,
                wb_flow,
                wc_flow,
            )
        )
        if result.shape != (size,) or not np.all(np.isfinite(result)):
            return np.full(size, 1.0e5)
        return result

    def initial_vector(
        self,
        initial: DE4Solution | SpectralDE2Solution | np.ndarray | None,
    ) -> np.ndarray:
        count = self.config.collocation_order + 1

        def interpolate(
            old_rho: np.ndarray,
            old_values: np.ndarray,
        ) -> np.ndarray:
            """Continue a global polynomial without injecting grid-scale kinks."""

            interpolator = BarycentricInterpolator(
                np.asarray(old_rho, dtype=float),
                np.asarray(old_values, dtype=float),
            )
            return np.asarray(interpolator(self.rho), dtype=float)

        if isinstance(initial, DE4Solution):
            old_rho = np.asarray(initial.rho_nodes)
            interpolated = [
                interpolate(old_rho, np.asarray(values))
                for values in (
                    initial.potential_values,
                    initial.wavefunction_values,
                    initial.wa_values,
                    initial.wb_values,
                    initial.wc_values,
                )
            ]
            return self.pack(*interpolated, initial.eta)
        if isinstance(initial, SpectralDE2Solution):
            potential = interpolate(
                np.asarray(initial.rho_nodes),
                np.asarray(initial.potential_values),
            )
            wavefunction = interpolate(
                np.asarray(initial.rho_nodes),
                np.asarray(initial.wavefunction_values),
            )
            zeros = np.zeros(count)
            return self.pack(
                potential,
                wavefunction,
                zeros,
                zeros,
                zeros,
                initial.eta,
            )
        if initial is not None:
            values = np.asarray(initial, dtype=float)
            expected = 5 * self.config.collocation_order + 4
            if values.shape != (expected,):
                raise ValueError(
                    f"Initial vector has shape {values.shape}; "
                    f"expected {(expected,)}"
                )
            return values

        de2 = solve_spectral_de2_fixed_point(
            SpectralDE2Config(
                dimension=float(self.config.dimension),
                regulator_alpha=self.config.regulator_alpha,
                regulator_family=self.config.regulator_family,
                collocation_order=self.config.collocation_order,
                rho_max=self.config.resolved_rho_max(),
                radial_nodes=max(64, self.config.radial_nodes),
                angular_nodes=max(20, self.config.polar_nodes),
                radial_cutoff=max(35.0, self.config.radial_cutoff),
                momentum_samples=(0.012, 0.018, 0.025, 0.034),
            )
        )
        return self.initial_vector(de2)

    def kappa(self, potential: np.ndarray) -> float:
        derivative = self.derivative_matrices[1] @ potential
        crossings = np.flatnonzero(
            (derivative[:-1] <= 0.0) & (derivative[1:] > 0.0)
        )
        if len(crossings) != 1:
            return float("nan")
        index = int(crossings[0])
        left, right = self.rho[index : index + 2]
        derivative_left, derivative_right = derivative[index : index + 2]
        return float(
            left
            - derivative_left
            * (right - left)
            / (derivative_right - derivative_left)
        )


def solve_spectral_de4_fixed_point(
    config: DE4Config,
    initial: DE4Solution | SpectralDE2Solution | np.ndarray | None = None,
    *,
    workers: Callable | None = None,
    maximum_evaluations: int = 1200,
    finite_difference_step: float = 1.0e-5,
    solver_method: str = "trf",
    target_residual_norm: float = 3.0e-7,
) -> DE4Solution:
    """Solve one five-function scalar DE4 fixed point."""

    if solver_method not in ("trf", "lm", "krylov"):
        raise ValueError("solver_method must be 'trf', 'lm', or 'krylov'")
    flow = _SpectralDE4Flow(config)
    initial_values = flow.initial_vector(initial)
    initial_residual = flow.residual(initial_values)
    initial_residual_norm = float(np.linalg.norm(initial_residual))
    if initial_residual_norm <= target_residual_norm:
        return _build_solution(
            config,
            flow,
            initial_values,
            initial_residual_norm,
            1,
        )

    def stop_at_residual_gate(intermediate_result) -> None:
        if (
            intermediate_result.fun is not None
            and np.linalg.norm(intermediate_result.fun)
            <= target_residual_norm
        ):
            raise StopIteration

    if solver_method == "krylov":
        result = root(
            flow.residual,
            initial_values,
            method="krylov",
            options={
                "fatol": target_residual_norm / np.sqrt(len(initial_values)),
                "maxiter": maximum_evaluations,
                "jac_options": {"rdiff": finite_difference_step},
            },
        )
        stopped_at_gate = False
    else:
        result = least_squares(
            flow.residual,
            initial_values,
            jac="2-point",
            diff_step=finite_difference_step,
            method=solver_method,
            xtol=3.0e-9,
            ftol=3.0e-9,
            gtol=3.0e-9,
            max_nfev=maximum_evaluations,
            x_scale="jac",
            workers=workers,
            callback=stop_at_residual_gate,
            verbose=0,
        )
        stopped_at_gate = (
            result.status == -2
            and float(np.linalg.norm(result.fun)) <= target_residual_norm
        )
    residual_norm = float(np.linalg.norm(result.fun))
    if (
        not (result.success or stopped_at_gate or residual_norm <= target_residual_norm)
        or residual_norm > 3.0e-6
    ):
        raise RuntimeError(
            "DE4 fixed-point solve failed: "
            f"success={result.success}, residual={residual_norm:.3e}, "
            f"message={result.message}"
        )
    return _build_solution(
        config,
        flow,
        result.x,
        residual_norm,
        int(getattr(result, "nfev", 0)),
    )


def linear_high_derivative_seed(
    config: DE4Config,
    base: DE4Solution,
) -> np.ndarray:
    """Solve the exact linear tangent of the high-derivative sector at s=0."""

    if base.high_derivative_feedback != 0.0:
        raise ValueError("The linear high-sector seed requires an s=0 base")
    feedback = config.high_derivative_feedback
    if not 0.0 < feedback <= 0.05:
        raise ValueError("The linear high-sector seed is local to s=0")

    flow = _SpectralDE4Flow(config)
    base_values = flow.initial_vector(base)
    potential, wavefunction, _, _, _, eta = flow.unpack(base_values)
    source = flow.residual(base_values, feedback)
    order = config.collocation_order
    count = order + 1
    high_start = order + count
    wa_source = source[high_start : high_start + count]
    wb_source = source[high_start + count : high_start + 2 * count]
    wc_source = source[high_start + 2 * count :]

    dimension = float(config.dimension)
    field_dimension = (dimension - 2.0 + eta) / 2.0
    radial_scaling = 2.0 * field_dimension * flow.rho
    transport = (
        radial_scaling[:, None] * flow.derivative_matrices[1]
    )
    wa_operator = (
        (2.0 + eta) * np.eye(count)
        + transport
    )
    four_field_scaling = (
        4.0 - dimension + 4.0 * field_dimension
    )
    four_operator = (
        four_field_scaling * np.eye(count)
        + transport
    )
    wa = np.linalg.solve(wa_operator, -wa_source)
    wb = np.linalg.solve(four_operator, -wb_source)
    wc = np.linalg.solve(four_operator, -wc_source)
    return flow.pack(
        potential,
        wavefunction,
        wa,
        wb,
        wc,
        eta,
    )


def precondition_high_derivative_sector(
    config: DE4Config,
    initial: DE4Solution | np.ndarray,
    *,
    workers: Callable | None = None,
    maximum_evaluations: int = 80,
    finite_difference_step: float = 1.0e-5,
    target_residual_norm: float = 3.0e-7,
) -> np.ndarray:
    """Correct Wa/Wb/Wc while temporarily holding U, Z, and eta fixed."""

    flow = _SpectralDE4Flow(config)
    initial_values = flow.initial_vector(initial)
    potential, wavefunction, wa, wb, wc, eta = flow.unpack(
        initial_values
    )
    order = config.collocation_order
    count = order + 1
    high_start = order + count

    def expand(high_values: np.ndarray) -> np.ndarray:
        return flow.pack(
            potential,
            wavefunction,
            high_values[:count],
            high_values[count : 2 * count],
            high_values[2 * count :],
            eta,
        )

    def high_residual(high_values: np.ndarray) -> np.ndarray:
        return flow.residual(expand(high_values))[high_start:]

    high_initial = np.concatenate((wa, wb, wc))
    initial_norm = float(np.linalg.norm(high_residual(high_initial)))
    if initial_norm <= target_residual_norm:
        return initial_values

    def stop_at_residual_gate(intermediate_result) -> None:
        if (
            intermediate_result.fun is not None
            and np.linalg.norm(intermediate_result.fun)
            <= target_residual_norm
        ):
            raise StopIteration

    result = least_squares(
        high_residual,
        high_initial,
        jac="2-point",
        diff_step=finite_difference_step,
        xtol=3.0e-9,
        ftol=3.0e-9,
        gtol=3.0e-9,
        max_nfev=maximum_evaluations,
        x_scale="jac",
        workers=workers,
        callback=stop_at_residual_gate,
        verbose=0,
    )
    residual_norm = float(np.linalg.norm(result.fun))
    stopped_at_gate = (
        result.status == -2
        and residual_norm <= target_residual_norm
    )
    if not (result.success or stopped_at_gate):
        raise RuntimeError(
            "DE4 high-sector preconditioner failed: "
            f"residual={residual_norm:.3e}, message={result.message}"
        )
    return expand(result.x)


def solve_spectral_de4_pseudo_arclength(
    config: DE4Config,
    earlier: DE4Solution,
    previous: DE4Solution,
    *,
    step_multiplier: float = 1.0,
    workers: Callable | None = None,
    maximum_evaluations: int = 200,
    finite_difference_step: float = 1.0e-5,
    target_residual_norm: float = 3.0e-7,
    solver_method: str = "trf",
) -> DE4Solution:
    """Take one predictor-corrector step along a possibly folded branch."""

    if step_multiplier <= 0.0:
        raise ValueError("step_multiplier must be positive")
    if solver_method not in ("trf", "krylov"):
        raise ValueError("solver_method must be 'trf' or 'krylov'")
    feedback_difference = (
        previous.high_derivative_feedback
        - earlier.high_derivative_feedback
    )
    if abs(feedback_difference) < 1.0e-12:
        raise ValueError("Pseudo-arclength seeds need distinct feedback values")

    flow = _SpectralDE4Flow(config)
    earlier_values = flow.initial_vector(earlier)
    previous_values = flow.initial_vector(previous)
    variable_scale = np.maximum.reduce(
        (
            np.abs(earlier_values),
            np.abs(previous_values),
            np.full_like(previous_values, 0.1),
        )
    )
    feedback_scale = max(abs(feedback_difference), 1.0e-4)
    earlier_scaled = np.concatenate(
        (
            earlier_values / variable_scale,
            [earlier.high_derivative_feedback / feedback_scale],
        )
    )
    previous_scaled = np.concatenate(
        (
            previous_values / variable_scale,
            [previous.high_derivative_feedback / feedback_scale],
        )
    )
    secant = previous_scaled - earlier_scaled
    secant_norm = float(np.linalg.norm(secant))
    tangent = secant / secant_norm
    predictor = previous_scaled + step_multiplier * secant

    def augmented_residual(scaled: np.ndarray) -> np.ndarray:
        values = scaled[:-1] * variable_scale
        feedback = float(scaled[-1] * feedback_scale)
        physical = flow.residual(values, feedback)
        arclength = float(np.dot(tangent, scaled - predictor))
        return np.concatenate((physical, [arclength]))

    def stop_at_residual_gate(intermediate_result) -> None:
        if (
            intermediate_result.fun is not None
            and np.linalg.norm(intermediate_result.fun)
            <= target_residual_norm
        ):
            raise StopIteration

    if solver_method == "trf":
        result = least_squares(
            augmented_residual,
            predictor,
            jac="2-point",
            diff_step=finite_difference_step,
            xtol=3.0e-9,
            ftol=3.0e-9,
            gtol=3.0e-9,
            max_nfev=maximum_evaluations,
            x_scale="jac",
            workers=workers,
            callback=stop_at_residual_gate,
            verbose=0,
        )
        stopped_at_gate = (
            result.status == -2
            and float(np.linalg.norm(result.fun)) <= target_residual_norm
        )
        solver_accepted = bool(result.success or stopped_at_gate)
    else:
        result = root(
            augmented_residual,
            predictor,
            method="krylov",
            options={
                "fatol": target_residual_norm / np.sqrt(len(predictor)),
                "maxiter": maximum_evaluations,
                "jac_options": {"rdiff": finite_difference_step},
            },
        )
        solver_accepted = bool(
            result.success
            or float(np.linalg.norm(result.fun)) <= target_residual_norm
        )
    values = result.x[:-1] * variable_scale
    feedback = float(result.x[-1] * feedback_scale)
    physical_residual = flow.residual(values, feedback)
    physical_norm = float(np.linalg.norm(physical_residual))
    arclength_residual = abs(
        float(np.dot(tangent, result.x - predictor))
    )
    if (
        not solver_accepted
        or physical_norm > 3.0e-6
        or arclength_residual > 3.0e-6
        or not 0.0 <= feedback <= 1.02
    ):
        raise RuntimeError(
            "DE4 pseudo-arclength step failed: "
            f"success={result.success}, feedback={feedback:.8f}, "
            f"physical_residual={physical_norm:.3e}, "
            f"arc_residual={arclength_residual:.3e}, "
            f"message={result.message}"
        )
    solution_config = replace(
        config,
        high_derivative_feedback=feedback,
    )
    solution_flow = _SpectralDE4Flow(solution_config)
    return _build_solution(
        solution_config,
        solution_flow,
        values,
        physical_norm,
        int(getattr(result, "nfev", 0)),
    )


def _build_solution(
    config: DE4Config,
    flow: _SpectralDE4Flow,
    values: np.ndarray,
    residual_norm: float,
    function_evaluations: int,
) -> DE4Solution:
    """Build a checkpoint after applying the common physical-solution gates."""

    potential, wavefunction, wa, wb, wc, eta = flow.unpack(values)
    kappa = flow.kappa(potential)
    if not np.isfinite(kappa):
        raise RuntimeError("DE4 solution did not contain one physical minimum")
    flow.residual(values)
    return DE4Solution(
        dimension=config.dimension,
        regulator=config.regulator_family,
        regulator_alpha=float(config.regulator_alpha),
        collocation_order=config.collocation_order,
        rho_max=config.resolved_rho_max(),
        rho_nodes=tuple(float(value) for value in flow.rho),
        potential_values=tuple(float(value) for value in potential),
        wavefunction_values=tuple(float(value) for value in wavefunction),
        wa_values=tuple(float(value) for value in wa),
        wb_values=tuple(float(value) for value in wb),
        wc_values=tuple(float(value) for value in wc),
        eta=eta,
        kappa=kappa,
        residual_norm=residual_norm,
        function_evaluations=function_evaluations,
        radial_nodes=config.radial_nodes,
        polar_nodes=config.polar_nodes,
        azimuthal_nodes=config.azimuthal_nodes,
        planar_angle_nodes=config.planar_angle_nodes,
        radial_cutoff=config.radial_cutoff,
        momentum_samples=config.momentum_samples,
        momentum_fit_degree=config.momentum_fit_degree,
        minimum_inverse_propagator=flow.last_minimum_inverse_propagator,
        high_derivative_feedback=config.high_derivative_feedback,
        strict_vertex_products=config.strict_vertex_products,
        strict_product_order=config.strict_product_order,
    )


def solve_spectral_reduced_de2_fixed_point(
    config: DE4Config,
    initial: DE4Solution | SpectralDE2Solution | np.ndarray | None = None,
    *,
    maximum_evaluations: int = 400,
    finite_difference_step: float = 1.0e-5,
) -> DE4Solution:
    """Solve a zero-feedback U/Z system in its reduced field space."""

    if (
        config.high_derivative_feedback != 0.0
        or not config.strict_vertex_products
    ):
        raise ValueError(
            "The reduced DE2 solver requires zero high-derivative feedback "
            "and strict vertex products"
        )
    flow = _SpectralDE4Flow(config)
    full_initial = flow.initial_vector(initial)
    potential, wavefunction, _, _, _, eta = flow.unpack(full_initial)
    order = config.collocation_order
    count = order + 1
    zero = np.zeros(count)

    def expand(values: np.ndarray) -> np.ndarray:
        local_potential = np.concatenate(([0.0], values[:order]))
        local_wavefunction = np.concatenate(
            ([1.0], values[order : 2 * order])
        )
        return flow.pack(
            local_potential,
            local_wavefunction,
            zero,
            zero,
            zero,
            float(values[-1]),
        )

    reduced_initial = np.concatenate(
        (potential[1:], wavefunction[1:], [eta])
    )

    def residual(values: np.ndarray) -> np.ndarray:
        full = flow.residual(expand(values))
        return full[: 2 * order + 1]

    result = least_squares(
        residual,
        reduced_initial,
        jac="2-point",
        diff_step=finite_difference_step,
        xtol=3.0e-10,
        ftol=3.0e-10,
        gtol=3.0e-10,
        max_nfev=maximum_evaluations,
        x_scale="jac",
    )
    full_values = expand(result.x)
    full_residual = flow.residual(full_values)
    residual_norm = float(np.linalg.norm(full_residual))
    if not result.success or residual_norm > 3.0e-7:
        raise RuntimeError(
            "Reduced DE2 fixed-point solve failed: "
            f"success={result.success}, residual={residual_norm:.3e}, "
            f"message={result.message}"
        )
    return _build_solution(
        config,
        flow,
        full_values,
        residual_norm,
        int(result.nfev),
    )


def audit_de4_projection(
    config: DE4Config,
    solution: DE4Solution,
) -> DE4ProjectionAudit:
    """Reproject ``eta`` and the full residual without changing the solution."""

    flow = _SpectralDE4Flow(config)
    values = flow.initial_vector(solution)
    residual = flow.residual(values)
    projected_eta = float(solution.eta - residual[config.collocation_order])
    return DE4ProjectionAudit(
        source_eta=float(solution.eta),
        projected_eta=projected_eta,
        eta_shift=projected_eta - float(solution.eta),
        residual_norm=float(np.linalg.norm(residual)),
        minimum_inverse_propagator=float(
            flow.last_minimum_inverse_propagator
        ),
        configuration=asdict(config),
    )
