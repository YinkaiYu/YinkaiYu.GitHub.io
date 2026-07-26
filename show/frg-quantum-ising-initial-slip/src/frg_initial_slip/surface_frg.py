"""Global bulk fixed points and the leading ordinary-boundary FRG projection.

The bulk fixed-point equation is integrated from its large-field asymptotic
solution down to the origin.  This avoids the loss of convergence of a local
Taylor expansion in two dimensions.

For the ordinary boundary we retain the one-loop boundary wave-function
renormalization of the Dirichlet operator ``O_1 = partial_n phi``, evaluated
with the nonperturbative bulk fixed-point vertex.  In this leading
parallel-momentum shell projection its anomalous dimension is

    eta_1 = -(N + 2) K_{D-1} g_4 / 24

for the Ising case N=1, where ``g_4 = 3 u''`` and
``K_s = S_{s-1}/(2 pi)^s``.  The normalization reproduces the universal
one-loop epsilon expansion as D approaches four.  It is not a full
field-dependent boundary FRG solution.  Evaluating the running vertex at the
potential minimum is the central LPA-prime projection; the origin projection
is retained as an explicit boundary-truncation diagnostic.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from functools import cache
from math import exp, gamma, pi
from warnings import catch_warnings, simplefilter

import numpy as np
from scipy.integrate import solve_ivp
from scipy.optimize import brentq


@dataclass(frozen=True)
class GlobalFixedPoint:
    dimension: float
    eta: float
    asymptotic_amplitude: float
    kappa: float
    lambda2_minimum: float
    lambda2_origin: float
    regularity_residual: float
    self_consistency_residual: float
    iterations: int
    rho_large: float
    rho_epsilon: float

    def to_dict(self) -> dict[str, float | int]:
        return asdict(self)


@dataclass(frozen=True)
class OrdinaryBoundaryProjection:
    projection: str
    lambda2: float
    quartic_vertex: float
    eta_1: float
    eta_parallel: float
    boundary_operator_dimension: float
    surface_field_eigenvalue: float
    theta_ordinary: float
    theta_random_fixed: float
    random_amplitude_eigenvalue: float
    random_variance_correction_exponent: float

    def to_dict(self) -> dict[str, float | str]:
        return asdict(self)


def _loop_coefficient(dimension: float, eta: float) -> float:
    prefactor = (4.0 * pi) ** (-dimension / 2.0) / gamma(dimension / 2.0)
    return prefactor * ((2.0 - eta) / dimension + eta / (dimension + 2.0))


def _bulk_eta(dimension: float, kappa: float, lambda2: float) -> float:
    prefactor = 1.0 / (
        2.0 ** (dimension - 2.0) * pi ** (dimension / 2.0) * gamma(1.0 + dimension / 2.0)
    )
    return prefactor * kappa * lambda2**2 / (1.0 + 2.0 * kappa * lambda2) ** 2


def _asymptotic_initial_values(
    amplitude: float,
    rho: float,
    dimension: float,
    eta: float,
) -> np.ndarray:
    """Leading large-field solution plus its first inverse-field correction."""
    scaling = dimension - 2.0 + eta
    power = dimension / scaling
    correction_power = 1.0 - power
    loop = _loop_coefficient(dimension, eta)
    correction = loop / (amplitude * power * (2.0 * power - 1.0) * (2.0 * dimension - scaling))
    potential = amplitude * rho**power + correction * rho**correction_power
    first = amplitude * power * rho ** (power - 1.0) + correction * correction_power * rho ** (
        correction_power - 1.0
    )
    second = amplitude * power * (power - 1.0) * rho ** (
        power - 2.0
    ) + correction * correction_power * (correction_power - 1.0) * rho ** (correction_power - 2.0)
    return np.array([potential, first, second], dtype=float)


def _integrate_fixed_point(
    log_amplitude: float,
    dimension: float,
    eta: float,
    rho_large: float,
    rho_epsilon: float,
    *,
    dense_output: bool = False,
):
    amplitude = exp(log_amplitude)
    loop = _loop_coefficient(dimension, eta)
    initial = _asymptotic_initial_values(amplitude, rho_large, dimension, eta)

    def flow(rho: float, values: np.ndarray) -> list[float]:
        _, first, second = values
        inverse_propagator = 1.0 + first + 2.0 * rho * second
        numerator = (-2.0 + eta) * first + (dimension - 2.0 + eta) * rho * second
        third = inverse_propagator**2 * numerator / (2.0 * loop * rho) - 1.5 * second / rho
        return [first, second, third]

    with catch_warnings():
        simplefilter("ignore", RuntimeWarning)
        return solve_ivp(
            flow,
            (rho_large, rho_epsilon),
            initial,
            method="Radau",
            rtol=2.0e-9,
            atol=2.0e-11,
            max_step=rho_large / 150.0,
            dense_output=dense_output,
        )


def _regularity_residual(
    log_amplitude: float,
    dimension: float,
    eta: float,
    rho_large: float,
    rho_epsilon: float,
) -> float:
    solution = _integrate_fixed_point(
        log_amplitude,
        dimension,
        eta,
        rho_large,
        rho_epsilon,
    )
    if not solution.success:
        raise RuntimeError("Global fixed-point integration failed before reaching the origin")
    _, first, second = solution.y[:, -1]
    loop = _loop_coefficient(dimension, eta)
    regular_second = (1.0 + first) ** 2 * (-2.0 + eta) * first / (3.0 * loop)
    return float(second - regular_second)


def _amplitude_bracket(dimension: float) -> tuple[float, float]:
    if abs(dimension - 2.0) < 1.0e-12:
        return (7.5, 8.75)
    if abs(dimension - 3.0) < 1.0e-12:
        return (2.5, 3.5)
    raise ValueError("The audited global solver currently supports D=2 or D=3")


def _integration_domain(dimension: float) -> tuple[float, float]:
    if abs(dimension - 2.0) < 1.0e-12:
        return (2.0, 1.0e-6)
    if abs(dimension - 3.0) < 1.0e-12:
        return (10.0, 1.0e-6)
    raise ValueError("The audited global solver currently supports D=2 or D=3")


def _solve_at_eta(
    dimension: float,
    eta: float,
    rho_large: float,
    rho_epsilon: float,
):
    bracket = _amplitude_bracket(dimension)
    log_amplitude = brentq(
        lambda value: _regularity_residual(value, dimension, eta, rho_large, rho_epsilon),
        *bracket,
        xtol=2.0e-11,
        rtol=2.0e-11,
        maxiter=100,
    )
    solution = _integrate_fixed_point(
        log_amplitude,
        dimension,
        eta,
        rho_large,
        rho_epsilon,
        dense_output=True,
    )
    if not solution.success or solution.sol is None:
        raise RuntimeError("Dense global fixed-point integration failed")

    grid = np.geomspace(rho_epsilon, rho_large, 4000)
    first = solution.sol(grid)[1]
    crossings: list[tuple[float, float]] = []
    for left, right, value_left, value_right in zip(
        grid[:-1], grid[1:], first[:-1], first[1:], strict=True
    ):
        if value_left < 0.0 < value_right:
            crossings.append((float(left), float(right)))
    if len(crossings) != 1:
        raise RuntimeError(f"Expected one Wilson-Fisher minimum, found {len(crossings)}")
    kappa = brentq(
        lambda rho: float(solution.sol(rho)[1]),
        *crossings[0],
        xtol=2.0e-13,
        rtol=2.0e-13,
    )
    lambda2_minimum = float(solution.sol(kappa)[2])
    lambda2_origin = float(solution.sol(rho_epsilon)[2])
    eta_updated = _bulk_eta(dimension, kappa, lambda2_minimum)
    residual = _regularity_residual(log_amplitude, dimension, eta, rho_large, rho_epsilon)
    return (
        exp(log_amplitude),
        kappa,
        lambda2_minimum,
        lambda2_origin,
        eta_updated,
        residual,
    )


@cache
def solve_global_fixed_point(dimension: float) -> GlobalFixedPoint:
    """Solve the self-consistent global LPA-prime Ising fixed point."""
    dimension = float(dimension)
    rho_large, rho_epsilon = _integration_domain(dimension)
    eta = 0.234 if dimension == 2.0 else 0.044
    iterations = 0
    for iterations in range(1, 21):
        values = _solve_at_eta(dimension, eta, rho_large, rho_epsilon)
        eta_updated = values[4]
        if abs(eta_updated - eta) < 2.0e-9:
            eta = eta_updated
            break
        eta = 0.35 * eta + 0.65 * eta_updated
    else:
        raise RuntimeError("Bulk anomalous-dimension iteration did not converge")

    (
        amplitude,
        kappa,
        lambda2_minimum,
        lambda2_origin,
        eta_updated,
        regularity_residual,
    ) = _solve_at_eta(dimension, eta, rho_large, rho_epsilon)
    return GlobalFixedPoint(
        dimension=dimension,
        eta=eta_updated,
        asymptotic_amplitude=amplitude,
        kappa=kappa,
        lambda2_minimum=lambda2_minimum,
        lambda2_origin=lambda2_origin,
        regularity_residual=regularity_residual,
        self_consistency_residual=eta_updated - eta,
        iterations=iterations,
        rho_large=rho_large,
        rho_epsilon=rho_epsilon,
    )


def parallel_shell_measure(surface_dimension: float) -> float:
    """K_s = S_{s-1}/(2 pi)^s for a radial s-dimensional shell."""
    sphere_area = 2.0 * pi ** (surface_dimension / 2.0) / gamma(surface_dimension / 2.0)
    return sphere_area / (2.0 * pi) ** surface_dimension


def ordinary_boundary_projection(
    fixed_point: GlobalFixedPoint, projection: str = "minimum"
) -> OrdinaryBoundaryProjection:
    """Project the leading Dirichlet boundary anomalous dimension."""
    if projection == "minimum":
        lambda2 = fixed_point.lambda2_minimum
    elif projection == "origin":
        lambda2 = fixed_point.lambda2_origin
    else:
        raise ValueError("projection must be 'minimum' or 'origin'")

    dimension = fixed_point.dimension
    eta = fixed_point.eta
    surface_dimension = dimension - 1.0
    quartic_vertex = 3.0 * lambda2
    eta_1 = -3.0 / 24.0 * parallel_shell_measure(surface_dimension) * quartic_vertex
    eta_parallel = 2.0 + eta + eta_1
    boundary_operator_dimension = (dimension - 2.0 + eta_parallel) / 2.0
    surface_field_eigenvalue = surface_dimension - boundary_operator_dimension
    bulk_field_dimension = (dimension - 2.0 + eta) / 2.0
    theta_ordinary = boundary_operator_dimension - bulk_field_dimension
    theta_random_fixed = surface_field_eigenvalue - bulk_field_dimension
    random_amplitude_eigenvalue = surface_field_eigenvalue - surface_dimension / 2.0
    random_variance_correction_exponent = -2.0 * random_amplitude_eigenvalue
    return OrdinaryBoundaryProjection(
        projection=projection,
        lambda2=lambda2,
        quartic_vertex=quartic_vertex,
        eta_1=eta_1,
        eta_parallel=eta_parallel,
        boundary_operator_dimension=boundary_operator_dimension,
        surface_field_eigenvalue=surface_field_eigenvalue,
        theta_ordinary=theta_ordinary,
        theta_random_fixed=theta_random_fixed,
        random_amplitude_eigenvalue=random_amplitude_eigenvalue,
        random_variance_correction_exponent=random_variance_correction_exponent,
    )
