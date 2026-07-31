"""Competing long- and short-range kinetic terms in the O(N) model.

This module implements the field-independent-wave-function version of the
``LPA''`` truncation introduced by Defenu, Trombettoni, and Codello.  Both
``q**sigma`` and ``q**2`` are retained.  The potential is expanded about its
running minimum, but its beta functions are evaluated with a small Taylor-jet
algebra rather than with a symbolic series expansion.

The RG time is ``t = log(k / Lambda)``.  Consequently a stability eigenvalue
``lambda > 0`` is infrared irrelevant, while the commonly quoted relevance
exponent is ``y = -lambda``.

Differentiating the published potential flow twice gives a radial-loop term
proportional to ``18 * lambda_2**2`` in the quadratic truncation.  One printed
low-order equation in Appendix B of PRE 92, 052113 appears to omit that second
power; this implementation follows the potential-flow projection, which is
also required dimensionally.
"""

from __future__ import annotations

from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from math import gamma, pi

import numpy as np
from numpy.typing import ArrayLike, NDArray
from scipy.optimize import root

FloatArray = NDArray[np.float64]


def v_d(d: float) -> float:
    """Return the conventional angular factor v_d."""

    return 1.0 / (2.0 ** (d + 1.0) * pi ** (d / 2.0) * gamma(d / 2.0))


@dataclass(frozen=True)
class CompetitiveConfig:
    """Parameters of the Taylor truncation.

    ``order`` is the largest derivative of the dimensionless potential kept
    at its minimum.  The state is ``(kappa, lambda_2, ..., lambda_M, j)``.
    """

    d: float
    n_components: int
    order: int

    def __post_init__(self) -> None:
        if not np.isfinite(self.d) or self.d <= 0.0:
            raise ValueError("d must be a positive finite real number")
        if not isinstance(self.n_components, int) or self.n_components < 1:
            raise ValueError("n_components must be a positive integer")
        if not isinstance(self.order, int) or not 2 <= self.order <= 20:
            raise ValueError("order must be an integer between 2 and 20")

    @property
    def state_size(self) -> int:
        return self.order + 1


class TaylorJet:
    """A truncated ordinary Taylor series with coefficients f^(m)(0)/m!."""

    __array_priority__ = 1000

    def __init__(self, coefficients: ArrayLike):
        values = np.asarray(coefficients)
        if values.ndim != 1:
            raise ValueError("Taylor-jet coefficients must be one-dimensional")
        self.c = values

    @property
    def order(self) -> int:
        return self.c.size - 1

    @classmethod
    def constant(cls, value: complex, order: int) -> TaylorJet:
        dtype = np.result_type(value, float)
        coefficients = np.zeros(order + 1, dtype=dtype)
        coefficients[0] = value
        return cls(coefficients)

    def _coerce(self, other: TaylorJet | complex) -> TaylorJet:
        if isinstance(other, TaylorJet):
            if other.order != self.order:
                raise ValueError("Taylor jets have different truncation orders")
            return other
        return TaylorJet.constant(other, self.order)

    def __add__(self, other: TaylorJet | complex) -> TaylorJet:
        rhs = self._coerce(other)
        return TaylorJet(self.c + rhs.c)

    def __radd__(self, other: TaylorJet | complex) -> TaylorJet:
        return self + other

    def __neg__(self) -> TaylorJet:
        return TaylorJet(-self.c)

    def __sub__(self, other: TaylorJet | complex) -> TaylorJet:
        return self + (-self._coerce(other))

    def __rsub__(self, other: TaylorJet | complex) -> TaylorJet:
        return self._coerce(other) - self

    def __mul__(self, other: TaylorJet | complex) -> TaylorJet:
        rhs = self._coerce(other)
        return TaylorJet(np.convolve(self.c, rhs.c)[: self.order + 1])

    def __rmul__(self, other: TaylorJet | complex) -> TaylorJet:
        return self * other

    def reciprocal(self) -> TaylorJet:
        if self.c[0] == 0:
            raise ZeroDivisionError("Taylor jet has zero constant term")
        result = np.zeros_like(self.c, dtype=np.result_type(self.c, float))
        result[0] = 1.0 / self.c[0]
        for m in range(1, self.order + 1):
            result[m] = -np.dot(self.c[1 : m + 1], result[m - 1 :: -1]) / self.c[0]
        return TaylorJet(result)

    def __truediv__(self, other: TaylorJet | complex) -> TaylorJet:
        return self * self._coerce(other).reciprocal()

    def __rtruediv__(self, other: TaylorJet | complex) -> TaylorJet:
        return self._coerce(other) / self

    def derivative(self) -> TaylorJet:
        result = np.zeros_like(self.c)
        indices = np.arange(1, self.order + 1)
        result[:-1] = indices * self.c[1:]
        return TaylorJet(result)


@dataclass(frozen=True)
class FixedPoint:
    """A fixed point and its linearized universal data."""

    config: CompetitiveConfig
    sigma: float
    branch: str
    state: FloatArray
    beta: FloatArray
    residual: float
    raw_residual: float
    eta: float
    stability_jacobian: FloatArray
    stability_eigenvalues: NDArray[np.complex128]
    stability_eigenvectors: NDArray[np.complex128]
    nu: float
    thermal_lambda: float
    kinetic_lambda: float
    kinetic_y: float

    @property
    def kappa(self) -> float:
        return float(self.state[0])

    @property
    def j(self) -> float:
        return float(self.state[-1])

    @property
    def lambdas(self) -> FloatArray:
        return self.state[1:-1].copy()

    @property
    def sigma_star(self) -> float:
        return 2.0 - self.eta if self.branch == "SR" else float("nan")


def _validate_state(state: ArrayLike, config: CompetitiveConfig) -> NDArray[np.generic]:
    values = np.asarray(state)
    if values.shape != (config.state_size,):
        raise ValueError(f"state must have shape ({config.state_size},)")
    return values


def eta_lpa_prime(state: ArrayLike, sigma: float, config: CompetitiveConfig) -> np.generic:
    """Return the short-range-normalized anomalous dimension eta_2.

    The formula uses the generalized optimized cutoff and includes both
    kinetic terms.  At a nonzero-j fixed point, beta_j=0 enforces
    ``eta_2 = 2 - sigma``.
    """

    values = _validate_state(state, config)
    kappa = values[0]
    lambda_2 = values[1]
    j = values[-1]
    transverse = 1.0 + j
    longitudinal = transverse + 2.0 * kappa * lambda_2
    numerator = (
        16.0
        * v_d(config.d)
        / config.d
        * kappa
        * lambda_2**2
        * (1.0 + 0.5 * sigma * j) ** 2
    )
    return numerator / (transverse**2 * longitudinal**2)


def beta_j_exact(state: ArrayLike, sigma: float, config: CompetitiveConfig) -> np.generic:
    """Exact beta function of the dimensionless LR/SR kinetic ratio."""

    values = _validate_state(state, config)
    return (sigma - 2.0 + eta_lpa_prime(values, sigma, config)) * values[-1]


def potential_flow_jet(
    state: ArrayLike, sigma: float, config: CompetitiveConfig
) -> TaylorJet:
    """Return the fixed-coordinate potential flow as a Taylor jet."""

    values = _validate_state(state, config)
    order = config.order
    kappa = values[0]
    j = values[-1]
    eta = eta_lpa_prime(values, sigma, config)

    coefficients = np.zeros(order + 1, dtype=np.result_type(values, float))
    for m in range(2, order + 1):
        coefficients[m] = values[m - 1] / gamma(m + 1.0)
    potential = TaylorJet(coefficients)
    rho_coefficients = np.zeros(order + 1, dtype=coefficients.dtype)
    rho_coefficients[0] = kappa
    rho_coefficients[1] = 1.0
    rho = TaylorJet(rho_coefficients)

    first = potential.derivative()
    second = first.derivative()
    transverse = 1.0 + j + first
    longitudinal = transverse + 2.0 * rho * second
    loop_amplitude = 4.0 * v_d(config.d) / config.d * (
        1.0 - eta / (config.d + 2.0) + 0.5 * sigma * j
    )

    return (
        -config.d * potential
        + (config.d - 2.0 + eta) * rho * first
        + loop_amplitude
        * ((config.n_components - 1.0) / transverse + 1.0 / longitudinal)
    )


def beta_functions(state: ArrayLike, sigma: float, config: CompetitiveConfig) -> NDArray[np.generic]:
    """Return beta functions for ``(kappa, lambda_2, ..., lambda_M, j)``."""

    values = _validate_state(state, config)
    flow = potential_flow_jet(values, sigma, config).c
    result = np.zeros(config.state_size, dtype=np.result_type(values, float))
    lambda_2 = values[1]
    # Nonlinear solvers may briefly probe the singular Gaussian coordinate
    # lambda_2=0.  Preserve the resulting NaN/inf for rejection, but do not
    # emit a runtime warning for an expected trial point.
    with np.errstate(divide="ignore", invalid="ignore"):
        result[0] = -flow[1] / lambda_2
    for m in range(2, config.order + 1):
        next_lambda = values[m] if m < config.order else 0.0
        result[m - 1] = gamma(m + 1.0) * flow[m] + next_lambda * result[0]
    result[-1] = beta_j_exact(values, sigma, config)
    return result


def normalized_beta_functions(
    state: ArrayLike, sigma: float, config: CompetitiveConfig
) -> NDArray[np.generic]:
    """Return beta functions in ``(kappa, lambda_m/m!, j)`` coordinates.

    This is algebraically equivalent to :func:`beta_functions`, but evaluates
    the moving-minimum term before multiplying by a factorial.  It avoids
    severe cancellation at M=9 and above and is therefore used for root
    finding and residual gates.
    """

    values = _validate_state(state, config)
    flow = potential_flow_jet(values, sigma, config).c
    result = np.zeros(config.state_size, dtype=np.result_type(values, float))
    with np.errstate(divide="ignore", invalid="ignore"):
        result[0] = -flow[1] / values[1]
    for m in range(2, config.order + 1):
        next_coefficient = values[m] / gamma(m + 2.0) if m < config.order else 0.0
        result[m - 1] = flow[m] + (m + 1.0) * next_coefficient * result[0]
    result[-1] = beta_j_exact(values, sigma, config)
    return result


def stability_jacobian(
    state: ArrayLike, sigma: float, config: CompetitiveConfig, *, step: float = 1.0e-28
) -> FloatArray:
    """Differentiate the complete beta vector by the complex-step method."""

    values = np.asarray(_validate_state(state, config), dtype=float)
    jacobian = np.empty((config.state_size, config.state_size), dtype=float)
    for column in range(config.state_size):
        shifted = values.astype(complex)
        shifted[column] += 1j * step
        jacobian[:, column] = np.imag(beta_functions(shifted, sigma, config)) / step
    return jacobian


def _scaled_eigensystem(jacobian: FloatArray, state: FloatArray) -> tuple[
    NDArray[np.complex128], NDArray[np.complex128]
]:
    scales = np.maximum(1.0, np.abs(state))
    scaled = jacobian * scales[np.newaxis, :] / scales[:, np.newaxis]
    return np.linalg.eig(scaled)


def _unique_relevant_eigenvalue(jacobian: FloatArray, branch: str) -> float:
    # At j=0 the stability matrix is block triangular, so taking the potential
    # block cleanly excludes the independently known LR crossover direction.
    # At j>0, however, j and all potential couplings mix: nu must come from the
    # full matrix or it acquires a systematic error.
    matrix = jacobian[:-1, :-1] if branch == "SR" else jacobian
    eigenvalues = np.linalg.eigvals(matrix)
    nearly_real = eigenvalues[np.abs(eigenvalues.imag) < 2.0e-7].real
    relevant = nearly_real[nearly_real < -1.0e-8]
    if relevant.size != 1:
        raise RuntimeError(
            "physical Wilson-Fisher screening expected exactly one relevant RG direction; "
            f"found {relevant.tolist()}"
        )
    return float(relevant[0])


def _make_fixed_point(
    state: ArrayLike,
    sigma: float,
    config: CompetitiveConfig,
    branch: str,
) -> FixedPoint:
    state_array = np.asarray(state, dtype=float)
    beta = np.asarray(beta_functions(state_array, sigma, config), dtype=float)
    raw_residual = float(np.linalg.norm(beta, ord=np.inf))
    residual = float(
        np.linalg.norm(normalized_beta_functions(state_array, sigma, config), ord=np.inf)
    )
    jacobian = stability_jacobian(state_array, sigma, config)
    eigenvalues, eigenvectors = _scaled_eigensystem(jacobian, state_array)
    thermal_lambda = _unique_relevant_eigenvalue(jacobian, branch)
    if branch == "SR":
        # This value is exact within (and beyond) the truncation because
        # beta_j=(sigma-2+eta)j makes the j row triangular at j=0.
        kinetic_lambda = float(sigma - 2.0 + eta_lpa_prime(state_array, sigma, config))
    else:
        # The positive eigenvalue bifurcating from zero at the Sak boundary is
        # the local kinetic crossover mode.  It is selected as the smallest
        # positive real eigenvalue.  Deep in the LR regime an eigenvector-
        # overlap continuation should be used if it approaches another mode.
        real_positive = eigenvalues[
            (np.abs(eigenvalues.imag) < 2.0e-7) & (eigenvalues.real > 1.0e-8)
        ].real
        if real_positive.size == 0:
            raise RuntimeError("LR fixed point has no identifiable kinetic crossover mode")
        kinetic_lambda = float(np.min(real_positive))
    return FixedPoint(
        config=config,
        sigma=float(sigma),
        branch=branch,
        state=state_array,
        beta=beta,
        residual=residual,
        raw_residual=raw_residual,
        eta=float(eta_lpa_prime(state_array, sigma, config)),
        stability_jacobian=jacobian,
        stability_eigenvalues=eigenvalues,
        stability_eigenvectors=eigenvectors,
        nu=-1.0 / thermal_lambda,
        thermal_lambda=thermal_lambda,
        kinetic_lambda=kinetic_lambda,
        kinetic_y=-kinetic_lambda,
    )


def _physical(state: FloatArray, sigma: float, config: CompetitiveConfig, *, lr: bool) -> bool:
    if not np.all(np.isfinite(state)):
        return False
    kappa, lambda_2, j = state[0], state[1], state[-1]
    if kappa <= 1.0e-9 or lambda_2 <= 1.0e-8:
        return False
    if j < (-1.0e-9 if not lr else 1.0e-10):
        return False
    if 1.0 + j <= 1.0e-8 or 1.0 + j + 2.0 * kappa * lambda_2 <= 1.0e-8:
        return False
    eta = float(eta_lpa_prime(state, sigma, config))
    return -1.0e-10 <= eta < 2.0 and (not lr or j > 1.0e-8)


def _state_scales(config: CompetitiveConfig) -> FloatArray:
    """Natural solver scales for derivative rather than jet coefficients."""

    scales = np.ones(config.state_size)
    for m in range(2, config.order + 1):
        scales[m - 1] = gamma(m + 1.0)
    return scales


def _root_scaled(
    equations,
    initial: FloatArray,
    config: CompetitiveConfig,
    *,
    max_iterations: int = 20_000,
    equations_are_normalized: bool = False,
) -> tuple[FloatArray, float]:
    """Solve in normalized Taylor-coefficient coordinates.

    The public state stores derivatives ``lambda_m = u^(m)(kappa)``.  These
    grow roughly factorially with order, which makes an unscaled nonlinear
    solve both slow and prone to false convergence.  Internally we solve for
    ``lambda_m/m!`` and normalize the corresponding beta functions in the
    same way.
    """

    scales = _state_scales(config)

    def normalized(candidate: FloatArray) -> FloatArray:
        values = np.asarray(equations(candidate * scales), dtype=float)
        return values if equations_are_normalized else values / scales

    solution = root(
        normalized,
        np.asarray(initial, dtype=float) / scales,
        method="lm",
        options={
            "ftol": 2.0e-14,
            "xtol": 2.0e-14,
            "gtol": 2.0e-14,
            "maxiter": max_iterations,
        },
    )
    state = np.asarray(solution.x * scales, dtype=float)
    return state, float(np.linalg.norm(normalized(solution.x), ord=np.inf))


def _sr_seed_candidates(config: CompetitiveConfig) -> Iterable[FloatArray]:
    # Traditional v_d normalization makes kappa O(10^-2--10^-1) in d=2--3.
    for kappa in (0.008, 0.02, 0.04, 0.08, 0.16):
        for lambda_2 in (0.5, 2.0, 6.0, 15.0, 35.0):
            seed = np.zeros(config.state_size)
            seed[0] = kappa
            seed[1] = lambda_2
            yield seed


def _pad_order(state: FloatArray, old_order: int, new_order: int) -> FloatArray:
    if new_order != old_order + 1:
        raise ValueError("Taylor continuation advances one order at a time")
    result = np.zeros(new_order + 1)
    result[0] = state[0]
    result[1:old_order] = state[1:old_order]
    result[old_order] = 0.0
    result[-1] = state[-1]
    return result


def solve_sr_fixed_point(
    d: float,
    n_components: int,
    order: int,
    *,
    sigma: float = 2.0,
    initial: Sequence[float] | None = None,
    residual_tolerance: float = 2.0e-9,
) -> FixedPoint:
    """Solve the short-range Wilson-Fisher branch at ``j=0``.

    For orders above two the solver continues in Taylor order.  This is much
    more reliable than attempting a high-dimensional root from an arbitrary
    polynomial seed.
    """

    target = CompetitiveConfig(d, n_components, order)

    if initial is not None:
        candidates = [np.asarray(initial, dtype=float)]
        if candidates[0].shape != (target.state_size,):
            raise ValueError(f"initial must have length {target.state_size}")
    elif order == 2:
        candidates = list(_sr_seed_candidates(target))
    else:
        lower = solve_sr_fixed_point(
            d,
            n_components,
            order - 1,
            sigma=sigma,
            residual_tolerance=max(residual_tolerance, 5.0e-9),
        )
        candidates = [_pad_order(lower.state, order - 1, order)]

    config = target

    def equations(state: FloatArray) -> FloatArray:
        beta = np.asarray(normalized_beta_functions(state, sigma, config), dtype=float)
        # Pin j exactly to zero rather than use beta_j, which is automatically
        # zero for every j at sigma=2-eta and would make the SR solve singular.
        beta[-1] = state[-1]
        return beta

    accepted: list[tuple[float, FloatArray]] = []
    for seed in candidates:
        state, equation_residual = _root_scaled(
            equations, seed, config, equations_are_normalized=True
        )
        state[-1] = 0.0
        full_residual = float(
            np.linalg.norm(normalized_beta_functions(state, sigma, config), ord=np.inf)
        )
        if (
            equation_residual <= residual_tolerance
            and full_residual <= residual_tolerance
            and _physical(state, sigma, config, lr=False)
        ):
            try:
                fixed_point = _make_fixed_point(state, sigma, config, "SR")
            except RuntimeError:
                continue
            # Exclude the Gaussian root and rank physical roots by residual,
            # with a weak preference for moderate couplings.
            if fixed_point.eta > 1.0e-8:
                score = full_residual + 1.0e-15 * np.linalg.norm(state)
                accepted.append((score, state.copy()))

    if not accepted:
        raise RuntimeError(
            f"no physical SR fixed point found for d={d}, N={n_components}, M={order}"
        )
    accepted.sort(key=lambda item: item[0])
    result = _make_fixed_point(accepted[0][1], sigma, config, "SR")
    if result.residual > residual_tolerance:
        raise RuntimeError(f"SR fixed-point residual {result.residual:.3e} exceeds tolerance")
    return result


def solve_lr_fixed_point(
    d: float,
    n_components: int,
    order: int,
    sigma: float,
    *,
    initial: Sequence[float] | None = None,
    continuation_steps: int = 12,
    residual_tolerance: float = 5.0e-9,
) -> FixedPoint:
    """Continue the interacting LR branch from the Sak bifurcation.

    The last root equation is ``eta_2 = 2-sigma`` rather than ``beta_j=0``.
    It removes the ever-present SR solution ``j=0``; the returned point is
    nevertheless validated with the original complete beta vector.
    """

    config = CompetitiveConfig(d, n_components, order)
    sr = solve_sr_fixed_point(d, n_components, order, sigma=sigma)
    sigma_star = 2.0 - sr.eta
    if not sigma < sigma_star - 5.0e-7:
        raise ValueError(
            f"LR branch requires sigma < sigma_star={sigma_star:.12g}; got {sigma}"
        )

    if initial is None:
        state = sr.state.copy()
        state[-1] = max(1.0e-4, 0.25 * (sigma_star - sigma))
    else:
        state = np.asarray(initial, dtype=float)
        if state.shape != (config.state_size,):
            raise ValueError(f"initial must have length {config.state_size}")

    first_gap = min(2.0e-3, 0.08 * (sigma_star - sigma))
    start = sigma_star - max(first_gap, 2.0e-5)
    path = np.linspace(start, sigma, max(2, continuation_steps))

    for current_sigma in path:
        target_eta = 2.0 - current_sigma

        def equations(
            candidate: FloatArray,
            sigma_at_step: float = current_sigma,
            eta_at_step: float = target_eta,
        ) -> FloatArray:
            beta = np.asarray(
                normalized_beta_functions(candidate, sigma_at_step, config), dtype=float
            )
            beta[-1] = float(eta_lpa_prime(candidate, sigma_at_step, config) - eta_at_step)
            return beta

        seeds = [state]
        for factor in (0.25, 0.5, 2.0, 4.0):
            trial = state.copy()
            trial[-1] = max(1.0e-7, state[-1] * factor)
            seeds.append(trial)
        solutions: list[tuple[float, FloatArray]] = []
        for seed in seeds:
            candidate, equation_residual = _root_scaled(
                equations, seed, config, equations_are_normalized=True
            )
            if (
                equation_residual < max(residual_tolerance, 1.0e-8)
                and _physical(candidate, current_sigma, config, lr=True)
            ):
                solutions.append((equation_residual, candidate))
        if not solutions:
            raise RuntimeError(
                f"LR continuation failed at sigma={current_sigma:.12g}, "
                f"target={sigma:.12g}"
            )
        solutions.sort(key=lambda item: (item[0], abs(item[1][-1] - state[-1])))
        state = solutions[0][1]

    result = _make_fixed_point(state, sigma, config, "LR")
    eta_gate = abs(result.eta - (2.0 - sigma))
    if result.residual > residual_tolerance or eta_gate > residual_tolerance:
        raise RuntimeError(
            f"LR fixed point failed gates: residual={result.residual:.3e}, "
            f"|eta-(2-sigma)|={eta_gate:.3e}"
        )
    return result


def sak_relevance_exponent(eta_sr: float, sigma: float) -> float:
    """Exact LR relevance exponent at a short-range fixed point."""

    return 2.0 - eta_sr - sigma


__all__ = [
    "CompetitiveConfig",
    "FixedPoint",
    "TaylorJet",
    "beta_functions",
    "beta_j_exact",
    "eta_lpa_prime",
    "normalized_beta_functions",
    "potential_flow_jet",
    "sak_relevance_exponent",
    "solve_lr_fixed_point",
    "solve_sr_fixed_point",
    "stability_jacobian",
    "v_d",
]
