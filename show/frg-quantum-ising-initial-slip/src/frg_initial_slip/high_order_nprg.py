"""Audited summaries for the strict NPRG derivative expansion."""

from __future__ import annotations

from dataclasses import asdict, dataclass

import numpy as np


@dataclass(frozen=True)
class PMSEstimate:
    """A local quadratic principle-of-minimal-sensitivity estimate."""

    alpha: float
    value: float
    curvature: float
    fit_residual: float
    alpha_points: tuple[float, ...]
    value_points: tuple[float, ...]

    def to_dict(self) -> dict:
        return asdict(self)


@dataclass(frozen=True)
class AlternatingDEEstimate:
    """Improved O(partial^4) estimate following De Polsi et al."""

    order2_extreme: float
    order4_extreme: float
    central: float
    uncertainty: float
    derivative_expansion_error: float
    field_discretization_error: float
    pms_error: float
    momentum_quadrature_error: float
    assumed_convergence_radius: float
    error_combination: str

    def to_dict(self) -> dict:
        return asdict(self)


def quadratic_pms(
    alpha: tuple[float, ...] | list[float],
    values: tuple[float, ...] | list[float],
) -> PMSEstimate:
    """Fit a local quadratic extremum and retain the interpolation residual."""

    alpha_array = np.asarray(alpha, dtype=float)
    value_array = np.asarray(values, dtype=float)
    if alpha_array.shape != value_array.shape or alpha_array.size < 3:
        raise ValueError("PMS requires at least three paired alpha/value points")
    coefficients = np.polyfit(alpha_array, value_array, 2)
    if abs(coefficients[0]) < 1.0e-14:
        raise ValueError("PMS quadratic is numerically flat")
    alpha_pms = float(-coefficients[1] / (2.0 * coefficients[0]))
    if not float(np.min(alpha_array)) <= alpha_pms <= float(np.max(alpha_array)):
        raise ValueError("PMS extremum lies outside the scanned interval")
    fitted = np.polyval(coefficients, alpha_array)
    return PMSEstimate(
        alpha=alpha_pms,
        value=float(np.polyval(coefficients, alpha_pms)),
        curvature=float(2.0 * coefficients[0]),
        fit_residual=float(np.max(np.abs(fitted - value_array))),
        alpha_points=tuple(float(value) for value in alpha_array),
        value_points=tuple(float(value) for value in value_array),
    )


def alternating_de4_estimate(
    order2_extreme: float,
    order4_extreme: float,
    *,
    convergence_radius: float = 4.0,
    field_discretization_error: float = 0.0,
    pms_error: float = 0.0,
    momentum_quadrature_error: float = 0.0,
) -> AlternatingDEEstimate:
    r"""Shift the O(partial^4) bound by half one conservative tail term.

    For alternating derivative-expansion bounds, De Polsi et al. use

    .. math::

       Q_{\rm imp}^{(4)}
       = Q_{\rm ext}^{(4)}
       + \frac{Q_{\rm ext}^{(2)}-Q_{\rm ext}^{(4)}}{2\mathcal R},

    with the conservative convergence radius ``R=4``.  The magnitude of
    this shift is also the derivative-expansion error.  Independently
    measured numerical systematics are added linearly.
    """

    if convergence_radius <= 0.0:
        raise ValueError("convergence_radius must be positive")
    shift = (
        float(order2_extreme) - float(order4_extreme)
    ) / (2.0 * convergence_radius)
    truncation_error = abs(shift)
    numerical_error = (
        abs(field_discretization_error)
        + abs(pms_error)
        + abs(momentum_quadrature_error)
    )
    return AlternatingDEEstimate(
        order2_extreme=float(order2_extreme),
        order4_extreme=float(order4_extreme),
        central=float(order4_extreme) + shift,
        uncertainty=truncation_error + numerical_error,
        derivative_expansion_error=truncation_error,
        field_discretization_error=abs(field_discretization_error),
        pms_error=abs(pms_error),
        momentum_quadrature_error=abs(momentum_quadrature_error),
        assumed_convergence_radius=float(convergence_radius),
        error_combination="linear sum of truncation and numerical systematics",
    )
