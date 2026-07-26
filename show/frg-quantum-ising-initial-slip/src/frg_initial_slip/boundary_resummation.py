r"""Two-loop-consistent ordinary-boundary functional-flow resummation.

Expanding the ordinary Dirichlet boundary vertex flow about ``D=4`` gives,
for the Ising universality class,

.. math::

   \theta_{\rm ord}(\epsilon)
   =1-\frac{\epsilon}{6}-\frac{17\epsilon^2}{162}
    +O(\epsilon^3),\qquad \epsilon=4-D .

The coefficients follow from the field and boundary-operator vertices of the
functional flow through two loops.  At a finite loop order, they agree with
the universal minimal-subtraction result.  This module does not pretend that
two loops are a high-order boundary calculation: it resums the short series,
varies all auxiliary parameters over a declared domain, and retains the
resulting order/resummation spread as a systematic error.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass

import numpy as np
from scipy.special import gamma, roots_genlaguerre

BOUNDARY_SERIES = (1.0, -1.0 / 6.0, -17.0 / 162.0)
LEADING_BOREL_SINGULARITY = 1.0 / 3.0


@dataclass(frozen=True)
class BoundaryResummation:
    epsilon: float
    dimension: float
    direct_two_loop: float
    central: float
    uncertainty: float
    stability_interval_68: tuple[float, float]
    stability_interval_90: tuple[float, float]
    stability_minimum: float
    stability_maximum: float
    selected_fraction: float
    borel_leroy_range: tuple[float, float]
    strong_coupling_range: tuple[float, float]
    grid_shape: tuple[int, int]
    leading_borel_singularity: float
    order_shift: float
    selected_count: int

    def to_dict(self) -> dict:
        result = asdict(self)
        for key in (
            "stability_interval_68",
            "stability_interval_90",
            "borel_leroy_range",
            "strong_coupling_range",
            "grid_shape",
        ):
            result[key] = list(result[key])
        return result


def conformal_borel_value(
    epsilon: float,
    borel_leroy: float,
    strong_coupling: float,
    *,
    order: int = 2,
    quadrature_order: int = 96,
) -> float:
    """Return the conformal--Borel--Leroy approximant at order one or two."""
    if order not in (1, 2):
        raise ValueError("Only the available one- and two-loop orders are supported")
    nodes, weights = roots_genlaguerre(quadrature_order, borel_leroy)
    return _conformal_borel_from_quadrature(
        epsilon,
        borel_leroy,
        strong_coupling,
        order,
        nodes,
        weights,
    )


def _conformal_borel_from_quadrature(
    epsilon: float,
    borel_leroy: float,
    strong_coupling: float,
    order: int,
    nodes: np.ndarray,
    weights: np.ndarray,
) -> float:
    argument = epsilon * nodes
    singularity = LEADING_BOREL_SINGULARITY
    conformal = (
        np.sqrt(1.0 + singularity * argument) - 1.0
    ) / (
        np.sqrt(1.0 + singularity * argument) + 1.0
    )

    coefficients = np.asarray(BOUNDARY_SERIES)
    borel = coefficients / np.asarray(
        [gamma(borel_leroy + power + 1.0) for power in range(3)]
    )
    inverse_map_linear = 4.0 / singularity
    mapped = [
        borel[0],
        inverse_map_linear * borel[1] - 2.0 * strong_coupling * borel[0],
    ]
    if order == 2:
        mapped.append(
            2.0 * inverse_map_linear * borel[1]
            + inverse_map_linear**2 * borel[2]
            - 2.0
            * strong_coupling
            * inverse_map_linear
            * borel[1]
            + strong_coupling
            * (2.0 * strong_coupling - 1.0)
            * borel[0]
        )
    polynomial = sum(
        coefficient * conformal**power
        for power, coefficient in enumerate(mapped)
    )
    integrand = (
        (1.0 - conformal) ** (-2.0 * strong_coupling)
        * polynomial
    )
    return float(np.sum(weights * integrand))


def resummation_landscape(
    epsilon: float,
    *,
    borel_leroy_points: int = 101,
    strong_coupling_points: int = 81,
    borel_leroy_range: tuple[float, float] = (0.0, 5.0),
    strong_coupling_range: tuple[float, float] = (0.0, 2.0),
) -> dict[str, np.ndarray]:
    """Evaluate the two-loop surface and its one-loop predecessor."""
    borel_leroy = np.linspace(*borel_leroy_range, borel_leroy_points)
    strong_coupling = np.linspace(
        *strong_coupling_range,
        strong_coupling_points,
    )
    order_two = np.empty((borel_leroy_points, strong_coupling_points))
    order_one = np.empty_like(order_two)
    for row, parameter_b in enumerate(borel_leroy):
        nodes, weights = roots_genlaguerre(96, parameter_b)
        for column, parameter_lambda in enumerate(strong_coupling):
            order_two[row, column] = _conformal_borel_from_quadrature(
                epsilon,
                parameter_b,
                parameter_lambda,
                2,
                nodes,
                weights,
            )
            order_one[row, column] = _conformal_borel_from_quadrature(
                epsilon,
                parameter_b,
                parameter_lambda,
                1,
                nodes,
                weights,
            )

    derivative_b, derivative_lambda = np.gradient(
        order_two,
        borel_leroy,
        strong_coupling,
        edge_order=2,
    )
    b_width = borel_leroy_range[1] - borel_leroy_range[0]
    lambda_width = strong_coupling_range[1] - strong_coupling_range[0]
    stability_score = np.sqrt(
        (b_width * derivative_b) ** 2
        + (lambda_width * derivative_lambda) ** 2
        + (order_two - order_one) ** 2
    )
    return {
        "borel_leroy": borel_leroy,
        "strong_coupling": strong_coupling,
        "order_two": order_two,
        "order_one": order_one,
        "stability_score": stability_score,
    }


def resum_boundary_exponent(
    epsilon: float,
    *,
    selected_fraction: float = 0.10,
) -> tuple[BoundaryResummation, dict[str, np.ndarray]]:
    """Resum one dimension and attach a conservative two-loop error."""
    landscape = resummation_landscape(epsilon)
    score = landscape["stability_score"]
    threshold = float(np.quantile(score, selected_fraction))
    selected = landscape["order_two"][score <= threshold]
    quantiles = np.quantile(selected, (0.05, 0.16, 0.50, 0.84, 0.95))
    direct = float(
        BOUNDARY_SERIES[0]
        + BOUNDARY_SERIES[1] * epsilon
        + BOUNDARY_SERIES[2] * epsilon**2
    )
    central = float(quantiles[2])
    order_shift = abs(central - direct)
    stability_half_width = float((quantiles[4] - quantiles[0]) / 2.0)
    uncertainty = max(order_shift, stability_half_width)
    estimate = BoundaryResummation(
        epsilon=float(epsilon),
        dimension=4.0 - float(epsilon),
        direct_two_loop=direct,
        central=central,
        uncertainty=uncertainty,
        stability_interval_68=(float(quantiles[1]), float(quantiles[3])),
        stability_interval_90=(float(quantiles[0]), float(quantiles[4])),
        stability_minimum=float(np.min(selected)),
        stability_maximum=float(np.max(selected)),
        selected_fraction=selected_fraction,
        borel_leroy_range=(
            float(landscape["borel_leroy"][0]),
            float(landscape["borel_leroy"][-1]),
        ),
        strong_coupling_range=(
            float(landscape["strong_coupling"][0]),
            float(landscape["strong_coupling"][-1]),
        ),
        grid_shape=tuple(int(value) for value in score.shape),
        leading_borel_singularity=LEADING_BOREL_SINGULARITY,
        order_shift=order_shift,
        selected_count=int(selected.size),
    )
    return estimate, landscape
