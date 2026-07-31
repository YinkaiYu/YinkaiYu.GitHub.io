r"""Auditable formulas used in arXiv:2602.07818 and their scaling checks.

The target preprint uses ``epsilon = 4-d`` and ``delta = 2-sigma``.  Its
non-classical long-range wedge is ``0 < delta < epsilon/2``.  Keeping these
small parameters explicit is useful because the Sak crossover layer has
``delta = O(epsilon**2)``, whereas the paper expands along rays with
``delta = O(epsilon)``.
"""

from __future__ import annotations

from math import gamma


def long_range_epsilon(epsilon: float, delta: float) -> float:
    """Return the conventional LR expansion variable ``epsilon'=2 sigma-d``."""
    return float(epsilon - 2.0 * delta)


def paper_eta(epsilon: float, delta: float, components: float) -> float:
    r"""Return Eq. (3)/(41) of arXiv:2602.07818v2.

    This function reproduces the printed claim; it is not endorsed as a
    correctly projected physical anomalous dimension.
    """
    n = float(components)
    numerator = (n + 2.0) * (epsilon - 2.0 * delta) ** 3
    denominator = (n + 8.0) ** 2 * (2.0 * epsilon - 3.0 * delta)
    if denominator == 0.0:
        raise ZeroDivisionError("the printed expression is singular at 2 epsilon=3 delta")
    return float(delta + numerator / denominator)


def paper_inverse_nu(epsilon: float, delta: float, components: float) -> float:
    r"""Return the target paper's one-loop thermal eigenvalue ``1/nu``."""
    n = float(components)
    return float(
        2.0
        - delta
        - (n + 2.0) / (n + 8.0) * (epsilon - 2.0 * delta)
    )


def sr_eta_two_loop(epsilon: float, components: float) -> float:
    r"""Leading nonzero short-range :math:`O(N)` anomalous dimension.

    .. math::
       \eta_{\rm SR}=\frac{N+2}{2(N+8)^2}\epsilon^2+O(\epsilon^3).
    """
    n = float(components)
    return float((n + 2.0) * epsilon**2 / (2.0 * (n + 8.0) ** 2))


def sak_sigma_star_two_loop(epsilon: float, components: float) -> float:
    """Sak boundary obtained with the two-loop short-range expansion."""
    return float(2.0 - sr_eta_two_loop(epsilon, components))


def sunset_momentum_power(dimension: float, sigma: float) -> float:
    r"""Power of external momentum in the massless two-loop sunset integral.

    Dimensional analysis gives ``Sigma_s(p) proportional |p|^(2d-3sigma)``.
    """
    return float(2.0 * dimension - 3.0 * sigma)


def sunset_pole_coordinate(epsilon: float, delta: float) -> float:
    r"""Return ``rho=2 epsilon-3 delta`` near ``d=4, sigma=2``.

    The sunset is proportional to ``Gamma(-1+rho/2) p^(2-rho)``.  Its pole
    therefore multiplies the analytic local operator :math:`p^2`.
    """
    return float(2.0 * epsilon - 3.0 * delta)


def bootstrap_gamma_residue(rho: float) -> float:
    r"""Numerical residue associated with the target paper's Eq. (24).

    The limit is ``rho Gamma(-1+rho/2)/Gamma(3) -> -1``.  This exposes a
    factor-of-two mismatch between the exact gamma expression printed in
    Eq. (24) and the pole coefficient printed in Eq. (25).
    """
    return float(rho * gamma(-1.0 + rho / 2.0) / gamma(3.0))


def zfactor_gamma_residue(rho: float) -> float:
    r"""Numerical residue associated with the target paper's Eq. (37).

    The limit is ``rho Gamma(rho/2)/Gamma(3) -> +1``, again half the
    coefficient used in the following printed pole formula.
    """
    return float(rho * gamma(rho / 2.0) / gamma(3.0))


def analytic_leakage_into_fractional_projector(momentum: float, sigma: float) -> float:
    r"""Contamination of a fractional projector by a local ``p^2`` term.

    Dividing an analytic contribution by :math:`p^\sigma` produces
    :math:`p^{2-\sigma}`.  It vanishes in the defining ``p -> 0`` projector
    for every noninteger ``sigma < 2``, but is nonzero at a finite subtraction
    point and becomes non-uniform as ``sigma -> 2``.
    """
    if momentum <= 0.0:
        raise ValueError("momentum must be positive")
    return float(momentum ** (2.0 - sigma))


def in_target_long_range_wedge(epsilon: float, delta: float) -> bool:
    """Whether ``0 < delta < epsilon/2`` as assumed by the target paper."""
    return bool(epsilon > 0.0 and 0.0 < delta < epsilon / 2.0)
