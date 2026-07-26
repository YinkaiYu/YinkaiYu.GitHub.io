"""Boundary and replicated-boundary RG identities for imaginary-time dynamics."""

from __future__ import annotations

from dataclasses import dataclass
from math import sqrt

import numpy as np


@dataclass(frozen=True)
class Estimate:
    value: float
    uncertainty: float = 0.0

    def __add__(self, other: Estimate) -> Estimate:
        return Estimate(
            self.value + other.value,
            sqrt(self.uncertainty**2 + other.uncertainty**2),
        )

    def __sub__(self, other: Estimate) -> Estimate:
        return Estimate(
            self.value - other.value,
            sqrt(self.uncertainty**2 + other.uncertainty**2),
        )

    def scaled(self, factor: float) -> Estimate:
        return Estimate(self.value * factor, abs(factor) * self.uncertainty)


def bulk_field_dimension(classical_dimension: int, eta: Estimate) -> Estimate:
    """Delta_phi = (D - 2 + eta) / 2."""
    return Estimate(float(classical_dimension - 2), 0.0).__add__(eta).scaled(0.5)


def ordered_fixed_exponent(delta_phi: Estimate, dynamic_exponent: float = 1.0) -> Estimate:
    """Signed logarithmic slope for a uniformly ordered fixed boundary."""
    return delta_phi.scaled(-1.0 / dynamic_exponent)


def ordinary_initial_slip(
    surface_field_eigenvalue: Estimate,
    delta_phi: Estimate,
    dynamic_exponent: float = 1.0,
) -> Estimate:
    """theta_ord = (y_h1 - Delta_phi) / z."""
    return (surface_field_eigenvalue - delta_phi).scaled(1.0 / dynamic_exponent)


def random_amplitude_eigenvalue(
    classical_dimension: int, surface_field_eigenvalue: Estimate
) -> Estimate:
    """y_w = y_h1 - (D - 1)/2 for the rms random boundary field."""
    return surface_field_eigenvalue - Estimate((classical_dimension - 1) / 2.0)


def random_variance_correction_exponent(
    classical_dimension: int, surface_field_eigenvalue: Estimate
) -> Estimate:
    """Positive correction exponent omega_dis = -2 y_w when disorder is irrelevant."""
    return random_amplitude_eigenvalue(
        classical_dimension, surface_field_eigenvalue
    ).scaled(-2.0)


def marginal_random_coupling(rg_time, coupling_0: float = 1.0):
    """Dimensionless D=2 replica variance g(l)=g0/(1+g0*l).

    The nonuniversal coefficient kappa is absorbed into g.
    """
    rg_time = np.asarray(rg_time, dtype=float)
    return coupling_0 / (1.0 + coupling_0 * rg_time)


def marginal_random_response(
    imaginary_time, theta: float = 3.0 / 8.0, coupling_0: float = 1.0
):
    """Universal power and leading marginal logarithm for a random fixed boundary."""
    imaginary_time = np.asarray(imaginary_time, dtype=float)
    return imaginary_time**theta / np.sqrt(
        1.0 + coupling_0 * np.log(imaginary_time)
    )


def marginal_effective_exponent(
    imaginary_time, theta: float = 3.0 / 8.0, coupling_0: float = 1.0
):
    """d ln(response) / d ln(tau) for the D=2 marginal flow."""
    imaginary_time = np.asarray(imaginary_time, dtype=float)
    return theta - coupling_0 / (
        2.0 * (1.0 + coupling_0 * np.log(imaginary_time))
    )


def format_estimate(estimate: Estimate, digits: int = 4) -> str:
    if estimate.uncertainty == 0:
        return f"{estimate.value:.{digits}f}"
    return f"{estimate.value:.{digits}f} ± {estimate.uncertainty:.{digits}f}"
