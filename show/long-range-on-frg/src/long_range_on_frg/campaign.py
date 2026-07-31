"""Reproducible numerical campaign for the public long-range FRG report."""

from __future__ import annotations

import json
import platform
from datetime import UTC, datetime
from pathlib import Path

import numpy as np
import scipy

from .competitive import FixedPoint, solve_lr_fixed_point, solve_sr_fixed_point
from .paper_expansion import (
    bootstrap_gamma_residue,
    paper_eta,
    paper_inverse_nu,
    sunset_pole_coordinate,
    zfactor_gamma_residue,
)


def _complex_rows(values: np.ndarray) -> list[dict[str, float]]:
    return [
        {"real": float(value.real), "imag": float(value.imag)}
        for value in sorted(values, key=lambda item: (item.real, item.imag))
    ]


def fixed_point_record(fixed: FixedPoint) -> dict:
    """Convert a fixed-point result into strict-JSON-compatible primitives."""
    return {
        "dimension": fixed.config.d,
        "components": fixed.config.n_components,
        "order": fixed.config.order,
        "sigma": fixed.sigma,
        "branch": fixed.branch,
        "kappa": fixed.kappa,
        "lambdas": [float(value) for value in fixed.lambdas],
        "j_star": fixed.j,
        "eta": fixed.eta,
        "nu": fixed.nu,
        "thermal_lambda": fixed.thermal_lambda,
        "kinetic_lambda": fixed.kinetic_lambda,
        "kinetic_y": fixed.kinetic_y,
        "kinetic_mode_identification": (
            "exact triangular j direction"
            if fixed.branch == "SR"
            else "local smallest positive real mode near the crossover"
        ),
        "normalized_residual": fixed.residual,
        "raw_derivative_residual": fixed.raw_residual,
        "stability_eigenvalues": _complex_rows(fixed.stability_eigenvalues),
    }


def _sr_sequence(dimension: float, components: int, maximum_order: int) -> tuple[list[dict], FixedPoint]:
    rows: list[dict] = []
    last: FixedPoint | None = None
    for order in range(2, maximum_order + 1):
        try:
            fixed = solve_sr_fixed_point(dimension, components, order, sigma=1.9)
        except RuntimeError as error:
            rows.append(
                {
                    "dimension": dimension,
                    "components": components,
                    "order": order,
                    "accepted": False,
                    "eta": None,
                    "nu": None,
                    "sigma_star": None,
                    "reason": str(error),
                }
            )
            continue
        record = fixed_point_record(fixed)
        record.update({"accepted": True, "sigma_star": fixed.sigma_star})
        rows.append(record)
        last = fixed
    if last is None:
        raise RuntimeError(f"no accepted SR result for d={dimension}, N={components}")
    return rows, last


def _lr_branch(
    dimension: float,
    components: int,
    order: int,
    sigmas: list[float],
) -> dict:
    points: list[dict] = []
    sigma_star = solve_sr_fixed_point(dimension, components, order, sigma=1.9).sigma_star
    for sigma in sigmas:
        fixed = solve_lr_fixed_point(
            dimension,
            components,
            order,
            sigma,
            continuation_steps=20,
        )
        record = fixed_point_record(fixed)
        inverse_nu = paper_inverse_nu(4.0 - dimension, 2.0 - sigma, components)
        record.update(
            {
                "accepted": True,
                "paper_inverse_nu": inverse_nu,
                "paper_nu": 1.0 / inverse_nu,
                "imposed_eta_relation_residual": abs(fixed.eta - (2.0 - sigma)),
            }
        )
        points.append(record)
    return {
        "dimension": dimension,
        "components": components,
        "order": order,
        "sigma_star": sigma_star,
        "points": points,
    }


def _comparison_grid(summaries: list[dict]) -> list[dict]:
    rows: list[dict] = []
    for summary in summaries:
        dimension = summary["dimension"]
        if dimension not in (3.0, 3.8) or summary["components"] != 1:
            continue
        epsilon = 4.0 - dimension
        lower = dimension / 2.0 + max(0.002, 0.015 * epsilon)
        for sigma in np.linspace(lower, 2.0, 181):
            delta = 2.0 - float(sigma)
            star = summary["sigma_star"]
            rows.append(
                {
                    "dimension": dimension,
                    "components": 1,
                    "sigma": float(sigma),
                    "epsilon": epsilon,
                    "delta": delta,
                    "sigma_star": star,
                    "frg_eta": 2.0 - float(sigma) if sigma < star else summary["eta"],
                    "paper_eta": paper_eta(epsilon, delta, 1),
                    "paper_inverse_nu": paper_inverse_nu(epsilon, delta, 1),
                }
            )
    return rows


def run_campaign() -> dict:
    """Run all accepted and deliberately rejected numerical checks."""
    convergence: list[dict] = []
    summaries: list[dict] = []

    # d=2 is retained as an honest low-order diagnostic.  M>=3 is rejected
    # by the one-relevant-direction gate, rather than reported as convergence.
    d2_rows, d2 = _sr_sequence(2.0, 1, 5)
    convergence.extend(d2_rows)
    summaries.append(
        {
            **fixed_point_record(d2),
            "sigma_star": d2.sigma_star,
            "status": "low_order_diagnostic_only",
            "systematic_note": "minimum Taylor expansion fails the physical-root gate from M=3",
        }
    )

    for dimension in (3.0, 3.8):
        rows, last = _sr_sequence(dimension, 1, 8)
        convergence.extend(rows)
        accepted = [row for row in rows if row["accepted"]]
        previous = accepted[-2]
        summaries.append(
            {
                **fixed_point_record(last),
                "sigma_star": last.sigma_star,
                "status": "accepted_M8",
                "M7_to_M8_eta_shift": abs(last.eta - previous["eta"]),
                "M7_to_M8_nu_shift": abs(last.nu - previous["nu"]),
                "systematic_note": "potential-order convergence only; derivative-expansion error remains separate",
            }
        )

    # General-O(N) SR checks in d=3.  The long-range branch scan below remains
    # focused on N=1, which is also the main numerical case in the controversy.
    for components in (2, 3):
        rows, last = _sr_sequence(3.0, components, 8)
        convergence.extend(rows)
        accepted = [row for row in rows if row["accepted"]]
        summaries.append(
            {
                **fixed_point_record(last),
                "sigma_star": last.sigma_star,
                "status": "accepted_M8",
                "M7_to_M8_eta_shift": abs(last.eta - accepted[-2]["eta"]),
                "M7_to_M8_nu_shift": abs(last.nu - accepted[-2]["nu"]),
                "systematic_note": "potential-order convergence only; derivative-expansion error remains separate",
            }
        )

    summary_lookup = {
        (row["dimension"], row["components"]): row for row in summaries
    }
    star2 = summary_lookup[(2.0, 1)]["sigma_star"]
    branches = [
        _lr_branch(2.0, 1, 2, [star2 - gap for gap in (0.006, 0.02, 0.05, 0.10, 0.15)]),
        # M=6 is the highest order that passes the LR continuation gates at
        # all three near-crossover points.  The SR endpoint is separately
        # determined at M=8; the order mismatch is retained in the metadata.
        _lr_branch(3.0, 1, 6, [
            solve_sr_fixed_point(3.0, 1, 6, sigma=1.9).sigma_star - gap
            for gap in (0.003, 0.01, 0.025)
        ]),
        # Close to d=4 the branch coordinate is nearly marginal and the M=6/8
        # nonlinear root becomes ill-conditioned.  M=4 remains stable and is
        # reported as such instead of weakening the residual gate.
        _lr_branch(3.8, 1, 4, [
            solve_sr_fixed_point(3.8, 1, 4, sigma=1.9).sigma_star - gap
            for gap in (0.001, 0.003, 0.008)
        ]),
    ]

    rho_values = [10.0 ** (-power) for power in range(2, 8)]
    pole_audit = {
        "rho_scan": [
            {
                "rho": rho,
                "bootstrap_residue": bootstrap_gamma_residue(rho),
                "zfactor_residue": zfactor_gamma_residue(rho),
            }
            for rho in rho_values
        ],
        "wedge_examples": [
            {
                "epsilon": epsilon,
                "delta": fraction * epsilon,
                "fraction": fraction,
                "rho": sunset_pole_coordinate(epsilon, fraction * epsilon),
            }
            for epsilon in (0.05, 0.2, 1.0)
            for fraction in (0.1, 0.25, 0.49)
        ],
        "limiting_residues": {"bootstrap": -1.0, "zfactor": 1.0},
    }

    return {
        "schema_version": "1.0",
        "generated_at": datetime.now(UTC).isoformat(),
        "environment": {
            "python": platform.python_version(),
            "numpy": np.__version__,
            "scipy": scipy.__version__,
            "platform": platform.platform(),
        },
        "method": {
            "name": "competitive two-kinetic-operator LPA-double-prime",
            "regulator": "mixed optimized cutoff",
            "potential_representation": "Taylor jets about the running minimum",
            "accepted_maximum_order": 8,
            "exact_gates": [
                "partial_t Z_sigma = 0",
                "beta_j = (sigma-2+eta_2) j",
                "SR y_LR = 2-eta_SR-sigma",
                "LR eta_2 = 2-sigma",
            ],
        },
        "short_range": {"convergence": convergence, "summaries": summaries},
        "long_range": {"branches": branches},
        "comparison": _comparison_grid(summaries),
        "pole_audit": pole_audit,
        "external_benchmarks": [
            {
                "dimension": 2.0,
                "components": 1,
                "method": "exact 2D Ising CFT",
                "eta": 0.25,
                "nu": 1.0,
                "sigma_star": 1.75,
                "source_id": "wu_mccoy_tracy_barouch_1976",
            },
            {
                "dimension": 3.0,
                "components": 1,
                "method": "published short-range FRG O(partial^6)",
                "eta": 0.0361,
                "eta_uncertainty": 0.0011,
                "nu": 0.63012,
                "nu_uncertainty": 0.00016,
                "sigma_star": 1.9639,
                "sigma_star_uncertainty": 0.0011,
                "source_id": "balog_chate_delamotte_marohnic_wschebor_2019",
                "tabulated_source_id": "de_polsi_balog_tissier_wschebor_2020",
            },
            {
                "dimension": 3.0,
                "components": 1,
                "method": "conformal bootstrap benchmark",
                "eta": 0.0362978,
                "eta_uncertainty": 0.0000020,
                "nu": 0.629971,
                "nu_uncertainty": 0.000004,
                "sigma_star": 1.9637022,
                "sigma_star_uncertainty": 0.0000020,
                "source_id": "kos_poland_simmons_duffin_2014",
                "tabulated_source_id": "de_polsi_balog_tissier_wschebor_2020",
            },
        ],
        "quality_statement": {
            "structural_result": "operator protection and the crossover eigenvalue do not depend on potential order",
            "numerical_result": "M-convergence controls only the potential polynomial; field-independent wave-function and regulator truncations remain",
            "d2": "M2 is diagnostic only; exact Ising data are shown separately and are not used to tune the solver",
            "d3": "M8 is the accepted direct result; published higher-derivative FRG is listed as an external benchmark",
        },
    }


def write_campaign(path: Path) -> dict:
    results = run_campaign()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(results, ensure_ascii=False, indent=2, allow_nan=False) + "\n")
    return results


__all__ = ["fixed_point_record", "run_campaign", "write_campaign"]
