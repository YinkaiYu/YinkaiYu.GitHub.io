"""Publication-quality figures for the long-range O(N) FRG report."""

from __future__ import annotations

from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np

from .paper_expansion import sr_eta_two_loop

NAVY = "#18324a"
BLUE = "#2c6e9b"
TEAL = "#21867a"
ORANGE = "#d87832"
RED = "#b34a4a"
GOLD = "#c9a227"
GRAY = "#6d7780"
LIGHT = "#e8edf0"


def _style() -> None:
    mpl.rcParams.update(
        {
            "figure.dpi": 130,
            "savefig.dpi": 240,
            "font.family": "DejaVu Sans",
            "font.size": 9.2,
            "axes.labelsize": 9.5,
            "axes.titlesize": 10.2,
            "legend.fontsize": 8.0,
            "xtick.labelsize": 8.5,
            "ytick.labelsize": 8.5,
            "axes.linewidth": 0.8,
            "axes.spines.top": False,
            "axes.spines.right": False,
            "axes.axisbelow": True,
            "axes.grid": True,
            "grid.alpha": 0.18,
            "grid.linewidth": 0.6,
            "lines.linewidth": 2.0,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
            "mathtext.fontset": "stix",
        }
    )


def _save(fig: plt.Figure, directory: Path, stem: str) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    fig.savefig(directory / f"{stem}.png", bbox_inches="tight", facecolor="white")
    fig.savefig(directory / f"{stem}.pdf", bbox_inches="tight", facecolor="white")
    plt.close(fig)


def _panel(ax: plt.Axes, label: str) -> None:
    ax.text(
        -0.12,
        1.03,
        label,
        transform=ax.transAxes,
        fontsize=10.5,
        fontweight="bold",
        va="bottom",
        ha="left",
        color=NAVY,
    )


def operator_projection_figure(directory: Path) -> None:
    """Show why a finite-momentum slope mixes p^sigma and p^2."""
    _style()
    fig, axes = plt.subplots(1, 2, figsize=(8.1, 3.05), constrained_layout=True)
    momenta = np.geomspace(1.0e-6, 1.0, 500)
    sigma = 1.8
    axes[0].loglog(momenta, momenta**sigma, color=BLUE, label=rf"nonanalytic $p^{{{sigma}}}$")
    axes[0].loglog(momenta, 0.35 * momenta**2, color=ORANGE, label=r"analytic $0.35p^2$")
    axes[0].loglog(
        momenta,
        momenta**sigma + 0.35 * momenta**2,
        color=NAVY,
        linestyle="--",
        label="sum",
    )
    axes[0].set(xlabel=r"momentum $p$", ylabel=r"inverse-propagator contribution")
    axes[0].legend(frameon=False, loc="upper left")
    _panel(axes[0], "a")

    for current_sigma, color in zip((1.5, 1.8, 1.95), (TEAL, BLUE, RED), strict=True):
        axes[1].loglog(
            momenta,
            momenta ** (2.0 - current_sigma),
            color=color,
            label=rf"$\sigma={current_sigma:g}$",
        )
    axes[1].axvline(1.0e-2, color=GRAY, linestyle=":", linewidth=1.2)
    axes[1].text(1.25e-2, 1.4e-1, r"finite subtraction point", color=GRAY, fontsize=7.8)
    axes[1].set(
        xlabel=r"momentum $p$",
        ylabel=r"$p^2/p^\sigma=p^{2-\sigma}$",
        ylim=(7e-4, 1.4),
    )
    axes[1].legend(frameon=False, loc="lower right")
    _panel(axes[1], "b")
    _save(fig, directory, "operator_projection")


def fixed_point_exchange_figure(results: dict, directory: Path) -> None:
    """Plot the exact long-range kinetic eigenvalue at the SR fixed point."""
    _style()
    summaries = results["short_range"]["summaries"]
    selected = [row for row in summaries if row["components"] == 1 and row["dimension"] in (3.0, 3.8)]
    fig, axes = plt.subplots(1, len(selected), figsize=(8.1, 3.05), constrained_layout=True)
    axes = np.atleast_1d(axes)
    for index, (ax, row) in enumerate(zip(axes, selected, strict=True)):
        eta = row["eta"]
        star = 2.0 - eta
        sigmas = np.linspace(max(row["dimension"] / 2.0, star - 0.28), 2.04, 400)
        eigenvalue = star - sigmas
        ax.plot(sigmas, eigenvalue, color=BLUE, label=r"FRG: $y_{\rm LR}$")
        ax.axhline(0.0, color=NAVY, linewidth=0.9)
        ax.axvline(star, color=TEAL, linestyle="--", label=rf"$\sigma_*={star:.4f}$")
        ax.axvline(2.0, color=RED, linestyle=":", label=r"paper: $\sigma_*=2$")
        ax.axvspan(star, 2.0, color=ORANGE, alpha=0.13)
        ax.set(
            xlabel=r"decay exponent $\sigma$",
            ylabel=r"SR stability exponent $y_{\rm LR}$",
            title=rf"$d={row['dimension']:g}$, $N=1$",
        )
        ax.legend(frameon=False, loc="lower left")
        _panel(ax, chr(ord("a") + index))
    _save(fig, directory, "fixed_point_exchange")


def eta_comparison_figure(results: dict, directory: Path) -> None:
    """Compare the protected FRG eta with the target paper's formula."""
    _style()
    rows = results["comparison"]
    dimensions = sorted({row["dimension"] for row in rows})
    fig, axes = plt.subplots(
        1, len(dimensions), figsize=(3.75 * len(dimensions), 3.15), constrained_layout=True
    )
    axes = np.atleast_1d(axes)
    for index, (ax, dimension) in enumerate(zip(axes, dimensions, strict=True)):
        block = [row for row in rows if row["dimension"] == dimension]
        sigma = np.asarray([row["sigma"] for row in block])
        frg = np.asarray([row["frg_eta"] for row in block])
        paper = np.asarray([row["paper_eta"] for row in block])
        star = block[0]["sigma_star"]
        ax.plot(sigma, frg, color=BLUE, label=r"competitive FRG")
        ax.plot(sigma, paper, color=RED, linestyle="--", label=r"arXiv:2602.07818")
        ax.plot(sigma, 2.0 - sigma, color=TEAL, linestyle=":", label=r"protected $2-\sigma$")
        ax.axvline(star, color=NAVY, linewidth=1.0, alpha=0.8)
        ax.axvspan(star, 2.0, color=ORANGE, alpha=0.11)
        ax.set(
            xlabel=r"decay exponent $\sigma$",
            ylabel=r"anomalous dimension $\eta$",
            title=rf"$d={dimension:g}$, $N=1$",
        )
        ax.legend(frameon=False, loc="upper left")
        _panel(ax, chr(ord("a") + index))
    _save(fig, directory, "eta_comparison")


def nu_comparison_figure(results: dict, directory: Path) -> None:
    """Compare direct competitive-FRG nu with the leading epsilon expansion."""
    _style()
    branches = results["long_range"]["branches"]
    dimensions = sorted({branch["dimension"] for branch in branches})
    fig, axes = plt.subplots(
        1, len(dimensions), figsize=(3.75 * len(dimensions), 3.15), constrained_layout=True
    )
    axes = np.atleast_1d(axes)
    for index, (ax, dimension) in enumerate(zip(axes, dimensions, strict=True)):
        branch = next(item for item in branches if item["dimension"] == dimension and item["components"] == 1)
        points = branch["points"]
        sigma = np.asarray([point["sigma"] for point in points])
        nu = np.asarray([point["nu"] for point in points])
        paper_nu = np.asarray([point["paper_nu"] for point in points])
        ax.plot(sigma, nu, marker="o", markersize=3.5, color=BLUE, label=r"direct LPA$''$")
        ax.plot(sigma, paper_nu, color=RED, linestyle="--", label=r"one-loop $4-\epsilon$")
        ax.axvline(branch["sigma_star"], color=TEAL, linestyle=":", linewidth=1.3)
        ax.set(
            xlabel=r"decay exponent $\sigma$",
            ylabel=r"correlation-length exponent $\nu$",
            title=rf"$d={dimension:g}$, $N=1$",
        )
        ax.legend(frameon=False)
        _panel(ax, chr(ord("a") + index))
    _save(fig, directory, "nu_comparison")


def convergence_figure(results: dict, directory: Path) -> None:
    """Visualize potential-order convergence and rejected d=2 roots."""
    _style()
    rows = results["short_range"]["convergence"]
    fig, axes = plt.subplots(1, 2, figsize=(8.1, 3.1), constrained_layout=True)
    for dimension, color, marker in ((2.0, ORANGE, "s"), (3.0, BLUE, "o")):
        block = [row for row in rows if row["dimension"] == dimension and row["components"] == 1]
        accepted = [row for row in block if row["accepted"]]
        rejected = [
            row
            for row in block
            if not row["accepted"] and row.get("eta") is not None and row.get("nu") is not None
        ]
        if accepted:
            order = [row["order"] for row in accepted]
            axes[0].plot(order, [row["eta"] for row in accepted], marker=marker, color=color, label=rf"$d={dimension:g}$")
            axes[1].plot(order, [row["nu"] for row in accepted], marker=marker, color=color, label=rf"$d={dimension:g}$")
        if rejected:
            axes[0].scatter(
                [row["order"] for row in rejected],
                [row["eta"] for row in rejected],
                marker="x",
                color=color,
                alpha=0.75,
                label=rf"$d={dimension:g}$ rejected",
            )
            axes[1].scatter(
                [row["order"] for row in rejected],
                [row["nu"] for row in rejected],
                marker="x",
                color=color,
                alpha=0.75,
            )
    axes[0].set(xlabel="potential order $M$", ylabel=r"$\eta_{\rm SR}$")
    axes[1].set(xlabel="potential order $M$", ylabel=r"$\nu_{\rm SR}$")
    axes[0].legend(frameon=False, ncol=1)
    for index, ax in enumerate(axes):
        ax.xaxis.set_major_locator(mpl.ticker.MaxNLocator(integer=True))
        _panel(ax, chr(ord("a") + index))
    _save(fig, directory, "potential_convergence")


def branch_structure_figure(results: dict, directory: Path) -> None:
    """Show the LR coordinate and the residual of its imposed branch equation."""
    _style()
    branches = results["long_range"]["branches"]
    fig, axes = plt.subplots(1, 2, figsize=(8.1, 3.15), constrained_layout=True)
    for branch, color in zip(branches, (ORANGE, BLUE, TEAL, RED), strict=False):
        if branch["components"] != 1:
            continue
        points = branch["points"]
        sigma = np.asarray([point["sigma"] for point in points])
        label = rf"$d={branch['dimension']:g}$"
        axes[0].plot(sigma, [point["j_star"] for point in points], marker="o", markersize=3, color=color, label=label)
        axes[1].scatter(
            sigma,
            [
                max(point["imposed_eta_relation_residual"], 1.0e-18)
                for point in points
            ],
            marker="o",
            s=16,
            color=color,
            label=label,
        )
    axes[0].set(
        xlabel=r"decay exponent $\sigma$",
        ylabel=r"LR coordinate $j_*$",
        yscale="log",
    )
    axes[1].set(
        xlabel=r"decay exponent $\sigma$",
        ylabel="imposed equation residual",
        yscale="log",
    )
    axes[0].legend(frameon=False)
    axes[1].legend(frameon=False)
    for index, ax in enumerate(axes):
        _panel(ax, chr(ord("a") + index))
    _save(fig, directory, "lr_branch_structure")


def boundary_layer_figure(directory: Path, components: int = 1) -> None:
    """Contrast O(epsilon^2) Sak layer with O(epsilon) rays."""
    _style()
    epsilon = np.linspace(0.002, 1.0, 500)
    delta_star = np.asarray([sr_eta_two_loop(value, components) for value in epsilon])
    fig, axes = plt.subplots(1, 2, figsize=(8.1, 3.05), constrained_layout=True)
    axes[0].plot(epsilon, delta_star, color=BLUE, label=r"Sak layer $\delta_*=\eta_{\rm SR}$")
    axes[0].plot(epsilon, 0.1 * epsilon, color=ORANGE, linestyle="--", label=r"ray $\delta=0.1\epsilon$")
    axes[0].plot(epsilon, 0.25 * epsilon, color=RED, linestyle="--", label=r"ray $\delta=0.25\epsilon$")
    axes[0].fill_between(epsilon, 0.0, 0.5 * epsilon, color=LIGHT, alpha=0.7, label="paper wedge")
    axes[0].set(xlabel=r"$\epsilon=4-d$", ylabel=r"$\delta=2-\sigma$", xlim=(0, 1), ylim=(0, 0.3))
    axes[0].legend(frameon=False, loc="upper left")
    _panel(axes[0], "a")

    axes[1].plot(epsilon, delta_star / epsilon, color=BLUE)
    axes[1].axhline(0.1, color=ORANGE, linestyle="--")
    axes[1].axhline(0.25, color=RED, linestyle="--")
    axes[1].set(
        xlabel=r"$\epsilon=4-d$",
        ylabel=r"ratio $\delta/\epsilon$",
        xlim=(0, 1),
        ylim=(0, 0.28),
    )
    _panel(axes[1], "b")
    _save(fig, directory, "epsilon_boundary_layer")


def generate_all_figures(results: dict, directory: Path) -> None:
    operator_projection_figure(directory)
    fixed_point_exchange_figure(results, directory)
    eta_comparison_figure(results, directory)
    nu_comparison_figure(results, directory)
    convergence_figure(results, directory)
    branch_structure_figure(results, directory)
    boundary_layer_figure(directory)
