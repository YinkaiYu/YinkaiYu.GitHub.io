"""Publication-quality figures for the FRG report."""

from __future__ import annotations

from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np

from .boundary_rg import marginal_effective_exponent, marginal_random_coupling

COLORS = {
    "ink": "#17212B",
    "muted": "#607080",
    "fixed": "#D95F4B",
    "ordinary": "#167D8D",
    "random": "#7056A8",
    "accent": "#E5A43B",
    "pale": "#EAF0F2",
}


def set_paper_style() -> None:
    mpl.rcParams.update(
        {
            "font.family": "sans-serif",
            "font.sans-serif": ["Noto Sans CJK SC", "DejaVu Sans"],
            "font.size": 10,
            "axes.labelsize": 10,
            "axes.titlesize": 10,
            "legend.fontsize": 8.5,
            "xtick.labelsize": 8.5,
            "ytick.labelsize": 8.5,
            "axes.linewidth": 0.8,
            "axes.edgecolor": COLORS["ink"],
            "axes.labelcolor": COLORS["ink"],
            "xtick.color": COLORS["ink"],
            "ytick.color": COLORS["ink"],
            "xtick.direction": "in",
            "ytick.direction": "in",
            "xtick.top": True,
            "ytick.right": True,
            "legend.frameon": False,
            "mathtext.fontset": "dejavusans",
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
            "savefig.bbox": "tight",
        }
    )


def _panel_label(axis, label: str) -> None:
    axis.text(
        0.02,
        0.97,
        label,
        transform=axis.transAxes,
        ha="left",
        va="top",
        fontweight="bold",
        color=COLORS["ink"],
    )


def _save(figure, output_directory: Path, stem: str) -> None:
    output_directory.mkdir(parents=True, exist_ok=True)
    figure.savefig(output_directory / f"{stem}.pdf")
    figure.savefig(output_directory / f"{stem}.png", dpi=320)
    plt.close(figure)


def boundary_classes_figure(output_directory: Path) -> None:
    set_paper_style()
    figure, axes = plt.subplots(1, 3, figsize=(9.0, 3.05), sharey=True)
    rng = np.random.default_rng(20260726)
    x = np.arange(11)
    classes = [
        ("严格 (+) fixed", COLORS["fixed"]),
        ("ordinary", COLORS["ordinary"]),
        ("随机 +/− fixed", COLORS["random"]),
    ]

    for index, (axis, (title, color)) in enumerate(zip(axes, classes, strict=True)):
        axis.set_xlim(-0.8, 10.8)
        axis.set_ylim(-0.15, 4.3)
        axis.axhline(0, color=COLORS["ink"], linewidth=1.1)
        axis.set_title(title, pad=7, fontweight="medium")
        axis.set_xlabel("空间位置  $x$")
        axis.set_xticks([0, 5, 10])
        axis.set_yticks([0, 1, 2, 3, 4])
        axis.tick_params(top=False)
        axis.text(
            -0.035,
            1.015,
            chr(ord("a") + index),
            transform=axis.transAxes,
            ha="right",
            va="bottom",
            fontweight="bold",
            color=COLORS["ink"],
        )

        if index == 0:
            spins = np.ones_like(x)
            axis.scatter(x, np.zeros_like(x), s=38, color=color, zorder=4)
            for xpos, spin in zip(x, spins, strict=True):
                axis.text(xpos, 0.08, "+", ha="center", va="bottom", color=color, fontsize=8)
            widths = [0.04, 0.09, 0.17, 0.27]
            for height, width in enumerate(widths, start=1):
                y = height
                profile = 0.52 + 0.36 * np.cos((x - 5) * width)
                axis.scatter(x, np.full_like(x, y), s=17, color=color, alpha=profile)
        elif index == 1:
            axis.scatter(
                x,
                np.zeros_like(x),
                s=38,
                facecolors="white",
                edgecolors=color,
                linewidth=1.2,
                zorder=4,
            )
            axis.text(
                5,
                0.20,
                r"$h_1=0$，保持 $\mathbb{Z}_2$ 对称",
                ha="center",
                va="bottom",
                color=color,
                fontsize=8,
            )
            for height in range(1, 5):
                signs = rng.choice([-1, 1], size=len(x))
                axis.scatter(
                    x,
                    np.full_like(x, height),
                    s=17,
                    c=np.where(signs > 0, color, COLORS["pale"]),
                    edgecolors=color,
                    linewidth=0.4,
                    alpha=0.85,
                )
        else:
            signs = np.array([1, -1, 1, 1, -1, -1, 1, -1, 1, -1, 1])
            axis.scatter(
                x,
                np.zeros_like(x),
                s=38,
                c=np.where(signs > 0, color, "white"),
                edgecolors=color,
                linewidth=1.1,
                zorder=4,
            )
            for xpos, sign in zip(x, signs, strict=True):
                axis.text(
                    xpos,
                    0.08,
                    "+" if sign > 0 else "−",
                    ha="center",
                    va="bottom",
                    color=color,
                    fontsize=8,
                )
            for height in range(1, 5):
                block = max(1, height)
                coarse = np.convolve(signs, np.ones(block) / block, mode="same")
                axis.scatter(
                    x,
                    np.full_like(x, height),
                    s=17,
                    c=coarse,
                    cmap="Purples",
                    vmin=-1,
                    vmax=1,
                    edgecolors=color,
                    linewidth=0.35,
                )

        axis.annotate(
            "",
            xy=(10.3, 3.8),
            xytext=(10.3, 0.55),
            arrowprops={"arrowstyle": "->", "color": COLORS["muted"], "lw": 0.8},
        )

    axes[0].set_ylabel("虚时间  $\\tau$")
    figure.subplots_adjust(wspace=0.12)
    _save(figure, output_directory, "boundary_classes")


def convergence_figure(results: dict, output_directory: Path) -> None:
    set_paper_style()
    data = results["bulk_lpa_prime_convergence_D3"]
    orders = np.array([row["order"] for row in data])
    eta = np.array([row["eta"] for row in data])
    theta_fixed = -(1.0 + eta) / 2.0

    figure, axes = plt.subplots(1, 2, figsize=(7.2, 3.1))
    panels = [
        (axes[0], eta, r"异常维数  $\eta$", 0.0361, 0.0011, 0.0362978),
        (
            axes[1],
            theta_fixed,
            r"有序 fixed 的斜率  $\theta_{+}$",
            -0.5181,
            0.0006,
            -0.5181489,
        ),
    ]
    for index, (axis, values, ylabel, center, error, benchmark) in enumerate(panels):
        axis.fill_between(
            [orders.min() - 0.2, orders.max() + 0.2],
            center - error,
            center + error,
            color=COLORS["ordinary"],
            alpha=0.16,
            linewidth=0,
            label=r"高阶 NPRG $O(\partial^6)$",
        )
        axis.axhline(
            benchmark,
            color=COLORS["ink"],
            linestyle=(0, (3, 2)),
            linewidth=1.0,
            label="高精度基准",
        )
        axis.plot(
            orders,
            values,
            color=COLORS["fixed"],
            marker="o",
            markersize=4.5,
            linewidth=1.3,
            label="本仓库 LPA′",
        )
        axis.set_xlabel("势能多项式最高阶  $N$")
        axis.set_ylabel(ylabel)
        axis.set_xticks(orders)
        axis.set_xlim(1.8, 7.2)
        _panel_label(axis, chr(ord("a") + index))
    axes[0].legend(loc="upper right")
    _save(figure, output_directory, "frg_convergence")


def exponent_comparison_figure(results: dict, output_directory: Path) -> None:
    set_paper_style()
    figure, axes = plt.subplots(1, 2, figsize=(7.8, 3.35), sharey=True)
    labels = ["严格 (+)\nfixed", "ordinary", "随机 +/−\nfixed"]
    markers = ["o", "s", "D"]
    colors = [COLORS["fixed"], COLORS["ordinary"], COLORS["random"]]

    for panel, quantum_dimension in enumerate((1, 2)):
        axis = axes[panel]
        dimension = results["dimensions"][str(quantum_dimension)]
        estimates = [
            dimension["theta_fixed_plus"],
            dimension["theta_ordinary"],
            dimension["theta_random_fixed"],
        ]
        for x, (estimate, marker, color) in enumerate(
            zip(estimates, markers, colors, strict=True)
        ):
            axis.errorbar(
                x,
                estimate["value"],
                yerr=estimate["uncertainty"],
                marker=marker,
                markersize=7,
                color=color,
                markerfacecolor="white" if x == 2 else color,
                markeredgewidth=1.3,
                capsize=3,
                linewidth=1.1,
            )
            axis.text(
                x,
                estimate["value"] + (0.045 if estimate["value"] >= 0 else -0.055),
                f'{estimate["value"]:+.4f}',
                ha="center",
                va="center",
                color=color,
                fontsize=8.5,
            )
        axis.axhline(0, color=COLORS["muted"], linewidth=0.8)
        axis.set_xticks(range(3), labels)
        axis.set_xlim(-0.55, 2.55)
        axis.set_title(f"{quantum_dimension}D 量子 Ising", pad=7)
        _panel_label(axis, chr(ord("a") + panel))
        if quantum_dimension == 1:
            axis.text(
                2,
                0.19,
                r"同幂律，另有  $[\ln\tau]^{-1/2}$",
                ha="center",
                color=COLORS["random"],
                fontsize=7.7,
            )
        else:
            omega = dimension["random_variance_correction_exponent"]["value"]
            axis.text(
                2,
                0.02,
                rf"修正  $\tau^{{-{omega:.4f}}}$",
                ha="center",
                color=COLORS["random"],
                fontsize=7.7,
            )
    axes[0].set_ylabel(r"临界短时对数斜率  $\theta$")
    axes[0].set_ylim(-0.65, 0.55)
    _save(figure, output_directory, "exponent_comparison")


def disorder_flow_figure(results: dict, output_directory: Path) -> None:
    set_paper_style()
    figure, axes = plt.subplots(1, 2, figsize=(7.7, 3.2))
    ell = np.linspace(0, 12, 400)

    axis = axes[0]
    for coupling, color in zip((0.25, 1.0, 4.0), ("#9C8AC5", COLORS["random"], "#4D347C")):
        axis.plot(
            ell,
            marginal_random_coupling(ell, coupling),
            color=color,
            linewidth=1.3,
            label=rf"$g_0={coupling:g}$",
        )
    axis.set_xlabel(r"RG “时间”  $\ell=\ln b$")
    axis.set_ylabel(r"复制子方差耦合  $g(\ell)$")
    axis.set_title("1D 量子：边缘无关", pad=7)
    axis.legend(loc="upper right")
    _panel_label(axis, "a")
    inset = axis.inset_axes([0.47, 0.42, 0.48, 0.42])
    tau = np.exp(np.linspace(0.15, 12, 300))
    inset.plot(
        np.log(tau),
        marginal_effective_exponent(tau),
        color=COLORS["fixed"],
        linewidth=1.1,
    )
    inset.axhline(0.375, color=COLORS["ink"], linewidth=0.7, linestyle=(0, (3, 2)))
    inset.set_xlabel(r"$\ln\tau$", fontsize=7)
    inset.set_ylabel(r"$\theta_{\rm eff}$", fontsize=7)
    inset.tick_params(labelsize=6.5)

    axis = axes[1]
    y_w = results["dimensions"]["2"]["random_amplitude_eigenvalue"]["value"]
    for amplitude, color in zip((0.5, 1.0, 2.0), ("#9C8AC5", COLORS["random"], "#4D347C")):
        axis.semilogy(
            ell,
            amplitude * np.exp(y_w * ell),
            color=color,
            linewidth=1.3,
            label=rf"$w_0={amplitude:g}$",
        )
    axis.set_xlabel(r"RG “时间”  $\ell=\ln b$")
    axis.set_ylabel(r"随机场均方根  $w(\ell)$")
    axis.set_title("2D 量子：无关", pad=7)
    axis.legend(loc="upper right")
    axis.text(
        0.96,
        0.12,
        rf"$y_w={y_w:.4f}$",
        transform=axis.transAxes,
        ha="right",
        color=COLORS["random"],
    )
    _panel_label(axis, "b")
    _save(figure, output_directory, "disorder_rg_flow")


def scaling_curves_figure(results: dict, output_directory: Path) -> None:
    set_paper_style()
    figure, axes = plt.subplots(1, 2, figsize=(7.8, 3.25), sharey=True)
    tau = np.geomspace(1.0, 1e4, 400)
    for panel, quantum_dimension in enumerate((1, 2)):
        axis = axes[panel]
        dimension = results["dimensions"][str(quantum_dimension)]
        fixed = dimension["theta_fixed_plus"]["value"]
        ordinary = dimension["theta_ordinary"]["value"]
        fixed_curve = tau**fixed
        ordinary_curve = tau**ordinary
        if quantum_dimension == 1:
            random_curve = tau**ordinary / np.sqrt(1.0 + 0.6 * np.log(tau))
        else:
            omega = dimension["random_variance_correction_exponent"]["value"]
            random_curve = tau**ordinary * (1.0 + 0.6 * tau ** (-omega)) / 1.6
        axis.loglog(tau, fixed_curve, color=COLORS["fixed"], lw=1.5, label="严格 (+) fixed")
        axis.loglog(
            tau, ordinary_curve, color=COLORS["ordinary"], lw=1.5, label="ordinary 响应"
        )
        axis.loglog(
            tau,
            random_curve,
            color=COLORS["random"],
            lw=1.5,
            linestyle=(0, (4, 2)),
            label="随机 fixed 响应",
        )
        axis.set_xlabel(r"虚时间  $\tau/\tau_{\rm mic}$")
        axis.set_title(f"{quantum_dimension}D 量子 Ising", pad=7)
        axis.set_xlim(1, 1e4)
        axis.set_ylim(0.006, 50)
        _panel_label(axis, chr(ord("a") + panel))
    axes[0].set_ylabel("归一化磁化／线性响应（示意）")
    axes[0].legend(loc="lower left")
    _save(figure, output_directory, "scaling_curves")


def make_all_figures(results: dict, output_directory: Path) -> None:
    boundary_classes_figure(output_directory)
    convergence_figure(results, output_directory)
    exponent_comparison_figure(results, output_directory)
    disorder_flow_figure(results, output_directory)
    scaling_curves_figure(results, output_directory)
