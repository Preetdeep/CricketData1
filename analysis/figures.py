"""Figures for the paper. Static PDFs in a restrained, print-safe palette."""
import warnings

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyfixest as pf

from common import FIGURES, ORIGINAL_LAST_SEASON, PANEL, RAW, TABLES

warnings.filterwarnings("ignore")

BLUE, ORANGE, AQUA = "#2a78d6", "#eb6834", "#1baf7a"
INK, MUTED, GRID = "#0b0b0b", "#52514e", "#d9d8d4"
plt.rcParams.update({
    "font.family": "serif", "font.size": 9, "axes.titlesize": 9, "axes.labelsize": 9,
    "axes.edgecolor": MUTED, "axes.labelcolor": INK, "xtick.color": MUTED, "ytick.color": MUTED,
    "axes.spines.top": False, "axes.spines.right": False, "axes.grid": True, "grid.color": GRID,
    "grid.linewidth": 0.5, "axes.axisbelow": True, "legend.frameon": False, "figure.dpi": 150,
    "lines.linewidth": 1.5,
})

f = pd.read_parquet(PANEL)
s = f[f.balls_faced_before > 0].copy()
STATE = "over_inn + wkts_before + bf_bin + br_bin + season"
PEOPLE = "batsman + bowler + venue"
VC = {"CRV1": "match_id"}

# ---------------------------------------------------------------------------
# Figure 1. The draft's comparison with and without a like-for-like baseline.
# ---------------------------------------------------------------------------
d = pd.read_csv(RAW / "deliveries.csv")
m = pd.read_csv(RAW / "matches.csv")
d = d.merge(m[["id", "season"]].rename(columns={"id": "match_id"}), on="match_id")
d = d[d.season <= ORIGINAL_LAST_SEASON].reset_index(drop=True)
wk = d[d.player_dismissed.notna() & (d.over + d.ball > 2)]
pv = d.loc[wk.index - 1]
filt = lambda x: x[(x.batsman_runs % 2 == 0) | ((x.ball == 6) & (x.batsman_runs % 2 == 1))]
pv = filt(pv)
dfil = filt(d)
dot = lambda x: ((x.total_runs == 0) & x.player_dismissed.isna()).mean() * 100
bd = lambda x: (x.total_runs.isin([4, 6]) & x.player_dismissed.isna()).mean() * 100

fig, axes = plt.subplots(1, 2, figsize=(6.5, 2.6), sharey=False)
for ax, (fn, title) in zip(axes, [(dot, "Dot balls"), (bd, "Boundaries")]):
    x = np.arange(2)
    w = 0.26
    vals = [[fn(src[src.inning == i]) for i in (1, 2)] for src in (d, dfil, pv)]
    labs = ["All balls", "All balls, same parity filter", "Ball before a wicket (draft)"]
    cols = [GRID, BLUE, ORANGE]
    for k, (v, lab, c) in enumerate(zip(vals, labs, cols)):
        bars = ax.bar(x + (k - 1) * w, v, w * 0.92, color=c, label=lab, edgecolor="white", linewidth=0.8)
        for b_, val in zip(bars, v):
            ax.text(b_.get_x() + b_.get_width() / 2, val + 0.6, f"{val:.0f}", ha="center", va="bottom",
                    fontsize=7, color=MUTED)
    ax.set_xticks(x, ["1st innings", "2nd innings"])
    ax.set_title(title, loc="left", color=INK)
    ax.set_ylabel("Share of deliveries (%)")
    ax.grid(axis="x", visible=False)
axes[0].legend(loc="upper left", bbox_to_anchor=(0, -0.16), ncol=3, fontsize=7.5)
fig.tight_layout()
fig.savefig(FIGURES / "fig1_replication.pdf", bbox_inches="tight")
plt.close(fig)

# ---------------------------------------------------------------------------
# Figure 2. Why raw comparisons mislead: the over gradient in risk and in the
# previous ball's outcome.
# ---------------------------------------------------------------------------
g = s.groupby("over").agg(out=("striker_out", "mean"), six=("prev_six", "mean"), dot=("prev_dot", "mean"),
                          four=("prev_four", "mean"))
fig, axes = plt.subplots(1, 2, figsize=(6.5, 2.4))
axes[0].plot(g.index, 100 * g.out, color=INK, marker="o", ms=3)
axes[0].set_title("Dismissal rate per faced ball (%)", loc="left")
axes[1].plot(g.index, 100 * g["dot"], color=BLUE, marker="o", ms=3, label="Dot")
axes[1].plot(g.index, 100 * g.four, color=AQUA, marker="o", ms=3, label="Four")
axes[1].plot(g.index, 100 * g.six, color=ORANGE, marker="o", ms=3, label="Six")
axes[1].set_title("Striker's previous ball was a ... (%)", loc="left")
for ax in axes:
    ax.set_xlabel("Over")
    ax.set_xticks([1, 5, 10, 15, 20])
for lab, col, y, dy in [("Dot", BLUE, g["dot"].iloc[-1], 0), ("Four", AQUA, g.four.iloc[-1], 5),
                       ("Six", ORANGE, g.six.iloc[-1], -5)]:
    axes[1].annotate(lab, (20, 100 * y), xytext=(4, dy), textcoords="offset points", color=MUTED, fontsize=7.5,
                     va="center")
axes[1].set_xlim(0.5, 22)
fig.tight_layout()
fig.savefig(FIGURES / "fig2_over_gradient.pdf", bbox_inches="tight")
plt.close(fig)

# ---------------------------------------------------------------------------
# Figure 3. Dismissal coefficients as controls are added, with the +/-10%
# equivalence band.
# ---------------------------------------------------------------------------
s["y"] = 100 * s.striker_out
X = "prev_dot + prev_four + prev_six"
specs = [("No controls", f"y ~ {X}"), ("Match state", f"y ~ {X} | {STATE}"),
         ("+ batter, bowler, venue", f"y ~ {X} | {STATE} + {PEOPLE}")]
res = []
for lab, fml in specs:
    mm = pf.feols(fml, data=s, vcov=VC)
    for t in ["prev_dot", "prev_four", "prev_six"]:
        res.append((lab, t, mm.coef()[t], mm.se()[t]))
res = pd.DataFrame(res, columns=["spec", "term", "b", "se"])
delta = 0.1 * s.y.mean()
fig, ax = plt.subplots(figsize=(6.5, 2.6))
terms = [("prev_dot", "After a dot"), ("prev_four", "After a four"), ("prev_six", "After a six")]
cols = [GRID, BLUE, ORANGE]
ax.axhspan(-delta, delta, color="#f1f0ec", zorder=0)
ax.axhline(0, color=MUTED, lw=0.8)
for k, (lab, _) in enumerate(specs):
    sub = res[res.spec == lab]
    xs = np.arange(3) + (k - 1) * 0.22
    ax.errorbar(xs, sub.b, yerr=1.96 * sub.se, fmt="o", color=[MUTED, BLUE, ORANGE][k], ms=4, capsize=0,
                elinewidth=1.4, label=lab)
ax.set_xticks(np.arange(3), [t[1] for t in terms])
ax.set_ylabel("Change in P(out), pp")
ax.text(-0.45, -delta * 0.85, "Shaded: $\\pm$10% of base rate", fontsize=7, color=MUTED, ha="left")
ax.legend(loc="upper left", fontsize=7.5, ncol=3)
ax.grid(axis="x", visible=False)
fig.tight_layout()
fig.savefig(FIGURES / "fig3_dismissal_coefs.pdf", bbox_inches="tight")
plt.close(fig)

# ---------------------------------------------------------------------------
# Figure 4. Behavioural response: next-ball outcomes relative to their means.
# ---------------------------------------------------------------------------
mo = pd.read_csv(TABLES / "momentum_outcomes.csv")
mo = mo[mo.outcome.isin(["Boundary", "Six", "Dot", "Out"])]
fig, ax = plt.subplots(figsize=(6.5, 2.6))
outs = ["Boundary", "Six", "Dot", "Out"]
for k, (t, lab) in enumerate(terms):
    sub = mo[mo.term == t].set_index("outcome").loc[outs]
    rel = 100 * sub.est / sub["mean"]
    rse = 100 * sub.se / sub["mean"]
    xs = np.arange(4) + (k - 1) * 0.22
    ax.errorbar(xs, rel, yerr=1.96 * rse, fmt="o", color=[BLUE, AQUA, ORANGE][k], ms=4, elinewidth=1.4, label=lab)
ax.axhline(0, color=MUTED, lw=0.8)
ax.set_xticks(np.arange(4), ["Boundary", "Six", "Dot ball", "Dismissal"])
ax.set_ylabel("Change relative to mean (%)")
ax.set_xlabel("Outcome on the current ball")
ax.legend(loc="upper right", fontsize=7.5, ncol=3)
ax.grid(axis="x", visible=False)
fig.tight_layout()
fig.savefig(FIGURES / "fig4_behaviour.pdf", bbox_inches="tight")
plt.close(fig)

# ---------------------------------------------------------------------------
# Figure 5. Dose response in the length of the dot-ball run.
# ---------------------------------------------------------------------------
s["dot_k"] = s.dot_streak.clip(upper=4).astype(int)
s["yb"] = 100 * s.cur_boundary
s["dot_run"] = np.where(s.prev_cat == "dot", s.dot_k, -1)  # -1 = previous ball not a dot
s["prev_bdry"] = s.prev_four + s.prev_six
out = []
for y, lab in [("y", "Dismissal"), ("yb", "Boundary")]:
    for k in [1, 2, 3, 4]:
        s[f"dr{k}"] = (s.dot_run == k).astype(int)
    mm = pf.feols(f"{y} ~ dr1 + dr2 + dr3 + dr4 + prev_four + prev_six | {STATE} + {PEOPLE}", data=s, vcov=VC)
    co = mm.coef(); se = mm.se()
    for k in [1, 2, 3, 4]:
        out.append((lab, k, co[f"dr{k}"], se[f"dr{k}"], s[y].mean()))
dr = pd.DataFrame(out, columns=["outcome", "k", "b", "se", "mean"])
dr.to_csv(TABLES / "dot_run.csv", index=False)
fig, axes = plt.subplots(1, 2, figsize=(6.5, 2.4))
for ax, lab, col in zip(axes, ["Dismissal", "Boundary"], [INK, BLUE]):
    sub = dr[dr.outcome == lab]
    ax.axhline(0, color=MUTED, lw=0.8)
    ax.errorbar(sub.k, sub.b, yerr=1.96 * sub.se, fmt="o-", color=col, ms=4, elinewidth=1.4)
    ax.set_xticks([1, 2, 3, 4], ["1", "2", "3", "4+"])
    ax.set_xlabel("Consecutive dots faced by striker")
    ax.set_title(f"{lab}: change vs. previous ball 1--3 runs (pp)", loc="left")
    ax.grid(axis="x", visible=False)
fig.tight_layout()
fig.savefig(FIGURES / "fig5_dot_runs.pdf", bbox_inches="tight")
plt.close(fig)

# ---------------------------------------------------------------------------
# Figure 6. Permutation null distributions (written once momentum.py has run).
# ---------------------------------------------------------------------------
pn = TABLES / "permutation_null.npy"
if pn.exists():
    perm = np.load(pn)
    pt = pd.read_csv(TABLES / "permutation.csv")
    specs_ = list(dict.fromkeys(pt.spec))
    fig, axes = plt.subplots(1, 2, figsize=(6.5, 2.4))
    for ax, (j, t, lab) in zip(axes, [(0, "prev_dot", "After a dot"), (2, "prev_six", "After a six")]):
        for i, (spec, col) in enumerate([(specs_[0], BLUE), (specs_[-1], ORANGE)]):
            ii = specs_.index(spec)
            ax.hist(perm[:, ii, j], bins=25, color=col, alpha=0.35, edgecolor="white", linewidth=0.4)
            act = pt[(pt.spec == spec) & (pt.term == t)].actual.iloc[0]
            ax.axvline(act, color=col, lw=1.6, label=f"{spec}: observed")
        ax.set_title(f"{lab}: boundary coefficient, pp", loc="left")
        ax.set_ylabel("Permutations")
        ax.grid(axis="x", visible=False)
    axes[0].legend(loc="upper left", bbox_to_anchor=(0, -0.2), ncol=2, fontsize=7.5)
    fig.tight_layout()
    fig.savefig(FIGURES / "fig6_permutation.pdf", bbox_inches="tight")
    plt.close(fig)
print("figures written")
