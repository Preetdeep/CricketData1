"""Paper 2: does the previous ball change what the batter does next?

Outcomes on the current ball: boundary, six, dot, runs off the bat, and
bowler-credited dismissal. Same sample and controls as the dismissal models.
The second half asks whether boundary persistence survives progressively
tighter controls for day-specific conditions, and corrects the batter-innings
fixed-effects estimate for the finite-sequence bias of Miller and Sanjurjo
(2018) with a within-innings permutation null.
"""
import os
import warnings

import numpy as np
import pandas as pd
import pyfixest as pf

from common import PANEL, SEED, TABLES, Numbers, pval_fmt
from tablefmt import regtable, yesno

warnings.filterwarnings("ignore")
num = Numbers("momentum")

f = pd.read_parquet(PANEL)
s = f[f.balls_faced_before > 0].copy()
s["bowler_match"] = s.bowler + "_" + s.match_id.astype(str)
s["batter_inn"] = s.batsman + "_" + s.innings_id
STATE = "over_inn + wkts_before + bf_bin + br_bin + season"
PEOPLE = "batsman + bowler + venue"
VC = {"CRV1": "match_id"}
X = "prev_dot + prev_four + prev_six"

outcomes = {
    "cur_boundary": ("Boundary", 100),
    "cur_six": ("Six", 100),
    "cur_dot": ("Dot", 100),
    "batsman_runs": ("Runs", 1),
    "striker_out": ("Out", 100),
}
models, means = [], []
for y, (nm, sc) in outcomes.items():
    s["yy"] = sc * s[y]
    m = pf.feols(f"yy ~ {X} | {STATE} + {PEOPLE}", data=s, vcov=VC)
    models.append(m)
    means.append(s.yy.mean())
    for t, tn in [("prev_dot", "Dot"), ("prev_four", "Four"), ("prev_six", "Six")]:
        num.add(f"M{nm}{tn}", m.coef()[t], "{:.3f}" if sc == 1 else "{:.2f}")
        num.add(f"M{nm}{tn}SE", m.se()[t], "{:.3f}" if sc == 1 else "{:.2f}")
        num.add(f"M{nm}{tn}P", pval_fmt(m.pvalue()[t]))
        num.add(f"M{nm}{tn}Rel", 100 * m.coef()[t] / s.yy.mean(), "{:.0f}")
    num.add(f"M{nm}Mean", s.yy.mean(), "{:.3f}" if sc == 1 else "{:.2f}")

labels = {"prev_dot": "Previous ball: dot", "prev_four": "Previous ball: four", "prev_six": "Previous ball: six"}
footer = [
    ("Units", ["pp", "pp", "pp", "runs", "pp"]),
    ("Match-state FE", yesno([1] * 5)),
    ("Batter, bowler, venue FE", yesno([1] * 5)),
    ("Mean of outcome", [f"{v:.2f}" if i != 3 else f"{v:.3f}" for i, v in enumerate(means)]),
    ("Observations", [f"{m._N:,}" for m in models]),
]
(TABLES / "momentum_outcomes.tex").write_text(
    regtable(models, ["prev_dot", "prev_four", "prev_six"], labels, footer,
             colnames=["Boundary", "Six", "Dot", "Runs", "Out"], digits=2)
)
pd.DataFrame(
    [{"outcome": outcomes[y][0], "term": t, "est": m.coef()[t], "se": m.se()[t], "mean": mu}
     for (y, m, mu) in zip(outcomes, models, means) for t in ["prev_dot", "prev_four", "prev_six"]]
).to_csv(TABLES / "momentum_outcomes.csv", index=False)

# ---------------------------------------------------------------------------
# Boundary persistence under tighter controls for the day's conditions.
# ---------------------------------------------------------------------------
s["yy"] = 100 * s.cur_boundary
specs = [
    ("State + people", f"{STATE} + {PEOPLE}"),
    ("+ match FE", f"{STATE} + {PEOPLE} + match_id"),
    ("+ team-innings FE", f"over + wkts_before + bf_bin + br_bin + {PEOPLE} + innings_id"),
    ("+ bowler-match FE", f"over_inn + wkts_before + bf_bin + br_bin + batsman + bowler_match"),
    ("Batter-innings FE", f"over_inn + wkts_before + bf_bin + br_bin + bowler + batter_inn"),
]
fe_models = [pf.feols(f"yy ~ {X} | {fe}", data=s, vcov=VC) for _, fe in specs]
footer2 = [
    ("Fixed effects", [lab for lab, _ in specs]),
    ("Observations", [f"{m._N:,}" for m in fe_models]),
]
tab = regtable(fe_models, ["prev_dot", "prev_four", "prev_six"], labels, footer2)
(TABLES / "momentum_fe.tex").write_text(tab)
for i, m in enumerate(fe_models):
    tag = "ABCDE"[i]
    for t, tn in [("prev_dot", "Dot"), ("prev_six", "Six"), ("prev_four", "Four")]:
        num.add(f"FE{tag}{tn}", m.coef()[t])
        num.add(f"FE{tag}{tn}P", pval_fmt(m.pvalue()[t]))

# ---------------------------------------------------------------------------
# Robustness of the boundary response (state + people FE throughout).
# ---------------------------------------------------------------------------
rb_rows = []


def rb(label, data, fe=f"{STATE} + {PEOPLE}"):
    data = data.copy()
    data["yy"] = 100 * data.cur_boundary
    m = pf.feols(f"yy ~ {X} | {fe}", data=data, vcov=VC)
    r = {"label": label, "n": m._N, "mean": data.yy.mean()}
    for t in ["prev_dot", "prev_four", "prev_six"]:
        r[t] = (m.coef()[t], m.se()[t], m.pvalue()[t])
    rb_rows.append(r)


rb("Baseline", s)
rb("Seasons 2008--2017", s[s.original_sample == 1])
rb("Season 2018 only", s[s.season == 2018], fe="over_inn + wkts_before + bf_bin + br_bin")
rb("Consecutive balls only", s[s.consecutive == 1])
rb("Same over only", s[(s.same_over == 1) & (s.consecutive == 1)])
rb("Striker has faced 5+ balls", s[s.balls_faced_before >= 5])
rb("Excluding death overs", s[s.phase != "death"])
rb("First innings", s[s.inning == 1])
rb("Second innings", s[s.inning == 2])
rb("Batter-season, bowler-season FE", s, fe=f"{STATE} + batsman^season + bowler^season + venue")
from common import stars
lines = [r"\begin{tabular}{lccccc}", r"\toprule", r" & Dot & Four & Six & Mean (pp) & N \\", r"\midrule"]
for r in rb_rows:
    cells = [f"{r[t][0]:.2f}{stars(r[t][2])} ({r[t][1]:.2f})" for t in ["prev_dot", "prev_four", "prev_six"]]
    lines.append(f"{r['label']} & " + " & ".join(cells) + f" & {r['mean']:.2f} & {r['n']:,} \\\\")
lines += [r"\bottomrule", r"\end{tabular}"]
(TABLES / "momentum_robust.tex").write_text("\n".join(lines) + "\n")
dots = [r["prev_dot"][0] for r in rb_rows]; sixes = [r["prev_six"][0] for r in rb_rows]
num.add("RbDotMin", min(dots)); num.add("RbDotMax", max(dots))
num.add("RbSixMin", min(sixes)); num.add("RbSixMax", max(sixes))

# ---------------------------------------------------------------------------
# Permutation null. Shuffling runs within each batter-innings keeps every
# batter's innings composition and every control fixed, and breaks only the
# order of balls. Under each FE spec, the mean of the permuted coefficients is
# what the estimator returns when order carries no information: innings-level
# heterogeneity in the pooled specs, finite-sequence (Nickell / Miller-Sanjurjo)
# bias in the small-group specs. Actual minus that mean is the order effect.
# ---------------------------------------------------------------------------
fb = f.copy()
fb = fb.sort_values(["match_id", "inning", "seq"]).reset_index(drop=True)
fb["batter_inn"] = fb.batsman + "_" + fb.innings_id
grp = fb.groupby("batter_inn", sort=False)
keep = (fb.balls_faced_before > 0).to_numpy()
runs0 = fb.batsman_runs.to_numpy()
# Dismissal balls stay where they are: the innings ends there by construction.
out_mask = fb.striker_out_any.to_numpy().astype(bool)
idx_groups = [np.asarray(g) for g in grp.indices.values()]
# Order within group by sequence (indices are already in sequence order).
fb["bowler_match"] = fb.bowler + "_" + fb.match_id.astype(str)
cols = ["over_inn", "over", "wkts_before", "bf_bin", "br_bin", "season", "batsman", "bowler", "venue",
        "match_id", "innings_id", "bowler_match", "batter_inn"]
base = fb.loc[keep, cols].reset_index(drop=True)
bi_codes = pd.factorize(fb.batter_inn)[0]
BR_EDGES = [-1, 0, 5, 10, 20, 30, 40, 50, 75, 300]
BR_LABELS = ["0", "1-5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-75", "76+"]
prev_index = np.full(len(fb), -1)
for ix in idx_groups:
    prev_index[ix[1:]] = ix[:-1]


def boundary_coefs(runs):
    """Boundary-persistence coefficients for every FE spec, on a given run sequence."""
    prev = runs[prev_index]
    d = base.copy()
    d["yy"] = 100 * np.isin(runs[keep], (4, 6))
    # Runs scored before the ball depend on the order, so recompute them.
    before = pd.Series(runs).groupby(bi_codes).cumsum().to_numpy() - runs
    d["br_bin"] = pd.cut(before[keep], BR_EDGES, labels=BR_LABELS).astype(str)
    p = prev[keep]
    d["prev_dot"] = (p == 0).astype(int)
    d["prev_four"] = (p == 4).astype(int)
    d["prev_six"] = (p == 6).astype(int)
    out = []
    for _, fe in specs:
        m = pf.feols(f"yy ~ {X} | {fe}", data=d, vcov="iid")
        out.append(m.coef()[["prev_dot", "prev_four", "prev_six"]].to_numpy())
    return np.array(out)  # specs x 3


actual = boundary_coefs(runs0)
rng = np.random.default_rng(SEED)
R = int(os.environ.get("PERM_R", 200))
perm = np.empty((R, len(specs), 3))
movable = [ix[~out_mask[ix]] for ix in idx_groups]
for r in range(R):
    runs = runs0.copy()
    for ix in movable:
        if len(ix) > 1:
            runs[ix] = runs0[rng.permutation(ix)]
    perm[r] = boundary_coefs(runs)
bias = perm.mean(0)
corrected = actual - bias
np.save(TABLES / "permutation_null.npy", perm)
rows = []
for i, (lab, _) in enumerate(specs):
    for j, (t, tn) in enumerate([("prev_dot", "Dot"), ("prev_four", "Four"), ("prev_six", "Six")]):
        rows.append({"spec": lab, "term": t, "actual": actual[i, j], "null_mean": bias[i, j],
                     "null_sd": perm[:, i, j].std(), "corrected": corrected[i, j],
                     "p": (np.sum(np.abs(perm[:, i, j] - bias[i, j]) >= abs(corrected[i, j])) + 1) / (R + 1)})
        tag = "ABCDE"[i]
        num.add(f"P{tag}{tn}Null", bias[i, j])
        num.add(f"P{tag}{tn}Corr", corrected[i, j])
        num.add(f"P{tag}{tn}P", pval_fmt(rows[-1]["p"]))
pt = pd.DataFrame(rows)
pt.to_csv(TABLES / "permutation.csv", index=False)

lines = [r"\begin{tabular}{lccccccccc}", r"\toprule",
         r" & \multicolumn{3}{c}{Previous ball: dot} & \multicolumn{3}{c}{Previous ball: four} & \multicolumn{3}{c}{Previous ball: six} \\",
         r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}\cmidrule(lr){8-10}",
         r"Fixed effects & Est. & Null & Corr. & Est. & Null & Corr. & Est. & Null & Corr. \\", r"\midrule"]
for i, (lab, _) in enumerate(specs):
    cells = []
    for j in range(3):
        r_ = pt[(pt.spec == lab)].iloc[j]
        st = "***" if r_.p < 0.01 else "**" if r_.p < 0.05 else "*" if r_.p < 0.1 else ""
        cells += [f"{r_.actual:.2f}", f"{r_.null_mean:.2f}", f"{r_.corrected:.2f}{st}"]
    lines.append(f"{lab} & " + " & ".join(cells) + r" \\")
lines += [r"\bottomrule", r"\end{tabular}"]
(TABLES / "permutation.tex").write_text("\n".join(lines) + "\n")
num.add("PermR", R, "{:d}")
num.add("BatterInnings", len(idx_groups), "{:,}")
num.add("BatterInningsMeanBalls", np.mean([len(ix) for ix in idx_groups]), "{:.1f}")

# ---------------------------------------------------------------------------
# Heterogeneity of the after-dot response: chases by required run rate.
# ---------------------------------------------------------------------------
ch = s[(s.inning == 2) & s.rrr.notna() & (s.runs_req > 0)].copy()
ch["high_rrr"] = (ch.rrr >= 9).astype(int)
for c in ["prev_dot", "prev_four", "prev_six"]:
    ch[f"{c}_hi"] = ch[c] * ch.high_rrr
ch["yy"] = 100 * ch.cur_boundary
mh = pf.feols(f"yy ~ {X} + prev_dot_hi + prev_four_hi + prev_six_hi | {STATE} + {PEOPLE} + rrr_bin", data=ch, vcov=VC)
num.add("HetDot", mh.coef()["prev_dot"])
num.add("HetDotHi", mh.coef()["prev_dot_hi"])
num.add("HetDotHiP", pval_fmt(mh.pvalue()["prev_dot_hi"]))
num.add("HetSixHi", mh.coef()["prev_six_hi"])
num.add("HetSixHiP", pval_fmt(mh.pvalue()["prev_six_hi"]))

num.write()
pf.etable(models)
pf.etable(fe_models)
print(pt.round(3).to_string())
