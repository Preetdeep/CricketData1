"""Robustness of the dismissal estimates (paper 1).

Each row re-estimates the full specification (match-state FE plus batter,
bowler and venue FE) under one change, and reports the dot, four and six
coefficients in percentage points with match-clustered standard errors.
Also reports a logit with average marginal effects, two-sided equivalence
tests and Benjamini-Hochberg adjusted p-values for the six innings tests.
"""
import warnings

import numpy as np
import pandas as pd
import pyfixest as pf
import statsmodels.formula.api as smf
from scipy import stats

from common import PANEL, TABLES, Numbers, pval_fmt, stars

warnings.filterwarnings("ignore")
num = Numbers("robust")

f = pd.read_parquet(PANEL)
s = f[f.balls_faced_before > 0].copy()
s["y"] = 100 * s.striker_out

STATE = "over_inn + wkts_before + bf_bin + br_bin + season"
PEOPLE = "batsman + bowler + venue"
VC = {"CRV1": "match_id"}
X = "prev_dot + prev_four + prev_six"
TERMS = ["prev_dot", "prev_four", "prev_six"]


def fit(data, y="y", x=X, fe=f"{STATE} + {PEOPLE}", vcov=VC):
    return pf.feols(f"{y} ~ {x} | {fe}", data=data, vcov=vcov)


rows = []


def record(label, m, data, terms=TERMS, y="y"):
    r = {"label": label, "n": m._N, "mean": data[y].mean()}
    for t in terms:
        r[t] = (m.coef()[t], m.se()[t], m.pvalue()[t])
    rows.append(r)


record("Baseline", fit(s), s)

# Outcome definitions.
s["y_any"] = 100 * s.striker_out_any
record("Outcome includes run outs", fit(s, y="y_any"), s, y="y_any")
s["y_c"] = 100 * s.out_caught
record("Outcome: caught only", fit(s, y="y_c"), s, y="y_c")
s["y_b"] = 100 * s.out_bowled_lbw
record("Outcome: bowled or lbw only", fit(s, y="y_b"), s, y="y_b")

# Treatment definition: a strict dot also concedes no extras.
s2 = s.copy()
s2["prev_dot"] = ((s2.prev_runs == 0) & (s2.prev_extras == 0)).astype(int)
s2["prev_bye"] = ((s2.prev_runs == 0) & (s2.prev_extras > 0)).astype(int)
m = fit(s2, x=X + " + prev_bye")
record("Strict dot (no extras)", m, s2)

# Team-level previous ball, as in the 2022 draft: whoever faced it.
t = f[f.inn_prev_runs.notna()].copy()
t["y"] = 100 * t.striker_out
t["prev_dot"] = ((t.inn_prev_runs == 0) & (t.inn_prev_wicket == 0)).astype(int)
t["prev_four"] = (t.inn_prev_runs == 4).astype(int)
t["prev_six"] = (t.inn_prev_runs == 6).astype(int)
t["prev_w"] = (t.inn_prev_wicket == 1).astype(int)
record("Previous ball of the innings (any striker)", fit(t, x=X + " + prev_w"), t)

# Samples.
record("Seasons 2008--2017 (draft sample)", fit(s[s.original_sample == 1]), s[s.original_sample == 1])
record("Season 2018 only (held out)", fit(s[s.season == 2018], fe=f"over_inn + wkts_before + bf_bin + br_bin"),
       s[s.season == 2018])
s_ = s[s.balls_faced_before >= 5]
record("Striker has faced 5+ balls", fit(s_), s_)
s_ = s[s.wkts_before <= 5]
record("Top order (0--5 wickets down)", fit(s_), s_)
s_ = s[s.phase != "death"]
record("Excluding death overs (16--20)", fit(s_), s_)

# Controls.
s["bowler_match"] = s.bowler + "_" + s.match_id.astype(str)
record("Team-innings FE", fit(s, fe="innings_id + over + wkts_before + bf_bin + br_bin + batsman + bowler"), s)
record("Bowler-by-match FE (the spell)",
       fit(s, fe="bowler_match + over_inn + wkts_before + bf_bin + br_bin + batsman"), s)
record("Batter-season and bowler-season FE",
       fit(s, fe=f"{STATE} + batsman^season + bowler^season + venue"), s)
record("Two-way clustering (match, batter)", fit(s, vcov={"CRV1": "match_id+batsman"}), s)

rob = pd.DataFrame(rows)

lines = [
    r"\begin{tabular}{lccccc}",
    r"\toprule",
    r" & Dot & Four & Six & Mean (pp) & N \\",
    r"\midrule",
]
for _, r in rob.iterrows():
    cells = []
    for t_ in TERMS:
        b, se, p = r[t_]
        cells.append(f"{b:.2f}{stars(p)} ({se:.2f})")
    lines.append(f"{r.label} & " + " & ".join(cells) + f" & {r['mean']:.2f} & {r.n:,} \\\\")
lines += [r"\bottomrule", r"\end{tabular}"]
(TABLES / "robustness.tex").write_text("\n".join(lines) + "\n")

b0 = rob.iloc[0]
num.add("RobRows", len(rob) - 1, "{:d}")
dots = np.array([r["prev_dot"][0] for _, r in rob.iterrows()])
sixes = np.array([r["prev_six"][0] for _, r in rob.iterrows()])
num.add("RobDotMin", dots.min())
num.add("RobDotMax", dots.max())
num.add("RobSixMin", sixes.min())
num.add("RobSixMax", sixes.max())
nsig = sum(1 for _, r in rob.iterrows() for t_ in ["prev_dot", "prev_six"] if r[t_][2] < 0.05 and r[t_][0] > 0)
num.add("RobPosSig", nsig, "{:d}")

# ---------------------------------------------------------------------------
# Logit with average marginal effects (state controls as dummies, season FE).
# ---------------------------------------------------------------------------
lg = smf.logit(
    "striker_out ~ prev_dot + prev_four + prev_six + C(over_inn) + C(wkts_before) + C(bf_bin) + C(br_bin) + C(season)",
    data=s,
).fit(disp=0, cov_type="cluster", cov_kwds={"groups": pd.factorize(s.match_id)[0]})
# Discrete-change average marginal effects, versus the omitted 1-3 runs category.
base_df = s.assign(prev_dot=0, prev_four=0, prev_six=0)
p0 = lg.predict(base_df)
for t_, nm in [("prev_dot", "Dot"), ("prev_four", "Four"), ("prev_six", "Six")]:
    p1 = lg.predict(base_df.assign(**{t_: 1}))
    num.add(f"Logit{nm}", 100 * float(np.mean(p1 - p0)))
    num.add(f"Logit{nm}OR", np.exp(lg.params[t_]))
    num.add(f"Logit{nm}P", pval_fmt(lg.pvalues[t_]))

# ---------------------------------------------------------------------------
# Equivalence: two one-sided tests against +/- 10% of the base rate.
# ---------------------------------------------------------------------------
base = s.y.mean()
delta = 0.10 * base
num.add("EqDelta", delta)
eq = []
for t_, nm in [("prev_dot", "Dot"), ("prev_four", "Four"), ("prev_six", "Six")]:
    b, se, _ = b0[t_]
    p_lo = stats.norm.sf((b + delta) / se)
    p_hi = stats.norm.cdf((b - delta) / se)
    p_tost = max(p_lo, p_hi)
    lo90, hi90 = b - 1.645 * se, b + 1.645 * se
    num.add(f"Eq{nm}P", pval_fmt(p_tost))
    num.add(f"Eq{nm}Lo", lo90)
    num.add(f"Eq{nm}Hi", hi90)
    num.add(f"Eq{nm}RelHi", 100 * hi90 / base, "{:.0f}")
    # One-sided test that the effect is below +10% of the base rate.
    num.add(f"Eq{nm}PUp", pval_fmt(p_hi))
    eq.append(p_tost)

# Minimum detectable effect at 80% power, 5% two-sided, from the baseline SE.
for t_, nm in [("prev_dot", "Dot"), ("prev_six", "Six")]:
    mde = 2.8 * b0[t_][1]
    num.add(f"MDE{nm}", mde)
    num.add(f"MDE{nm}Rel", 100 * mde / base, "{:.0f}")

# ---------------------------------------------------------------------------
# Multiple testing on the six innings-specific hypotheses (paper 1, H1-H2).
# ---------------------------------------------------------------------------
s["prev_dot_x2"] = s.prev_dot * s.inn2
s["prev_four_x2"] = s.prev_four * s.inn2
s["prev_six_x2"] = s.prev_six * s.inn2
mi = fit(s, x=X + " + prev_dot_x2 + prev_four_x2 + prev_six_x2")
ps = mi.pvalue()[["prev_dot", "prev_four", "prev_six", "prev_dot_x2", "prev_four_x2", "prev_six_x2"]]
order = np.argsort(ps.values)
m_ = len(ps)
adj = np.empty(m_)
prev = 1.0
for rank in range(m_ - 1, -1, -1):
    i = order[rank]
    prev = min(prev, ps.values[i] * m_ / (rank + 1))
    adj[i] = prev
num.add("BHMinP", f"={adj.min():.2f}")
num.add("BHFourXP", f"={adj[list(ps.index).index('prev_four_x2')]:.2f}")

num.write()
print(rob.to_string())
