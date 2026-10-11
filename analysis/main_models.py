"""Main estimates: does the striker's previous ball change the chance of dismissal?

Linear probability models on faced deliveries where the striker has faced at
least one earlier ball in the innings. The outcome is a bowler-credited
dismissal of the striker, scaled to percentage points. The omitted category for
the previous ball is 1-3 runs off the bat. Standard errors are clustered by match.
"""
import numpy as np
import pandas as pd
import pyfixest as pf

from common import PANEL, TABLES, Numbers, pval_fmt
from tablefmt import regtable, yesno

num = Numbers("main")

f = pd.read_parquet(PANEL)
s = f[f.balls_faced_before > 0].copy()
s["y"] = 100 * s.striker_out
for c in ["prev_dot", "prev_four", "prev_six"]:
    s[f"{c}_x2"] = s[c] * s.inn2

STATE = "over_inn + wkts_before + bf_bin + br_bin + season"
PEOPLE = "batsman + bowler + venue"
VC = {"CRV1": "match_id"}
X = "prev_dot + prev_four + prev_six"
XI = X + " + prev_dot_x2 + prev_four_x2 + prev_six_x2"

m1 = pf.feols(f"y ~ {X}", data=s, vcov=VC)
m2 = pf.feols(f"y ~ {X} | {STATE}", data=s, vcov=VC)
m3 = pf.feols(f"y ~ {X} | {STATE} + {PEOPLE}", data=s, vcov=VC)
m4 = pf.feols(f"y ~ {XI} | {STATE} + {PEOPLE}", data=s, vcov=VC)
sc = s[s.consecutive == 1]
m5 = pf.feols(f"y ~ {XI} | {STATE} + {PEOPLE}", data=sc, vcov=VC)
# Within the same over and bowler: the previous ball was bowled by the same
# bowler, in the same spell, to the same batter.
so = s[(s.same_over == 1) & (s.consecutive == 1)]
m6 = pf.feols(f"y ~ {XI} | {STATE} + {PEOPLE}", data=so, vcov=VC)
models = [m1, m2, m3, m4, m5, m6]

labels = {
    "prev_dot": "Previous ball: dot",
    "prev_four": "Previous ball: four",
    "prev_six": "Previous ball: six",
    "prev_dot_x2": r"\quad $\times$ second innings",
    "prev_four_x2": r"\quad $\times$ second innings",
    "prev_six_x2": r"\quad $\times$ second innings",
}
order = ["prev_dot", "prev_dot_x2", "prev_four", "prev_four_x2", "prev_six", "prev_six_x2"]
footer = [
    ("Sample", ["All", "All", "All", "All", "Consecutive", "Same over"]),
    ("Match-state FE", yesno([0, 1, 1, 1, 1, 1])),
    ("Batter, bowler, venue FE", yesno([0, 0, 1, 1, 1, 1])),
    ("Mean of outcome (pp)", [f"{d.y.mean():.2f}" for d in (s, s, s, s, sc, so)]),
    ("Observations", [f"{m._N:,}" for m in models]),
]
(TABLES / "main.tex").write_text(regtable(models, order, labels, footer))

# Headline numbers.
for tag, m in [("Raw", m1), ("State", m2), ("Full", m3)]:
    for t, nm in [("prev_dot", "Dot"), ("prev_four", "Four"), ("prev_six", "Six")]:
        num.add(f"{tag}{nm}", m.coef()[t])
        num.add(f"{tag}{nm}SE", m.se()[t])
        num.add(f"{tag}{nm}P", pval_fmt(m.pvalue()[t]))
        lo, hi = m.confint().loc[t]
        num.add(f"{tag}{nm}Lo", lo)
        num.add(f"{tag}{nm}Hi", hi)
num.add("BaseRate", s.y.mean())
for t, nm in [("prev_dot_x2", "DotX"), ("prev_four_x2", "FourX"), ("prev_six_x2", "SixX")]:
    num.add(f"Int{nm}", m4.coef()[t])
    num.add(f"Int{nm}SE", m4.se()[t])
    num.add(f"Int{nm}P", pval_fmt(m4.pvalue()[t]))
num.add("ConsN", len(sc), "{:,}")
num.add("SameOverN", len(so), "{:,}")

# Wald test: four and six effects on dismissal are equal (full model).
b, V = m3.coef(), m3._vcov
names = list(m3.coef().index)
i4, i6 = names.index("prev_four"), names.index("prev_six")
diff = b.iloc[i6] - b.iloc[i4]
se = np.sqrt(V[i4, i4] + V[i6, i6] - 2 * V[i4, i6])
from scipy import stats
num.add("SixMinusFour", diff)
num.add("SixMinusFourP", pval_fmt(2 * stats.norm.sf(abs(diff / se))))

# ---------------------------------------------------------------------------
# Dose-response: length of the dot-ball and boundary streak ending at the
# previous ball.
# ---------------------------------------------------------------------------
s["dot_k"] = s.dot_streak.clip(upper=4).astype(int)
s["bdry_k"] = s.bdry_streak.clip(upper=2).astype(int)
md = pf.feols(f"y ~ C(dot_k, contr.treatment(base=0)) + C(bdry_k, contr.treatment(base=0)) | {STATE} + {PEOPLE}",
              data=s, vcov=VC)
dose = md.tidy().reset_index()
dose.to_csv(TABLES / "dose_response.csv", index=False)

# Pre-specified pressure index within chases: required run rate.
ch = s[(s.inning == 2) & s.rrr.notna() & (s.balls_left > 0) & (s.runs_req > 0)].copy()
ch["high_rrr"] = (ch.rrr >= 9).astype(int)
for c in ["prev_dot", "prev_four", "prev_six"]:
    ch[f"{c}_hi"] = ch[c] * ch.high_rrr
mc = pf.feols(
    "y ~ prev_dot + prev_four + prev_six + prev_dot_hi + prev_four_hi + prev_six_hi"
    f" | {STATE} + {PEOPLE} + rrr_bin",
    data=ch, vcov=VC,
)
num.add("ChaseN", len(ch), "{:,}")
num.add("ChaseHighShare", 100 * ch.high_rrr.mean(), "{:.1f}")
for t, nm in [("prev_dot_hi", "Dot"), ("prev_four_hi", "Four"), ("prev_six_hi", "Six")]:
    num.add(f"Chase{nm}Hi", mc.coef()[t])
    num.add(f"Chase{nm}HiSE", mc.se()[t])
    num.add(f"Chase{nm}HiP", pval_fmt(mc.pvalue()[t]))
num.add("ChaseDot", mc.coef()["prev_dot"])
num.add("ChaseDotP", pval_fmt(mc.pvalue()["prev_dot"]))

labels_c = {
    "prev_dot": "Previous ball: dot",
    "prev_four": "Previous ball: four",
    "prev_six": "Previous ball: six",
    "prev_dot_hi": r"Dot $\times$ RRR $\geq 9$",
    "prev_four_hi": r"Four $\times$ RRR $\geq 9$",
    "prev_six_hi": r"Six $\times$ RRR $\geq 9$",
}
footer_c = [
    ("Match-state, RRR-bin FE", yesno([1])),
    ("Batter, bowler, venue FE", yesno([1])),
    ("Mean of outcome (pp)", [f"{ch.y.mean():.2f}"]),
    ("Observations", [f"{mc._N:,}"]),
]
(TABLES / "chase.tex").write_text(
    regtable([mc], ["prev_dot", "prev_dot_hi", "prev_four", "prev_four_hi", "prev_six", "prev_six_hi"], labels_c, footer_c,
             colnames=["Chases"])
)

num.write()
pf.etable(models + [md, mc])
print(dose)
