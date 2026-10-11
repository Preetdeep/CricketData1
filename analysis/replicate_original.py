"""Replicate the 2022 draft's retrospective design and show where it goes wrong.

The draft took the row immediately above every wicket in the raw file, kept rows
where batsman_runs was even (or odd on ball 6), and compared the outcome mix of
those rows with the outcome mix of all deliveries, innings by innings, using
two-sample t-tests.
"""
import numpy as np
import pandas as pd
from scipy import stats

from common import ORIGINAL_LAST_SEASON, PANEL, RAW, TABLES, Numbers, pval_fmt

num = Numbers("replicate")

# ---------------------------------------------------------------------------
# 1. The draft's procedure, run on the draft's sample (IPL 2008-2017).
# ---------------------------------------------------------------------------
d = pd.read_csv(RAW / "deliveries.csv")
m = pd.read_csv(RAW / "matches.csv")
d = d.merge(m[["id", "season"]].rename(columns={"id": "match_id"}), on="match_id")
d = d[d.season <= ORIGINAL_LAST_SEASON].reset_index(drop=True)
num.add("OrigDeliveries", len(d), "{:,}")
num.add("OrigMatches", d.match_id.nunique(), "{:,}")

wk = d[d.player_dismissed.notna() & (d.over + d.ball > 2)]
prev = d.loc[wk.index - 1].copy()
# How often is the "previous" row from another innings or match, or a super over?
cross = (prev.match_id.to_numpy() != wk.match_id.to_numpy()) | (prev.inning.to_numpy() != wk.inning.to_numpy())
num.add("OrigCrossInnings", int(cross.sum()), "{:,}")
num.add("OrigSuperOverRows", int((wk.inning > 2).sum()), "{:,}")

keep = (prev.batsman_runs % 2 == 0) | ((prev.ball == 6) & (prev.batsman_runs % 2 == 1))
pv = prev[keep].copy()
wkk = wk[keep.to_numpy()]
same_striker = pv.batsman.to_numpy() == wkk.batsman.to_numpy()
num.add("OrigPrevRows", len(pv), "{:,}")
num.add("OrigSameStrikerShare", 100 * same_striker.mean(), "{:.1f}")
dismissed_is_striker = wkk.player_dismissed.to_numpy() == wkk.batsman.to_numpy()
num.add("OrigRunOutShare", 100 * (wkk.dismissal_kind == "run out").mean(), "{:.1f}")

# The draft recoded wicket balls as "W", so a dot is 0 total runs and no wicket.
for x in (pv, d):
    x["Zero"] = ((x.total_runs == 0) & x.player_dismissed.isna()).astype(int)
    x["Boundary"] = (x.total_runs.isin([4, 6]) & x.player_dismissed.isna()).astype(int)
# The like-for-like baseline: all balls passing the same parity filter.
dfil = d[(d.batsman_runs % 2 == 0) | ((d.ball == 6) & (d.batsman_runs % 2 == 1))]
num.add("OrigFilterDropShare", 100 * (1 - len(dfil) / len(d)), "{:.1f}")

rows = []
for var in ["Zero", "Boundary"]:
    for inn in (1, 2):
        a = pv[pv.inning == inn][var]
        b = d[d.inning == inn][var]
        c = dfil[dfil.inning == inn][var]
        t = stats.ttest_ind(b, a, equal_var=False)
        rows.append((var, inn, b.mean(), c.mean(), a.mean(), a.mean() / b.mean(), a.mean() / c.mean(), t.pvalue, len(a)))
    a1, a2 = pv[pv.inning == 1][var], pv[pv.inning == 2][var]
    t = stats.ttest_ind(a1, a2, equal_var=False)
    num.add(f"Orig{var}InnP", pval_fmt(t.pvalue))
rep = pd.DataFrame(rows, columns=["var", "inning", "all", "filt", "before_w", "ratio", "ratio_filt", "p", "n"])

for _, r in rep.iterrows():
    tag = f"{r['var']}{'One' if r.inning == 1 else 'Two'}"
    num.add(f"Orig{tag}All", 100 * r["all"], "{:.1f}")
    num.add(f"Orig{tag}Before", 100 * r.before_w, "{:.1f}")
    num.add(f"Orig{tag}Ratio", r.ratio, "{:.2f}")
    num.add(f"Orig{tag}Filt", 100 * r.filt, "{:.1f}")
    num.add(f"Orig{tag}RatioFilt", r.ratio_filt, "{:.2f}")

# ---------------------------------------------------------------------------
# 2. The same question asked prospectively on the cleaned panel, same seasons.
#    P(out | previous ball of same striker was X), by innings.
# ---------------------------------------------------------------------------
f = pd.read_parquet(PANEL)
f = f[(f.original_sample == 1) & (f.balls_faced_before > 0)]
haz = f.groupby(["inning", "prev_cat"]).striker_out.agg(["mean", "size"]).reset_index()
base = f.groupby("inning").striker_out.mean()
haz["rel"] = haz.apply(lambda r: r["mean"] / base[r.inning], axis=1)
for _, r in haz.iterrows():
    tag = f"{r.prev_cat.capitalize()}{'One' if r.inning == 1 else 'Two'}"
    num.add(f"Haz{tag}", 100 * r["mean"])
    num.add(f"Rel{tag}", r.rel)

# The draft's comparison mixes the striker's own previous ball with balls faced by the
# partner. Reproduce the retrospective "dot share before a wicket" with the striker's own
# previous ball, and the matching share among all balls with a previous ball.
f["prev_zero"] = (f.prev_runs == 0).astype(int)
for inn in (1, 2):
    s = f[f.inning == inn]
    before = s[s.striker_out == 1].prev_zero.mean()
    allb = s.prev_zero.mean()
    tag = "One" if inn == 1 else "Two"
    num.add(f"OwnDotBefore{tag}", 100 * before, "{:.1f}")
    num.add(f"OwnDotAll{tag}", 100 * allb, "{:.1f}")

# ---------------------------------------------------------------------------
# Table: the draft's comparison next to the striker-level hazard.
# ---------------------------------------------------------------------------
lines = [
    r"\begin{tabular}{lcccccc}",
    r"\toprule",
    r" & \multicolumn{3}{c}{First innings} & \multicolumn{3}{c}{Second innings} \\",
    r"\cmidrule(lr){2-4}\cmidrule(lr){5-7}",
    r" & All & All, filtered & Before W & All & All, filtered & Before W \\",
    r"\midrule",
    r"\multicolumn{7}{l}{\emph{Panel A. Draft procedure: share of deliveries (\%)}} \\",
]
for var, lab in [("Zero", "Dot ball"), ("Boundary", "Boundary")]:
    r1 = rep[(rep["var"] == var) & (rep.inning == 1)].iloc[0]
    r2 = rep[(rep["var"] == var) & (rep.inning == 2)].iloc[0]
    lines.append(
        f"{lab} & {100*r1['all']:.1f} & {100*r1.filt:.1f} & {100*r1.before_w:.1f} & "
        f"{100*r2['all']:.1f} & {100*r2.filt:.1f} & {100*r2.before_w:.1f} \\\\"
    )
    lines.append(
        f"\\quad Ratio to baseline & & {r1.ratio_filt:.2f} & {r1.ratio:.2f} & & {r2.ratio_filt:.2f} & {r2.ratio:.2f} \\\\"
    )
n1 = int(rep[(rep.inning == 1)].n.iloc[0]); n2 = int(rep[(rep.inning == 2)].n.iloc[0])
lines.append(f"Wickets used & \\multicolumn{{3}}{{c}}{{{n1:,}}} & \\multicolumn{{3}}{{c}}{{{n2:,}}} \\\\")
lines += [
    r"\midrule",
    r"\multicolumn{7}{l}{\emph{Panel B. Prospective hazard: P(striker out $\mid$ striker's own previous ball), \% (N)}} \\",
]
lab = {"dot": "Previous ball a dot", "run": "Previous ball 1--3 runs", "four": "Previous ball a four", "six": "Previous ball a six"}
for c in ["dot", "run", "four", "six"]:
    h1 = haz[(haz.inning == 1) & (haz.prev_cat == c)].iloc[0]
    h2 = haz[(haz.inning == 2) & (haz.prev_cat == c)].iloc[0]
    lines.append(
        f"{lab[c]} & \\multicolumn{{3}}{{c}}{{{100*h1['mean']:.2f} ({int(h1['size']):,})}} & "
        f"\\multicolumn{{3}}{{c}}{{{100*h2['mean']:.2f} ({int(h2['size']):,})}} \\\\"
    )
lines.append(
    f"Any previous ball & \\multicolumn{{3}}{{c}}{{{100*base[1]:.2f}}} & \\multicolumn{{3}}{{c}}{{{100*base[2]:.2f}}} \\\\"
)
lines += [r"\bottomrule", r"\end{tabular}"]
(TABLES / "replication.tex").write_text("\n".join(lines) + "\n")
num.write()
print(rep.round(4).to_string())
print(haz.round(4).to_string())
