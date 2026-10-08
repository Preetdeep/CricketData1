"""Descriptive statistics table."""
import pandas as pd

from common import PANEL, TABLES, Numbers

num = Numbers("desc")
f = pd.read_parquet(PANEL)
s = f[f.balls_faced_before > 0]

rows = [
    ("Faced deliveries", lambda x: f"{len(x):,}"),
    ("Matches", lambda x: f"{x.match_id.nunique():,}"),
    ("Dismissal, bowler credited (\\%)", lambda x: f"{100 * x.striker_out.mean():.2f}"),
    ("\\quad of which caught (\\%)", lambda x: f"{100 * x.out_caught.mean():.2f}"),
    ("\\quad of which bowled, lbw or hit wicket (\\%)", lambda x: f"{100 * x.out_bowled_lbw.mean():.2f}"),
    ("\\quad of which stumped (\\%)", lambda x: f"{100 * x.out_stumped.mean():.2f}"),
    ("Striker run out (\\%)", lambda x: f"{100 * x.striker_runout.mean():.2f}"),
    ("Dot ball, 0 off the bat (\\%)", lambda x: f"{100 * (x.batsman_runs == 0).mean():.2f}"),
    ("1--3 runs off the bat (\\%)", lambda x: f"{100 * x.batsman_runs.isin([1, 2, 3]).mean():.2f}"),
    ("Four (\\%)", lambda x: f"{100 * (x.batsman_runs == 4).mean():.2f}"),
    ("Six (\\%)", lambda x: f"{100 * (x.batsman_runs == 6).mean():.2f}"),
    ("Runs off the bat per ball", lambda x: f"{x.batsman_runs.mean():.3f}"),
    ("Balls already faced by striker", lambda x: f"{x.balls_faced_before.mean():.1f}"),
    ("Wickets down", lambda x: f"{x.wkts_before.mean():.2f}"),
    ("Required run rate (chases)", lambda x: f"{x.rrr.mean():.2f}" if x.rrr.notna().any() else "--"),
]
cols = [("All", s), ("First innings", s[s.inning == 1]), ("Second innings", s[s.inning == 2])]
lines = [r"\begin{tabular}{lccc}", r"\toprule", " & " + " & ".join(c for c, _ in cols) + r" \\", r"\midrule"]
lines.append(r"\multicolumn{4}{l}{\emph{Panel A. Outcome on the current ball}} \\")
for i, (lab, fn) in enumerate(rows):
    if i == 12:
        lines.append(r"\multicolumn{4}{l}{\emph{Panel B. State before the ball}} \\")
    lines.append(f"{lab} & " + " & ".join(fn(x) for _, x in cols) + r" \\")
lines.append(r"\multicolumn{4}{l}{\emph{Panel C. Striker's previous ball (\%)}} \\")
for c, lab in [("dot", "Dot"), ("run", "1--3 runs"), ("four", "Four"), ("six", "Six")]:
    lines.append(f"{lab} & " + " & ".join(f"{100 * (x.prev_cat == c).mean():.2f}" for _, x in cols) + r" \\")
lines += [r"\bottomrule", r"\end{tabular}"]
(TABLES / "descriptives.tex").write_text("\n".join(lines) + "\n")

num.add("ShareDotPrev", 100 * (s.prev_cat == "dot").mean(), "{:.1f}")
num.add("ShareSixPrev", 100 * (s.prev_cat == "six").mean(), "{:.1f}")
num.add("ShareFourPrev", 100 * (s.prev_cat == "four").mean(), "{:.1f}")
num.add("ShareCaught", 100 * s.out_caught.sum() / s.striker_out.sum(), "{:.0f}")
num.write()
