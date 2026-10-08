"""Build the delivery-level analysis panel from the raw IPL ball-by-ball files.

Unit of observation: a delivery faced by a striker (wides excluded, since a wide
is not a ball faced). Each row carries the match state before the ball, the
striker's own history in the innings, and the outcome of the striker's previous
ball faced.
"""
import numpy as np
import pandas as pd

from common import BOWLER_DISMISSALS, PANEL, RAW, Numbers

num = Numbers("build")

d = pd.read_csv(RAW / "deliveries.csv")
m = pd.read_csv(RAW / "matches.csv")
num.add("RawDeliveries", len(d), "{:,}")
num.add("RawMatches", d.match_id.nunique(), "{:,}")

# File order is the scorer's order of deliveries. Keep it explicitly.
d["seq"] = np.arange(len(d))

# Match 7946 (2018) contains four rows that exactly duplicate an earlier row of
# the same innings, one of them out of sequence (over 13 recorded inside over 14).
dup = d.drop(columns="seq").duplicated() & (d.match_id == 7946)
num.add("DroppedDuplicates", int(dup.sum()), "{:,}")
d = d[~dup]

d = d.merge(
    m[["id", "season", "venue", "dl_applied", "result", "winner"]].rename(columns={"id": "match_id"}),
    on="match_id",
    how="left",
    validate="many_to_one",
)
assert d.season.notna().all()

# Regulation innings only (drops super overs).
d = d[(d.inning <= 2) & (d.is_super_over == 0)].copy()
d = d.sort_values(["match_id", "inning", "seq"]).reset_index(drop=True)

d["wide"] = d.wide_runs > 0
d["noball"] = d.noball_runs > 0
d["legal"] = ~d.wide & ~d.noball

# Dismissal coding.
kind = d.dismissal_kind.fillna("")
d["striker_out_any"] = d.player_dismissed.eq(d.batsman)
d["striker_out"] = d.striker_out_any & kind.isin(BOWLER_DISMISSALS)
d["striker_runout"] = d.striker_out_any & kind.eq("run out")
d["out_caught"] = d.striker_out & kind.isin({"caught", "caught and bowled"})
d["out_bowled_lbw"] = d.striker_out & kind.isin({"bowled", "lbw", "hit wicket"})
d["out_stumped"] = d.striker_out & kind.eq("stumped")
d["any_wicket"] = d.player_dismissed.notna()

# Team state before the ball.
g = d.groupby(["match_id", "inning"], sort=False)
d["team_runs_before"] = g.total_runs.cumsum() - d.total_runs
d["wkts_before"] = g.any_wicket.cumsum() - d.any_wicket.astype(int)
d["legal_before"] = g.legal.cumsum() - d.legal.astype(int)
d["balls_left"] = 120 - d.legal_before
d["phase"] = pd.cut(d.over, [0, 6, 15, 20], labels=["powerplay", "middle", "death"])

# Chase state. Target = first-innings total + 1. Rain-affected (D/L) matches
# have revised targets that the data do not record, so they get no chase state.
first = d[d.inning == 1].groupby("match_id").total_runs.sum().rename("first_inn_total")
d = d.merge(first, on="match_id", how="left")
chase = (d.inning == 2) & (d.dl_applied == 0)
d["target"] = np.where(chase, d.first_inn_total + 1, np.nan)
d["runs_req"] = d.target - d.team_runs_before
d["rrr"] = np.where(chase & (d.balls_left > 0), 6 * d.runs_req / d.balls_left.clip(lower=1), np.nan)
d["crr"] = np.where(d.legal_before > 0, 6 * d.team_runs_before / d.legal_before.clip(lower=1), np.nan)

# Faced deliveries only from here on.
f = d[~d.wide].copy()
f = f.sort_values(["match_id", "inning", "seq"]).reset_index(drop=True)

# Striker history within the innings.
gb = f.groupby(["match_id", "inning", "batsman"], sort=False)
f["balls_faced_before"] = gb.cumcount()
f["bat_runs_before"] = gb.batsman_runs.cumsum() - f.batsman_runs
f["prev_runs"] = gb.batsman_runs.shift(1)
f["prev_seq"] = gb.seq.shift(1)
f["prev_over"] = gb.over.shift(1)
f["prev_bowler"] = gb.bowler.shift(1)
f["prev_extras"] = gb.extra_runs.shift(1)

# Previous faced delivery of the innings, whoever faced it.
gi = f.groupby(["match_id", "inning"], sort=False)
f["inn_prev_striker"] = gi.batsman.shift(1)
f["inn_prev_seq"] = gi.seq.shift(1)
f["inn_prev_runs"] = gi.batsman_runs.shift(1)
f["inn_prev_total"] = gi.total_runs.shift(1)
f["inn_prev_wicket"] = gi.any_wicket.shift(1)

# The striker's previous ball was also the previous faced delivery of the innings.
f["consecutive"] = f.prev_seq.notna() & f.prev_seq.eq(f.inn_prev_seq)
f["same_over"] = f.prev_over.eq(f.over)
f["same_bowler"] = f.prev_bowler.eq(f.bowler)

# Previous-ball outcome categories, from the striker's point of view.
def categorise(r):
    return np.select(
        [r.isna(), r == 0, r == 4, r == 6],
        ["none", "dot", "four", "six"],
        default="run",  # 1, 2, 3, 5 or 7 runs off the bat
    )

f["prev_cat"] = categorise(f.prev_runs)
f["prev_dot"] = (f.prev_cat == "dot").astype(int)
f["prev_four"] = (f.prev_cat == "four").astype(int)
f["prev_six"] = (f.prev_cat == "six").astype(int)
f["prev_boundary"] = f.prev_four | f.prev_six

# Run lengths of dots and boundaries ending at the previous ball.
def run_length(flag, keys):
    out = np.zeros(len(flag), dtype=int)
    prev_key, streak = None, 0
    for i, (k, x) in enumerate(zip(keys, flag)):
        if k != prev_key:
            streak = 0
            prev_key = k
        out[i] = streak
        streak = streak + 1 if x else 0
    return out

keys = list(zip(f.match_id, f.inning, f.batsman))
f["dot_streak"] = run_length((f.batsman_runs == 0).to_numpy(), keys)
f["bdry_streak"] = run_length(f.batsman_runs.isin([4, 6]).to_numpy(), keys)

# Outcome flags on the current ball.
f["cur_dot"] = (f.batsman_runs == 0) & ~f.striker_out_any
f["cur_boundary"] = f.batsman_runs.isin([4, 6])
f["cur_six"] = f.batsman_runs == 6

# Coarse bins for flexible controls.
f["bf_bin"] = pd.cut(
    f.balls_faced_before, [-1, 0, 1, 2, 3, 4, 5, 7, 10, 15, 20, 30, 45, 200],
    labels=["0", "1", "2", "3", "4", "5", "6-7", "8-10", "11-15", "16-20", "21-30", "31-45", "46+"],
).astype(str)
f["br_bin"] = pd.cut(
    f.bat_runs_before, [-1, 0, 5, 10, 20, 30, 40, 50, 75, 300],
    labels=["0", "1-5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-75", "76+"],
).astype(str)
f["rrr_bin"] = pd.cut(f.rrr, [-np.inf, 6, 8, 10, 12, np.inf], labels=["<6", "6-8", "8-10", "10-12", "12+"]).astype(str)
f["inn2"] = (f.inning == 2).astype(int)
f["over_inn"] = f.over.astype(str) + "_" + f.inning.astype(str)
f["innings_id"] = f.match_id.astype(str) + "_" + f.inning.astype(str)
f["original_sample"] = (f.season <= 2017).astype(int)

for c in ["striker_out", "striker_out_any", "striker_runout", "out_caught", "out_bowled_lbw",
          "out_stumped", "cur_dot", "cur_boundary", "cur_six", "consecutive", "same_over", "same_bowler"]:
    f[c] = f[c].astype(int)

keep = [
    "match_id", "inning", "season", "venue", "dl_applied", "seq", "over", "ball", "phase",
    "batting_team", "bowling_team", "batsman", "non_striker", "bowler",
    "batsman_runs", "extra_runs", "total_runs", "legal", "noball", "dismissal_kind",
    "striker_out", "striker_out_any", "striker_runout", "out_caught", "out_bowled_lbw", "out_stumped",
    "cur_dot", "cur_boundary", "cur_six",
    "team_runs_before", "wkts_before", "legal_before", "balls_left", "target", "runs_req", "rrr", "crr",
    "first_inn_total", "balls_faced_before", "bat_runs_before",
    "prev_runs", "prev_extras", "prev_cat", "prev_dot", "prev_four", "prev_six", "prev_boundary",
    "consecutive", "same_over", "same_bowler", "dot_streak", "bdry_streak",
    "inn_prev_striker", "inn_prev_runs", "inn_prev_total", "inn_prev_wicket",
    "bf_bin", "br_bin", "rrr_bin", "inn2", "over_inn", "innings_id", "original_sample",
]
f = f[keep]
f["phase"] = f.phase.astype(str)
f["prev_boundary"] = f.prev_boundary.astype(int)
f.to_parquet(PANEL, index=False)

# Sample statistics for the paper.
num.add("Seasons", f"{f.season.min()}--{f.season.max()}")
num.add("Matches", f.match_id.nunique(), "{:,}")
num.add("Deliveries", len(d), "{:,}")
num.add("FacedDeliveries", len(f), "{:,}")
num.add("BowlerDismissals", int(f.striker_out.sum()), "{:,}")
num.add("RunOuts", int(d[d.dismissal_kind == "run out"].shape[0]), "{:,}")
num.add("Batters", f.batsman.nunique(), "{:,}")
num.add("Bowlers", f.bowler.nunique(), "{:,}")
num.add("DLMatches", int(m.dl_applied.sum()), "{:,}")
s = f[f.balls_faced_before > 0]
num.add("AnalysisN", len(s), "{:,}")
num.add("AnalysisOuts", int(s.striker_out.sum()), "{:,}")
num.add("AnalysisOutRate", 100 * s.striker_out.mean())
num.add("ConsecutiveShare", 100 * s.consecutive.mean(), "{:.1f}")
num.add("FirstBallOuts", int(f[f.balls_faced_before == 0].striker_out.sum()), "{:,}")
num.write()
print(f"panel: {len(f):,} faced deliveries, {f.striker_out.sum():,} bowler-credited dismissals")
