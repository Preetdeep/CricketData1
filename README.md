# Pressure, Overconfidence, or Neither?

Ball-by-ball evidence on dismissal risk and batting aggression in Twenty20 cricket (IPL 2008-2018).

**Manuscript:** `paper/main.pdf` (source `paper/main.tex`).

## Findings

1. The 2022 draft's retrospective result (dots and boundaries over-represented before wickets) is a
   selection artefact. Its parity filter removes singles and threes from the pre-wicket sample but not
   from the baseline.
2. Asked prospectively, with match-state, batter, bowler and venue fixed effects, the striker's previous
   ball (dot, four, six) does not predict dismissal. Equivalence tests rule out effects above 10% of the
   base rate after a dot or a six. There is no innings or chase-pressure pattern after a multiple-testing
   correction.
3. Batters do respond to the previous ball. Boundary rates rise after a dot and, more strongly, after a
   six. A within-innings permutation test (Miller-Sanjurjo-style bias correction) shows this response
   comes from the order of balls, not from the day's conditions.

## Reproduce

```bash
pip install -r requirements.txt    # Python 3.11+
make analysis                      # panel, tables, figures, numbers (~50 min: 200-draw permutation)
make paper                         # needs a TeX distribution with latexmk
```

Set `PERM_R=20` for a quick run of `analysis/momentum.py`.

## Layout

| Path | Contents |
|---|---|
| `analysis/build_panel.py` | Raw files to delivery-level panel (striker history, match state, chase state) |
| `analysis/descriptives.py` | Table 1 |
| `analysis/replicate_original.py` | Reproduces the 2022 draft's design, adds the like-for-like baseline |
| `analysis/main_models.py` | Dismissal LPMs, innings and chase interactions |
| `analysis/robustness.py` | Robustness table, logit, equivalence tests, MDEs, Benjamini-Hochberg |
| `analysis/momentum.py` | Behavioural outcomes, FE ladder, permutation null |
| `analysis/figures.py` | Figures 1-6 |
| `output/` | Generated tables (`.tex`), figures (`.pdf`), numbers (`.tex` macros used in the text) |
| `legacy/` | The original 2022 R script and R Markdown drafts, kept for provenance |

Every number quoted in the manuscript is a LaTeX macro written by the pipeline, so text and results
cannot diverge.
