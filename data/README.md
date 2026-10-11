# Data

`raw/deliveries.csv` and `raw/matches.csv` are the public IPL ball-by-ball and match files
(seasons 2008-2018). They are a public GitHub mirror of the Kaggle IPL dataset, copied from
`insaid2018/Term-1`:
`https://github.com/insaid2018/Term-1/tree/master/Data/Projects`
(`deliveries.csv` and `matches.csv`). The committed files match that mirror byte for byte.

The mirror repository publishes no licence file. The 2022 drafts cite the Kaggle notebook
"Let's play cricket" (https://www.kaggle.com/code/ash316/let-s-play-cricket), released under
the Apache 2.0 licence. That licence covers the notebook, not these CSV files. No dataset
licence is stated on the mirror.

| File | Rows | SHA-256 |
|---|---|---|
| deliveries.csv | 164,750 | 4712d49a3f65269b903ee3eff4012e33c612e4ae5a4927b07eef06038982b2e8 |
| matches.csv | 696 | f8490975c12fc76ed4962311a732eeda4b3c3c37523b792a806682c49a029f5a |

Seasons 2008-2017 of this file contain exactly 150,460 deliveries from 636 matches. That is the sample
used by the 2022 draft in `legacy/`.

`derived/panel.parquet` is rebuilt by `analysis/build_panel.py` and is not committed.
