# Player Value Metrics

Code behind a set of NBA player-value models built for a Substack piece comparing
how different ways of measuring "how good is this player" can disagree, sometimes
sharply, about the same guy. Five approaches, increasing in how much they lean on
play-by-play data versus box-score data.

## The approaches

1. **Single-year RAPM, and an 11-year recency-decayed version of it**
   (`metrics/01_single_year_and_decayed_rapm.ipynb`)
   Regularized Adjusted Plus-Minus: ridge regression on possession-level lineup
   data (five offensive player IDs, five defensive player IDs, and the points
   scored on that possession) estimating each player's marginal effect on point
   differential, independent of box-score stats entirely. This notebook builds
   both the plain single-season version and a second version that pools 11 years
   of possessions with a recency decay, so older seasons count for less.

2. **Box-score-only model** (`metrics/02_box_score_only.ipynb`)
   No possession-level data at all. Team-level box score rate stats are
   regressed (ridge regression again) against team offensive/defensive rating,
   then that same fitted relationship is applied to each player's own per-100
   rate stats, in the spirit of Basketball-Reference's BPM.

3. **Prior-informed (Bayesian) RAPM** (`metrics/03_prior_informed_rapm.ipynb`)
   An empirical-Bayes blend of #1 and #2: a player's current-season RAPM is
   shrunk toward a box-score-derived prior (scaled to the same units via a
   calibration regression), with the shrinkage weight determined by how many
   possessions the player has actually logged this season.

4. **Recency-weighted, prior-informed RAPM** (`metrics/04_recency_weighted_rapm.ipynb`)
   The same empirical-Bayes idea as #3, but both halves now run on a rolling
   clock instead of a single season: the RAPM side pools 11 years of
   decayed possessions (from #1) instead of one season, and the box-score prior
   decays continuously from real game dates (roughly a 90-day half-life) instead
   of anchoring on the prior season.

## `data_collection/`

The three notebooks that pull the raw inputs (game logs, listed positions, and
in-season traded-player stint boundaries) the metrics notebooks above build from.


## Data

This repo is code only, no data files. The metrics notebooks read from a local
`data/` folder (teamstats/teamtotals CSVs, possession-level RAPM inputs, and a
player-stats reference file) that isn't included here, since it's large, mostly
re-derivable by running the `data_collection/` notebooks plus a possession-level
play-by-play pull, and feeds into a separate private Shiny app's own data
pipeline. If you want to run these yourself, you'll need to point `DATA_DIR`,
`POSS_DIR`, and `PS_PATH` (near the top of each metrics notebook) at your own
copies of that data.


