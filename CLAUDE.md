# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An R + Stan analysis pipeline on NBA box scores (via hoopR) studying player development, playing style, and team context. The output is a public write-up plus code. The reasoning and decision gates are in `docs/PLAN.md` (read first). Stage notes are in `docs/STAGE_A_style_space.md`, `docs/STAGE_B_team_study.md`, and `docs/STAGE_C_integration.md`.

The full pipeline (01–15) was first run on 2026-09-30. Run logs are in `output/logs/` and QA figures in `output/qa/`.

## Running

There is no build, test suite, or linter. Run the numbered scripts in order from this folder, since all paths are relative:

```bash
Rscript 03_player_season.R
```

Each script prints checks and saves its output. Read the checks before you run the next script. To rerun one step, run that script alone. It reads its inputs from `data/` or `output/`, which the earlier scripts wrote.

Requirements: dplyr, ggplot2, mgcv, lme4, brms, and hoopR for 01–06. cmdstanr plus CmdStan (installed at `~/.cmdstan`) for 05, 06, 08, 09 and 15. vegan and permute for 12. brms calls must use `backend = "cmdstanr"`, because the rstan backend fails to load on this Mac (TBB symbol error).

Run times: the brms fits in 05 and 06 take about 10–15 minutes each, and the Stan fits in 08, 09 and 15 take about 15–25 minutes each. Run them in the background. brms caches its fits in `output/m_brms*.rds`, so delete those files to force a refit after the data changes.

## Architecture

- **`00_setup.R` is the single source of configuration.** Every script calls `source("00_setup.R")` first. It loads the packages and defines `cfg` (seasons, filters, holdout cutoff, seed, style/PCA settings, `data`/`output` dirs), `cols` (hoopR column map for 01–09), and `cols2` (extra columns for 10 onward). Change thresholds and column names here, never in the individual scripts.
- **Scripts pass data through RDS files, not shared state.** Raw and derived data go in `data/` (`box_raw.rds` → `player_season.rds`; `player_team_season.rds`, `team_season_margin.rds`, `style_scores.rds`, `style_asof.rds`, `ps_style.rds`). Models and results go in `output/`. Some dependencies cross stages: 15 needs `output/holdout_predictions_ss.rds` from 09, 13 needs outputs from both 03 and 11, and 11 and 12 read some files only if they exist.
- **Data quirks handled in `00_setup.R`.** hoopR labels All-Star and Rising Stars games as regular season (`season_type == 2`). `drop_exhibition()` removes them, and any script that reads `box_raw.rds` should wrap it in this function. Debut means the first regular-season game played, not the first season over `min_tsa`.
- **Helper files** (`ss_helpers.R`, `ss_cov_helpers.R`, `style_helpers.R`) are sourced after `00_setup.R`. They use name prefixes: `ss_*` for state-space, `ss_cov_*` for the covariate version, and `sty_*` for style.
- **The state-space model is fit in marginal form in Stan, and the latent states are recovered in R.** `07_state_space.stan` integrates out the permanent level `alpha_j` and the AR(1) transient form `z_jt`. That leaves a per-player multivariate normal with covariance `tau_a^2 + tau_z^2 * phi^|Δexp|`, plus the known sampling error `se^2` and an extra `sigma^2` on the diagonal. Rows must be sorted by player, then experience (`start`/`len` index into them). Latent states and forecasts come from the conditional-normal formulas in `ss_helpers.R`, applied to posterior draws (`cfg$ss_draws`). `14_state_space_cov.stan` and `ss_cov_helpers.R` are the same model with lagged-style covariates added to the mean.
- **Outcome:** league-adjusted true shooting (`ts_rel`, which uses `lg_ts`). The sampling error `se_ts` is the binomial formula multiplied by `cfg$se_scale`, which was calibrated by the split-half check in `qa_03_player_season.R`. `08` asserts that these columns exist, so rerun `03` if it fails.
- **Holdout discipline.** Seasons from `cfg$holdout_from` onward are held out. Every model is scored against simple rivals (naive carry-forward, league-adjusted naive, GAM curve) with weighted RMSE and interval coverage. In 13 and 15, holdout rows use style *as of the last training season* to avoid leakage.
- **Style PCA** (11) is fit on the reference seasons `style_ref_from`–`style_ref_to` using era-standardized per-36 rates (the `style_sqrt` rates are square-rooted first). All seasons are then projected onto it. `cfg$style_k` is chosen by hand from the scree plot and loadings.

## Project conventions (from docs/PLAN.md)

- Small scripts with one job each. Print and save the intermediate outputs.
- Limited tidyr. Use dplyr only for grouped summaries and base R for reshaping.
- Every modeling step is scored against simple rivals on the holdout. Show uncertainty and report coverage.
- Respect the decision gates in PLAN.md, for example: do not build the team-context model if team changes are few, and report style as descriptive if it does not improve the holdout. Make no claim the results don't support, and no causal claims about style and winning.
- When adding a script, update the run-order list in `README.md`.
