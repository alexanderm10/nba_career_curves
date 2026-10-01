# Stage C: feeding style into the state-space model

## Question
Does a player's previous style improve the forecast of his next seasons, and can team context be separated from talent?

## What is built (scripts 13 to 15)
- **13_lagged_style.R** attaches each player's previous style scores to his rows, builds style as of the holdout
  cutoff, and counts team changes.
- **14_state_space_cov.stan** is the state-space model with covariates on the mean.
  y = curve(experience) + X * beta + permanent level + transient form + noise.
  X holds the lagged style scores and a no-lag indicator.
- **ss_cov_helpers.R** and **15_fit_ss_cov.R** fit on training seasons, forecast the holdout, and compare with the
  base model from 09 on identical rows.

## Leakage rules (important)
- Style is built from box stats. Using same-season style to explain same-season efficiency is partly circular.
  Training rows therefore use the previous season's style only.
- For held-out seasons every forecast uses style as of the last training season. A forecast made at the cutoff would
  not know later style.
- Rookies have no previous style. They get zero scores plus a no-lag indicator so the model does not misread them.

## How to read 15
- **Beta table.** Covariates with 90 percent intervals clearly away from zero are the ones to report.
- **Scores table.** state_space_style versus state_space on the same rows. A gain smaller than run-to-run noise means
  style is descriptive here, not a forecasting gain.
- **Coverage.** Both models should land near 0.90.

## Not built: team context as a habitat effect
Idea. A player's transient form depends on teammates' style and team quality, so talent is separated from environment.
Identification. Players who change teams supply the within-player variation, like transplants in an ecology experiment.
Gate. 13 prints how many players change teams. If that is small, skip it.
If it is large enough, the next step is a team-context covariate for each player-season, built from minutes-weighted
teammate style excluding the player himself, then added to X.
Risks. Teammate style and team quality are entangled with who the team chooses to keep. Report as an association.

## Other extensions
- Style by experience interaction, asking whether development slopes differ by style.
- Joint model that carries PC score uncertainty into the state-space stage.
- Multivariate state space on the PC scores themselves, tracking players through style space over time.
- Reverse direction. Use latent talent from the state-space model to ask whether strong teams hold more talent or had
  better shooting luck.
