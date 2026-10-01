# Stage A: style space

## Purpose
Reduce ten per-36 rates and shot-mix shares to a few interpretable axes, check that the axes mean the same thing
across eras, and learn how much of each axis is really talent.

## Inputs
data/box_raw.rds from 01. Optional data/player_season.rds from 03 for the efficiency check.

## Variables (style_vars in style_helpers.R)
fga36, fg3a_rate, fta_rate, ast36, tov36, orb36, drb36, stl36, blk36, pf36.
Shooting efficiency is left out on purpose so style stays separate from the outcome in the state-space model.

## Steps
| Script | What it does | What to read |
|---|---|---|
| 10_style_rates.R | Standardize columns, team margins from player points, player-team-season rates | Game check (2 teams per game), margin by season near 0, rate quantiles, extreme values |
| 11_style_pca.R | Era z-scores, fit on reference period, project all seasons | Skew table, scree, loadings and names, position means, talent correlations, stability matrix, extreme players |

## Design choices and why
- **Per-36 rates** put bench and starters on one scale.
- **Z-score within season** stops era drift from becoming a component.
- **Fit on 2003 to 2012, project the rest.** Axes stay fixed so later seasons are comparable.
- **Minutes floor of 400** on a player-team stint. Noisy low-minute rates would otherwise distort the axes.
  Raise it if the extreme-players list shows small-sample oddities.
- **Square root on stl36, blk36, orb36** to tame skew. Adjust after reading the skew table.
- **Stints, not player-seasons,** because Stage B needs team membership. Traded players appear once per team.

## Checks that tell you it worked
- First three or four components explain a sensible share and each has loadings you can name in a phrase.
- Position means separate in the expected direction (for example centers high on rebounds and blocks).
- The stability matrix has large diagonal values, for example above 0.9, and small off-diagonals.
- Extreme-player lists read like basketball.

## Checks that tell you to stop and fix something
- A component with no nameable meaning. Reduce k or try a rotation.
- Stability diagonal below about 0.8. The axes are not the same across eras. Consider era-specific fits.
- A component that correlates strongly with plus-minus or efficiency. It is talent-heavy. Stage B handles this.

## Outputs
data/player_team_season.rds, data/team_season_margin.rds, data/style_scores.rds,
output/11_scree.png, output/11_loadings.png, output/11_loadings.csv, output/pc_talent_cor.rds, output/pc_ref.rds.

## Not done yet
- Noise-aware scores (shrink low-minute rates, or a factor model with measurement error).
- Rotation such as varimax for easier naming.
- Shot-location or tracking inputs.
