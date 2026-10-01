# Project plan: development, style, and team context

## Why this project exists
Show, step by step and in public, how to make decisions under uncertainty and forecast from imperfect data, and how to
turn models into something a coaching staff can use. Intermediate outputs stay visible.

## The idea in one paragraph
A player's observed season is a noisy view of latent processes. Permanent talent, transient form, and sampling luck
all sit under the box score. Style (what a player does on the floor) and team context (who he plays with) are
candidate drivers of those processes. The work asks whether latent structure forecasts better than simple rules,
whether style explains development, and whether strong and weak teams occupy different regions of style space.
This is an ecosystem framing. Individuals are nested in communities, communities change over eras, and what we see is
a noisy observation of what is underneath.

## Questions, in order
1. **Q1, development.** How do true shooting and related efficiency develop with experience, once league drift and
   sampling luck are removed? (Scripts 01 to 09. Built.)
2. **Q2, style.** What are the main axes along which players differ, and are they stable across eras? (Scripts 10, 11.)
3. **Q3, teams.** Do strong teams differ from weak teams in style centroid or roster variety, and does that gap
   change over time? (Script 12.)
4. **Q4, integration.** Does a player's previous style improve the state-space forecast? Can team context be separated
   from talent? (Scripts 13 to 15. Team context is documented but not built.)

## Stage map
| Stage | Scripts | Notes file | Output to read first |
|---|---|---|---|
| Baseline and state space | 00 to 09, 07 Stan | README.md | 09 scores table |
| A. Style space | 10, 11 | STAGE_A_style_space.md | 11 scree, loadings, stability matrix |
| B. Team study | 12 | STAGE_B_team_study.md | 12 style gap plot, PERMANOVA table |
| C. Integration | 13, 14, 15 | STAGE_C_integration.md | 15 scores table, beta intervals |

## Decision gates
- After 02 and 10. If column names fail, fix the maps in 00_setup.R before anything else.
- After 09. If the state-space model does not beat "last season carried forward" on the holdout, say so plainly and
  ask why (short careers, thin data, too little persistence) before adding more structure.
- After 11. Pick cfg$style_k from the scree and loadings. Do not keep components that cannot be named.
- After 12. If the spread test (p_spread) is small, read the location test with care. If the talent-heavy rerun erases
  the effect, the finding is about quality and not style, and the write-up should say that.
- After 13. If team changes are few, do not build the team-context model.
- After 15. If style does not improve the holdout, report it as descriptive.

## Standing rules for this project
- Small scripts, one job each, intermediate outputs printed and saved.
- Limited tidyr. dplyr for grouped summaries only. Base R for reshaping.
- Every modeling step is scored against simple rivals on a holdout.
- Uncertainty is shown, not hidden. Interval coverage is reported.
- No claim in the write-up that the results do not support.

## Known risks
- **Column names.** All hoopR column names are assumed and unverified. Scripts 02 and 10 check them.
- **Untested code.** Nothing here has run in R or Stan. The state-space math was checked on simulated data in Python.
  Expect a round of small fixes on the first real run.
- **Talent versus style.** Usage and scoring-volume components partly track talent. Stage B reruns without them.
- **Causation.** Team style and team quality are associated in this design. Nothing here shows that changing style
  changes winning. Keep that out of the write-up.
- **Survivorship.** Players with long careers are a selected group. Later experience years are thin.
- **Holdout optimism.** The holdout uses the realized league TS% and realized attempts of the held-out season.
- **Two-stage error.** PC scores enter the state-space model as if known exactly.
- **Tracking data.** Box scores are a coarse view of style. Tracking and video-derived features would be better.
  Say that openly and treat this work as a method demonstration.

## What "done" looks like
- A short public write-up with three figures: the development curve with uncertainty, the variance split
  (permanent talent, transient form, sampling luck), and the style gap over time.
- One honest paragraph on what did not work or did not add value.
- Code in a repository with the run order in README.md.
