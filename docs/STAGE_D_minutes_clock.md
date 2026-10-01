# Stage D: playing time as the development clock

## Question
Is a player's development better tracked by prior career minutes than by years since debut?
Two players in "year 2" can have 800 or 4,400 prior minutes. If development comes from playing time,
a minutes clock should fit within-player change better and forecast the holdout better.

## Why it came up
Years since debut is a coarse, calendar-based clock. It also ignores seasons cut short by injury,
lockout, or COVID, and seasons a player sat out. Prior minutes is continuous and can be built from
the box scores already pulled, with no new data.

## Design
- `cum_min_prior`: regular-season minutes in all games before the season, including seasons below
  `min_tsa`. Rookies are 0. Modeled as `lcm = log(1 + minutes / 1000)` because learning curves usually
  flatten. The GAM chooses the shape on that scale.
- Script 16 compares GAM curves on league-adjusted TS%: years only, minutes only, and both. It fits each
  pooled and with a player random effect (within-player). Comparison is by AIC and holdout.
- Holdout:
  - **Main:** the first held-out season only (`season == holdout_from`). Prior minutes are fully known
    at the cutoff, so this is a fair forecast test.
  - **Upper bound:** all held-out seasons with realized prior minutes. Optimistic, because a forecast made
    at the cutoff would not know the minutes coaches give in later seasons. Same spirit as the
    realized-league-TS% caveat in PLAN.md.
- State-space version (next step, only if script 16 shows a gain): put minutes terms on the mean via
  `14_state_space_cov.stan`. Form persistence stays in seasons.

## Interpretation limits
Coaches give minutes to players who look good, so prior minutes partly encode what the staff has
already seen. The player random effect absorbs some of that, not all. A forecasting gain means
"playing-time decisions carry information beyond years," not "playing time causes improvement."

## Decision gate
If minutes do not beat years on the main holdout rows, report it as descriptive and stop here.

## Results (first run, 2026-09-30)
- The two clocks correlate at 0.89, but they separate within each year. In year 3, prior minutes run from
  about 1,500 (10th percentile) to 6,600 (90th).
- Within-player fit (AIC, GAM with player random effect): years 0, minutes alone +62, both −18 relative
  to years alone. So years fits better than minutes. Given years, minutes adds a small linear term (p = 0.005)
  with a **negative** sign. At year 3, going from 1,500 to 6,600 prior minutes lowers TS% by about 0.6 points.
  This could be mileage or wear, or heavy-minute players taking harder shots. It is small, and the design
  cannot tell these apart.
- Holdout weighted RMSE, main rows (2023, n = 294): years 0.03347, minutes 0.03364, both 0.03341,
  state space 0.03247. The upper-bound rows give the same ordering.
- Side result: the flexible within-player curve also peaks around years 3–4 and declines after year 8.
  So the late decline in 08 is not just an artifact of the quadratic shape.

## Decision
Minutes do not beat years on the main holdout. Report the minutes clock as descriptive and do not build the
state-space version.
