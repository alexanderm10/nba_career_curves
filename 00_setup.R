# 00_setup.R
# Shared settings. Every other script starts with source("00_setup.R").
# Change things here, not in the individual scripts.

pkgs <- c("hoopR", "dplyr", "ggplot2", "mgcv", "lme4", "brms")
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  stop("Install these first: ", paste(missing_pkgs, collapse = ", "),
       "\n(brms also needs a working Stan toolchain, see mc-stan.org/rstan)")
}
invisible(lapply(pkgs, library, character.only = TRUE))

cfg <- list(
  # hoopR season = the year the season ends. Start at 2002 so that
  # players first seen in 2003 or later can be treated as true rookies.
  seasons       = 2002:hoopR::most_recent_nba_season(),
  first_rookie  = 2003,     # debut season must be >= this to count as a rookie
  min_tsa       = 150,      # minimum true shooting attempts in a player-season
  max_exp       = 12,       # drop the thin tail of very long careers
  min_seasons   = 2,        # seasons per player needed to keep them
  exp_center    = 3,        # centering value for experience
  holdout_from  = 2023,     # seasons >= this are held out in 06
  seed          = 2026,
  ss_draws      = 200,      # posterior draws used for state-space forecasts in 09
  se_scale      = 1.136,    # multiplier on the binomial se_ts. From the split-half check in
                            # qa_03_player_season.R (observed/formula variance 1.29, flat across
                            # attempt bins). Points per attempt is noisier than a 0/1 trial.

  # ---- style space and team study (scripts 10 to 15) ----
  style_min_min   = 400,    # minimum minutes for a player-team-season to get a style score
  style_sqrt      = c("stl36", "blk36", "orb36"),  # rates square-rooted before scaling (check skew in 11)
  style_ref_from  = 2003,   # PCA is fit on this reference period, then all seasons are projected
  style_ref_to    = 2012,
  style_late_from = 2016,   # late period used only for the loading-stability check
  style_k         = 3,      # chosen after 11: PC4 unnamed, eigenvalue 0.87, era stability 0.61
  tier_n          = 8,      # top and bottom teams per season in the tier comparison
  era_cuts        = c(2002, 2009, 2016, 2100),   # early 2002-08, mid 2009-15, late 2016+ (left-closed)
  talent_cor      = 0.30,   # a PC correlating this strongly with impact or efficiency is flagged talent-heavy
  n_perm          = 999,    # permutations for PERMANOVA tests
  dir_data      = "data",
  dir_out       = "output"
)

# Column names in hoopR::load_nba_player_box(). If a pull fails the check in
# 02_inspect.R, this is the one place to fix the mapping.
cols <- list(
  id          = "athlete_id",
  name        = "athlete_display_name",
  season      = "season",
  season_type = "season_type",   # 2 = regular season
  minutes     = "minutes",
  points      = "points",
  fga         = "field_goals_attempted",
  fta         = "free_throws_attempted",
  dnp         = "did_not_play"
)

# Additional columns for the style and team work (scripts 10 onward). pm and pos are
# optional; related checks are skipped if they are absent. Verify against names(box).
cols2 <- list(
  game_id = "game_id",
  team_id = "team_id",
  fg3a    = "three_point_field_goals_attempted",
  oreb    = "offensive_rebounds",
  dreb    = "defensive_rebounds",
  ast     = "assists",
  stl     = "steals",
  blk     = "blocks",
  tov     = "turnovers",
  pf      = "fouls",
  pm      = "plus_minus",
  pos     = "athlete_position_abbreviation"
)

# hoopR labels All-Star and Rising Stars games as season_type 2. Those "teams" (East/West
# All-Stars, Team LeBron, World, USA, ...) play a handful of games in total, while real
# franchises play well over 100. Drop any game involving a team below that threshold.
drop_exhibition <- function(box, min_team_games = 100) {
  reg <- box[box[[cols$season_type]] == 2, ]
  g   <- unique(reg[, c(cols2$game_id, cols2$team_id)])
  n   <- table(g[[cols2$team_id]])
  bad <- names(n)[n < min_team_games]
  drop <- box[[cols2$team_id]] %in% bad | box$opponent_team_id %in% bad
  message("drop_exhibition: removing ", sum(drop), " rows from ",
          length(unique(box[[cols2$game_id]][drop])), " exhibition games")
  box[!drop, ]
}

dir.create(cfg$dir_data, showWarnings = FALSE)
dir.create(cfg$dir_out,  showWarnings = FALSE)
