# 10_style_rates.R
# Stage A, step 1. Build two tables from the raw box scores already pulled in 01:
#   player_team_season.rds   per-36 rates and shot-mix shares for each player-team-season
#   team_season_margin.rds   mean point margin per game for each team-season (team quality)

source("00_setup.R")
source("style_helpers.R")

box <- drop_exhibition(readRDS(file.path(cfg$dir_data, "box_raw.rds")))

# ---- Step 1: columns present? -------------------------------------------------
sty_check_cols(box, cols2)

# ---- Step 2: standardize and keep regular season games played ----------------
std    <- sty_standardize_box(box, cols, cols2)
played <- sty_played(std)
message("Rows in: ", nrow(std), "  Regular season played rows: ", nrow(played))

# ---- Step 3: team margins from ALL played rows, before any stat filtering ------
print(sty_game_check(played))   # want nearly all games to have exactly 2 teams
tm <- sty_team_margin(played)
message("Team-seasons: ", nrow(tm), "  Seasons: ", length(unique(tm$season)))
print(table(tm$season))         # about 30 teams per season (fewer in early seasons)
print(summary(tm$games))        # about 82 games per team
print(round(tapply(tm$margin_pg, tm$season, mean), 3))   # each season should average about 0
saveRDS(tm, file.path(cfg$dir_data, "team_season_margin.rds"))

# ---- Step 4: rows with every counting stat, then player-team-season totals ----
comp <- sty_complete(played)
message("Rows dropped for missing counting stats: ", nrow(played) - nrow(comp),
        " (", round(100 * (1 - nrow(comp) / nrow(played)), 2), "%)")

pts <- sty_player_team_season(comp)
message("Player-team-seasons: ", nrow(pts))
print(table(table(paste(pts$player_id, pts$season))))   # stints per player-season, 1 is most

# ---- Step 5: rates -------------------------------------------------------------
pts <- sty_rates(pts)
elig <- pts[pts$min >= cfg$style_min_min & pts$fga > 0, ]
message("Rows meeting the minutes floor (", cfg$style_min_min, "): ", nrow(elig),
        " of ", nrow(pts))
print(round(sapply(elig[style_vars], quantile, probs = c(0, 0.01, 0.5, 0.99, 1)), 2))
# Extreme values to eyeball. A per-36 rate far outside normal basketball is a data problem.
print(utils::head(elig[order(-elig$blk36), c("player_name", "season", "min", "blk36")], 5))
print(utils::head(elig[order(-elig$fta_rate), c("player_name", "season", "min", "fta_rate")], 5))

saveRDS(pts, file.path(cfg$dir_data, "player_team_season.rds"))
