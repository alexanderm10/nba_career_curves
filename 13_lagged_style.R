# 13_lagged_style.R
# Stage C, step 1. Attach each player's PREVIOUS style to their player-season rows so it
# can enter the state-space model without leaking the same season's box stats.
# Also counts how many players change teams, which decides whether a team-context
# effect can be estimated at all.

source("00_setup.R")
source("style_helpers.R")

sc  <- readRDS(file.path(cfg$dir_data, "style_scores.rds"))
ps  <- readRDS(file.path(cfg$dir_data, "player_season.rds"))
pts <- readRDS(file.path(cfg$dir_data, "player_team_season.rds"))
pcs <- paste0("pc", seq_len(cfg$style_k))

# ---- Step 1: one style row per player-season (minutes-weighted across team stints) ----------
pss <- sty_player_season_scores(sc, pcs)
pss <- pss[order(pss$player_id, pss$season), ]
message("Player-seasons with a style score: ", nrow(pss))

# ---- Step 2: previous available style row for each player ------------------------------------
for (v in pcs) {
  pss[[paste0("lag_", v)]] <- ave(pss[[v]], pss$player_id, FUN = function(x) c(NA, head(x, -1)))
}
pss$lag_season <- ave(pss$season, pss$player_id, FUN = function(x) c(NA, head(x, -1)))
pss$lag_gap    <- pss$season - pss$lag_season
print(table(pss$lag_gap, useNA = "ifany"))   # 1 is the usual case, NA is a first scored season

# ---- Step 3: attach to the modeling rows ---------------------------------------------------------
lagcols <- c(paste0("lag_", pcs), "lag_gap")
psx <- merge(ps, pss[c("player_id", "season", lagcols)], by = c("player_id", "season"), all.x = TRUE)
psx$no_lag <- as.integer(is.na(psx[[paste0("lag_", pcs[1])]]))
message("Rows with no prior style score: ", sum(psx$no_lag), " of ", nrow(psx))
print(table(experience = psx$exp, no_lag = psx$no_lag))   # rookies (exp 0) are all no_lag
for (v in paste0("lag_", pcs)) psx[[v]][is.na(psx[[v]])] <- 0   # no_lag flag carries the information
print(round(sapply(psx[paste0("lag_", pcs)], sd), 2))

# ---- Step 4: style as of the holdout cutoff, for forecasting in 15 -------------------------------
asof <- pss[pss$season < cfg$holdout_from, ]
asof <- asof[order(asof$player_id, asof$season), ]
asof <- asof[!duplicated(asof$player_id, fromLast = TRUE), c("player_id", pcs)]
names(asof)[-1] <- paste0("asof_", pcs)
message("Players with an as-of-cutoff style: ", nrow(asof))
saveRDS(asof, file.path(cfg$dir_data, "style_asof.rds"))

# ---- Step 5: how many players change teams? --------------------------------------------------------
pt <- pts[order(pts$player_id, pts$season, -pts$min), ]
prim <- pt[!duplicated(pt[c("player_id", "season")]), c("player_id", "season", "team_id")]
prim <- prim[order(prim$player_id, prim$season), ]
prim$prev_team   <- ave(prim$team_id, prim$player_id, FUN = function(x) c(NA, head(x, -1)))
prim$prev_season <- ave(prim$season,  prim$player_id, FUN = function(x) c(NA, head(x, -1)))
prim$moved <- !is.na(prim$prev_team) & prim$prev_season == prim$season - 1 &
  prim$team_id != prim$prev_team

mv <- merge(psx[c("player_id", "season", "exp")], prim[c("player_id", "season", "moved")],
            by = c("player_id", "season"))
message("Modeled player-seasons: ", nrow(mv), "  Team changes from the prior season: ", sum(mv$moved))
message("Distinct players with at least one change: ",
        length(unique(mv$player_id[mv$moved])), " of ", length(unique(mv$player_id)))
print(table(experience = mv$exp, moved = mv$moved))
message("Team changes are what identify a team-context effect separately from player talent.")
message("If this count is small, leave the team-context idea in docs/STAGE_C_integration.md for later.")

saveRDS(psx, file.path(cfg$dir_data, "ps_style.rds"))
