# 03_player_season.R
# Collapse game rows to player-seasons, compute true shooting %, its
# sampling error, and years of experience since debut.
#
# Metric: TS% = PTS / (2 * (FGA + 0.44 * FTA))
# Sampling error: se = sqrt(ts * (1 - ts) / tsa)
#   This treats each true shooting attempt as a Bernoulli trial on
#   (points / 2). It is an approximation. Check it in step 6 below.

source("00_setup.R")
box <- drop_exhibition(readRDS(file.path(cfg$dir_data, "box_raw.rds")))

# Step 1: standardize names and types in one small data frame
std <- data.frame(
  player_id   = as.character(box[[cols$id]]),
  player_name = box[[cols$name]],
  season      = as.integer(box[[cols$season]]),
  season_type = box[[cols$season_type]],
  minutes     = suppressWarnings(as.numeric(box[[cols$minutes]])),
  pts         = suppressWarnings(as.numeric(box[[cols$points]])),
  fga         = suppressWarnings(as.numeric(box[[cols$fga]])),
  fta         = suppressWarnings(as.numeric(box[[cols$fta]])),
  dnp         = box[[cols$dnp]] %in% TRUE,
  stringsAsFactors = FALSE
)
message("Step 1, rows in: ", nrow(std))

# Step 2: regular season games where the player played
reg <- std[std$season_type == 2 & !std$dnp & !is.na(std$minutes) & std$minutes > 0, ]
reg <- reg[!is.na(reg$pts) & !is.na(reg$fga) & !is.na(reg$fta), ]
message("Step 2, regular season played rows: ", nrow(reg))

# Step 3: player-season totals
ps <- reg |>
  dplyr::group_by(player_id, player_name, season) |>
  dplyr::summarise(
    games = dplyr::n(),
    min   = sum(minutes),
    pts   = sum(pts),
    fga   = sum(fga),
    fta   = sum(fta),
    .groups = "drop"
  )
message("Step 3, player-seasons: ", nrow(ps))

# Step 3b: league true shooting % by season, from ALL player-seasons before any
# filtering. Used to adjust for era drift (the three-point shift), which is
# otherwise confounded with experience.
lg <- ps |>
  dplyr::group_by(season) |>
  dplyr::summarise(lg_ts = sum(pts) / (2 * sum(fga + 0.44 * fta)), .groups = "drop")
print(as.data.frame(lg))

# Step 4: the metric and its sampling error
ps$tsa   <- ps$fga + 0.44 * ps$fta
ps$ts    <- ps$pts / (2 * ps$tsa)
message("Step 4, rows with zero attempts: ", sum(ps$tsa == 0))
ps <- ps[ps$tsa >= cfg$min_tsa, ]
message("Step 4, rows after min_tsa filter: ", nrow(ps))
message("Step 4, rows with ts outside (0,1): ", sum(ps$ts <= 0 | ps$ts >= 1))
ps <- ps[ps$ts > 0 & ps$ts < 1, ]
ps$se_ts_binom <- sqrt(ps$ts * (1 - ps$ts) / ps$tsa)
ps$se_ts <- ps$se_ts_binom * cfg$se_scale   # calibrated in qa_03_player_season.R

# League-adjusted TS: player minus league for that season. Used by the
# state-space model (07 to 09). The raw ts column is kept for 04 to 06.
ps <- merge(ps, lg, by = "season")
ps$ts_rel <- ps$ts - ps$lg_ts
message("Step 4b, ts_rel mean (want near small positive, since filtered players are regulars): ",
        round(mean(ps$ts_rel), 4))

# Step 5: experience since debut. Debut = first regular season with any minutes in the
# data, taken from all games (before the min_tsa filter), so a thin rookie year still
# counts as year 0. Only reliable for players who debuted after the first pulled season.
debut <- reg |>
  dplyr::group_by(player_id) |>
  dplyr::summarise(debut = min(season), .groups = "drop")
nseas <- ps |>
  dplyr::group_by(player_id) |>
  dplyr::summarise(n_seasons = dplyr::n(), .groups = "drop")   # qualifying seasons
ps <- merge(merge(ps, debut, by = "player_id"), nseas, by = "player_id")
ps$exp   <- ps$season - ps$debut
ps$exp_c <- ps$exp - cfg$exp_center

before <- nrow(ps)
ps <- ps[ps$debut >= cfg$first_rookie & ps$exp <= cfg$max_exp & ps$n_seasons >= cfg$min_seasons, ]
message("Step 5, rows kept after rookie, exp, and min_seasons filters: ", nrow(ps), " of ", before)

ps <- ps[order(ps$player_id, ps$season), ]
saveRDS(ps, file.path(cfg$dir_data, "player_season.rds"))

# Step 6: checks to eyeball
message("Players: ", length(unique(ps$player_id)), "  Player-seasons: ", nrow(ps))
print(table(ps$exp))
print(summary(ps[, c("tsa", "ts", "se_ts")]))

# Is se_ts in the right range? League TS% is roughly .54 and a full-season
# starter has ~1000+ attempts, which gives se near .015.
print(tapply(ps$se_ts, cut(ps$tsa, c(0, 300, 600, 1000, Inf)), median))

# Survivorship warning: who is still around at each experience year?
print(tapply(ps$player_id, ps$exp, function(x) length(unique(x))))
message("Note: players who last longer are better on average, so later experience ",
        "years are a selected group. Read the curve with that in mind.")
