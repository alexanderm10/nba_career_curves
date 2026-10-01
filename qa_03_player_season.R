# qa_03_player_season.R
# QA figures for the player-season table built in 03. Run after 03.
# Saves figures to output/qa/.
#
# The split-half check tests the sampling error formula in 03. Each qualifying
# player-season is split into odd and even games. If se_ts is right, the gap between the
# two halves' TS% should have variance close to se_half_odd^2 + se_half_even^2.
# Ratio > 1 means se_ts is too small (points per attempt is not a 0/1 trial, threes pay
# 1.5 per half-point, and-ones etc.).

source("00_setup.R")
dir_qa <- file.path(cfg$dir_out, "qa")
dir.create(dir_qa, showWarnings = FALSE)
ps  <- readRDS(file.path(cfg$dir_data, "player_season.rds"))
box <- drop_exhibition(readRDS(file.path(cfg$dir_data, "box_raw.rds")))
th  <- ggplot2::theme_minimal()
save_fig <- function(p, name, w = 7, h = 4.5) {
  ggplot2::ggsave(file.path(dir_qa, name), p, width = w, height = h, dpi = 150)
}

# ---- 1. league TS% by season (era drift that ts_rel removes) ---------------------------
lg <- unique(ps[c("season", "lg_ts")])
save_fig(ggplot2::ggplot(lg, ggplot2::aes(season, lg_ts)) +
  ggplot2::geom_line() + ggplot2::geom_point() + th +
  ggplot2::labs(x = "Season", y = "League TS%", title = "League true shooting by season"),
  "03_league_ts.png")

# ---- 2. rows per experience year and per debut cohort ----------------------------------
save_fig(ggplot2::ggplot(ps, ggplot2::aes(factor(exp))) + ggplot2::geom_bar() + th +
  ggplot2::labs(x = "Years since debut", y = "Player-seasons",
                title = "Sample thins with experience (survivorship)"),
  "03_rows_by_exp.png")

# ---- 3. TS% distribution and se vs attempts --------------------------------------------
save_fig(ggplot2::ggplot(ps, ggplot2::aes(ts_rel)) +
  ggplot2::geom_histogram(bins = 60) + th +
  ggplot2::labs(x = "TS% minus league TS%", y = "Player-seasons",
                title = "League-adjusted TS% (qualifying player-seasons)"),
  "03_ts_rel_hist.png")

save_fig(ggplot2::ggplot(ps, ggplot2::aes(tsa, se_ts)) +
  ggplot2::geom_point(alpha = 0.15, size = 0.8) + th +
  ggplot2::labs(x = "True shooting attempts", y = "Sampling error of TS%",
                title = "Sampling error shrinks with attempts"),
  "03_se_vs_tsa.png")

# ---- 4. raw mean ts_rel by experience (what the models will try to smooth) -------------
m <- aggregate(cbind(ts_rel, tsa) ~ exp, data = ps, FUN = mean)
m$se <- tapply(ps$ts_rel, ps$exp, sd) / sqrt(tapply(ps$ts_rel, ps$exp, length))
save_fig(ggplot2::ggplot(m, ggplot2::aes(exp, ts_rel)) +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = ts_rel - 2 * se, ymax = ts_rel + 2 * se)) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2) + th +
  ggplot2::labs(x = "Years since debut", y = "Mean TS% minus league",
                title = "Raw league-adjusted TS% by experience (+/- 2 SE)"),
  "03_raw_curve.png")
print(m)

# ---- 5. split-half test of the sampling error formula ----------------------------------
g <- data.frame(
  player_id = as.character(box[[cols$id]]),
  season    = as.integer(box[[cols$season]]),
  stype     = box[[cols$season_type]],
  gdate     = box$game_date,
  minutes   = suppressWarnings(as.numeric(box[[cols$minutes]])),
  pts = as.numeric(box[[cols$points]]), fga = as.numeric(box[[cols$fga]]),
  fta = as.numeric(box[[cols$fta]])
)
g <- g[g$stype == 2 & !is.na(g$minutes) & g$minutes > 0 & !is.na(g$pts), ]
g <- g[paste(g$player_id, g$season) %in% paste(ps$player_id, ps$season), ]
g <- g[order(g$player_id, g$season, g$gdate), ]
g$half <- stats::ave(seq_len(nrow(g)), g$player_id, g$season, FUN = seq_along) %% 2

h <- g |>
  dplyr::group_by(player_id, season, half) |>
  dplyr::summarise(pts = sum(pts), tsa = sum(fga + 0.44 * fta), .groups = "drop")
h$ts <- h$pts / (2 * h$tsa)
h$v  <- h$ts * (1 - h$ts) / h$tsa
a <- h[h$half == 0, ]; b <- h[h$half == 1, ]
sh <- merge(a, b, by = c("player_id", "season"), suffixes = c("_a", "_b"))
sh <- sh[sh$tsa_a > 30 & sh$tsa_b > 30, ]
sh$z <- (sh$ts_a - sh$ts_b) / sqrt(sh$v_a + sh$v_b)
ratio <- var(sh$z)
message("Split-half variance ratio (observed / formula): ", round(ratio, 3),
        "  -> se_ts should be scaled by about ", round(sqrt(ratio), 3))
sh$tsa_bin <- cut(sh$tsa_a + sh$tsa_b, c(0, 300, 600, 1000, Inf))
print(tapply(sh$z, sh$tsa_bin, var))

save_fig(ggplot2::ggplot(sh, ggplot2::aes(z)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 60) +
  ggplot2::stat_function(fun = stats::dnorm, colour = "firebrick") + th +
  ggplot2::labs(x = "Standardized odd-minus-even-game TS% gap",
                y = "Density",
                title = "Split-half check of the sampling error formula",
                subtitle = paste0("Red = N(0,1) if se_ts were exact. Observed variance ratio = ",
                                  round(ratio, 2))),
  "03_split_half.png")
saveRDS(list(ratio = ratio, by_tsa = tapply(sh$z, sh$tsa_bin, var)),
        file.path(dir_qa, "03_split_half.rds"))

# ---- 6. time from debut to first qualifying season (min_tsa) -----------------------------
# How many players never reach the attempts floor, and how long the rest take. Players who
# qualify late are less likely to end up in the modeled set, so it leans toward early starters.
# g above holds only qualifying player-seasons, so rebuild from all played games
all_g <- data.frame(
  player_id = as.character(box[[cols$id]]), season = as.integer(box[[cols$season]]),
  stype = box[[cols$season_type]], minutes = suppressWarnings(as.numeric(box[[cols$minutes]])),
  fga = as.numeric(box[[cols$fga]]), fta = as.numeric(box[[cols$fta]]))
all_g <- all_g[all_g$stype == 2 & !is.na(all_g$minutes) & all_g$minutes > 0 & !is.na(all_g$fga), ]
d6 <- all_g |>
  dplyr::group_by(player_id, season) |>
  dplyr::summarise(tsa = sum(fga + 0.44 * fta), .groups = "drop") |>
  dplyr::group_by(player_id) |>
  dplyr::summarise(debut = min(season),
                   first_q = suppressWarnings(min(season[tsa >= cfg$min_tsa])), .groups = "drop")
d6 <- d6[d6$debut >= cfg$first_rookie, ]
message("Players debuting ", cfg$first_rookie, "+: ", nrow(d6),
        "  never reaching ", cfg$min_tsa, " attempts: ", sum(!is.finite(d6$first_q)))
q6 <- d6[is.finite(d6$first_q), ]
q6$lag <- q6$first_q - q6$debut
print(round(prop.table(table(pmin(q6$lag, 3))), 3))
q6$in_model <- q6$player_id %in% ps$player_id
print(round(tapply(q6$in_model, pmin(q6$lag, 3), mean), 3))   # share reaching the modeled set
save_fig(ggplot2::ggplot(q6, ggplot2::aes(factor(pmin(lag, 5)))) + ggplot2::geom_bar() +
  ggplot2::scale_x_discrete(labels = c(0:4, "5+")) + th +
  ggplot2::labs(x = paste0("Seasons from debut to first ", cfg$min_tsa, "-attempt season"),
                y = "Players", title = "Time to first qualifying season",
                subtitle = paste0("Players debuting ", cfg$first_rookie, "+ who ever qualify (n = ",
                                  nrow(q6), ")")),
  "03_time_to_qualify.png")
