# 16_minutes_clock.R
# Stage D, step 1. Is cumulative playing time a better development clock than years since debut?
# Notes and reasoning: docs/STAGE_D_minutes_clock.md
#
# Clock variables, both known before the season starts:
#   exp      years since debut (as in 03)
#   lcm      log(1 + prior career regular-season minutes / 1000)
# Prior minutes count every regular-season game, including seasons below min_tsa.
#
# Holdout scoring, curve-only GAMs with a player random effect, on the league-adjusted scale:
#   main     season == holdout_from only. Prior minutes are fully known at the cutoff.
#   upper    all held-out seasons with realized prior minutes. Optimistic for later seasons,
#            because a forecast made at the cutoff would not know the minutes coaches hand out.
# Minutes partly reflect what coaches have already seen in a player, so a gain here means
# playing time carries forecasting information, not that playing time causes improvement.

source("00_setup.R")
box <- drop_exhibition(readRDS(file.path(cfg$dir_data, "box_raw.rds")))
ps  <- readRDS(file.path(cfg$dir_data, "player_season.rds"))
th  <- ggplot2::theme_minimal()

# ---- Step 1: prior career minutes for every player-season -------------------------------------
g <- data.frame(
  player_id = as.character(box[[cols$id]]),
  season    = as.integer(box[[cols$season]]),
  stype     = box[[cols$season_type]],
  minutes   = suppressWarnings(as.numeric(box[[cols$minutes]]))
)
g <- g[g$stype == 2 & !is.na(g$minutes) & g$minutes > 0, ]
sm <- aggregate(minutes ~ player_id + season, data = g, FUN = sum)
sm <- sm[order(sm$player_id, sm$season), ]
sm$cum_min_prior <- stats::ave(sm$minutes, sm$player_id, FUN = function(x) cumsum(x) - x)

ps <- merge(ps, sm[c("player_id", "season", "cum_min_prior")], by = c("player_id", "season"))
ps$cm  <- ps$cum_min_prior / 1000
ps$lcm <- log1p(ps$cm)
ps$pid <- factor(ps$player_id)
ps$w   <- ps$tsa / mean(ps$tsa)
message("Rows: ", nrow(ps), "  Rookie rows with zero prior minutes: ", sum(ps$exp == 0 & ps$cm == 0),
        " of ", sum(ps$exp == 0))

# ---- Step 2: how different are the two clocks? ----------------------------------------------------
message("Correlation exp vs lcm: ", round(cor(ps$exp, ps$lcm), 3),
        "   exp vs cm: ", round(cor(ps$exp, ps$cm), 3))
spread <- aggregate(cm ~ exp, data = ps, FUN = function(x) round(quantile(x, c(0.1, 0.5, 0.9)), 1))
print(do.call(data.frame, spread))   # thousands of prior minutes, 10th / 50th / 90th percentile

p1 <- ggplot2::ggplot(ps, ggplot2::aes(factor(exp), cm)) +
  ggplot2::geom_boxplot(outlier.size = 0.4) + th +
  ggplot2::labs(x = "Years since debut", y = "Prior career minutes (thousands)",
                title = "Same years, very different playing time")
ggplot2::ggsave(file.path(cfg$dir_out, "16_clock_spread.png"), p1, width = 7, height = 4.5, dpi = 150)

# ---- Step 3: curves on the full data, pooled and within-player --------------------------------------
# bam with fREML: gam() with a 1,000-level random effect took far too long.
fit_set <- function(dd) {
  fit <- function(f) mgcv::bam(f, data = dd, weights = w, method = "fREML", discrete = TRUE)
  list(
    exp_pool = fit(ts_rel ~ s(exp, k = 6)),
    min_pool = fit(ts_rel ~ s(lcm, k = 6)),
    exp_re   = fit(ts_rel ~ s(exp, k = 6) + s(pid, bs = "re")),
    min_re   = fit(ts_rel ~ s(lcm, k = 6) + s(pid, bs = "re")),
    both_re  = fit(ts_rel ~ s(exp, k = 6) + s(lcm, k = 6) + s(pid, bs = "re"))
  )
}
t0 <- Sys.time()
fits <- fit_set(ps)
message("Full-data GAM seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cmp <- data.frame(model = names(fits),
                  aic   = round(sapply(fits, AIC), 1),
                  dev_expl = round(sapply(fits, function(m) summary(m)$dev.expl), 4),
                  row.names = NULL)
cmp$d_aic <- cmp$aic - min(cmp$aic)
print(cmp)
print(summary(fits$both_re)$s.table)

# Partial curves from the within-player models, on comparable axes
pred_curve <- function(m, var, grid) {
  nd <- data.frame(exp = 3, lcm = median(ps$lcm), pid = ps$pid[1])
  nd <- nd[rep(1, length(grid)), ]
  nd[[var]] <- grid
  pr <- predict(m, newdata = nd, se.fit = TRUE, exclude = "s(pid)")
  data.frame(x = grid, fit = pr$fit, lo = pr$fit - 1.96 * pr$se.fit, hi = pr$fit + 1.96 * pr$se.fit)
}
ce <- cbind(clock = "Years since debut (exp_re)", pred_curve(fits$exp_re, "exp", 0:cfg$max_exp))
cm_grid <- seq(0, quantile(ps$cm, 0.99), length.out = 60)
cmn <- cbind(clock = "Prior minutes, thousands (min_re)", pred_curve(fits$min_re, "lcm", log1p(cm_grid)))
cmn$x <- cm_grid
p2 <- ggplot2::ggplot(rbind(ce, cmn), ggplot2::aes(x, fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.25) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::facet_wrap(~ clock, scales = "free_x") + th +
  ggplot2::labs(x = NULL, y = "TS% minus league (within-player curve)",
                title = "Development curve on two clocks", subtitle = "GAM with player random effect, 95% bands")
ggplot2::ggsave(file.path(cfg$dir_out, "16_two_clocks.png"), p2, width = 9, height = 4.5, dpi = 150)

# ---- Step 4: holdout ----------------------------------------------------------------------------------
train <- ps[ps$season <  cfg$holdout_from, ]
test  <- ps[ps$season >= cfg$holdout_from & ps$player_id %in% train$player_id, ]
train$pid <- droplevels(train$pid)
test$pid  <- factor(test$player_id, levels = levels(train$pid))
fits_tr <- fit_set(train)[c("exp_re", "min_re", "both_re")]

wrmse <- function(obs, pred, w) sqrt(weighted.mean((obs - pred)^2, w))
for (m in names(fits_tr)) test[[paste0("pred_", m)]] <- as.numeric(predict(fits_tr[[m]], newdata = test)) + test$lg_ts

# Same-row reference from the state-space holdout in 09
ss <- readRDS(file.path(cfg$dir_out, "holdout_predictions_ss.rds"))
test <- merge(test, ss[c("player_id", "season", "pred_ss", "pred_naive_adj")], by = c("player_id", "season"))

score <- function(dd, label) {
  data.frame(rows = label, n = nrow(dd),
             naive_adj   = wrmse(dd$ts, dd$pred_naive_adj, dd$tsa),
             gam_exp     = wrmse(dd$ts, dd$pred_exp_re,    dd$tsa),
             gam_minutes = wrmse(dd$ts, dd$pred_min_re,    dd$tsa),
             gam_both    = wrmse(dd$ts, dd$pred_both_re,   dd$tsa),
             state_space = wrmse(dd$ts, dd$pred_ss,        dd$tsa))
}
scores <- rbind(score(test[test$season == cfg$holdout_from, ], "main: first held-out season"),
                score(test, "upper: all held-out, realized minutes"))
num <- sapply(scores, is.numeric) & names(scores) != "n"
scores[num] <- round(scores[num], 5)
print(scores)
message("Read 'main' first. 'upper' uses minutes a real forecast would not know yet.")

saveRDS(ps[c("player_id", "season", "cum_min_prior", "cm", "lcm")],
        file.path(cfg$dir_data, "prior_minutes.rds"))
write.csv(scores, file.path(cfg$dir_out, "16_holdout_scores.csv"), row.names = FALSE)
write.csv(cmp,    file.path(cfg$dir_out, "16_model_comparison.csv"), row.names = FALSE)
