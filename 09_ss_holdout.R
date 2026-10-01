# 09_ss_holdout.R
# Does latent talent beat simpler forecasts? Fit on seasons before cfg$holdout_from,
# forecast later seasons for players who already have a history.
#
# Rivals, all scored on the raw TS% scale:
#   naive_raw    last observed raw TS% carried forward
#   naive_adj    last league-adjusted TS% carried forward, plus the league value
#   curve_adj    population curve only (no player state), plus the league value
#   state_space  curve + permanent level + transient form, plus the league value
#
# Caveat, applies equally to every adjusted model: the league TS% of the held-out season
# is the realized value, which a real forecast would not know. Likewise the predictive
# interval uses the realized attempts (se) of the held-out season.

source("00_setup.R")
source("ss_helpers.R")
if (!requireNamespace("cmdstanr", quietly = TRUE)) stop("Install cmdstanr first.")

ps <- readRDS(file.path(cfg$dir_data, "player_season.rds"))
train <- ps[ps$season <  cfg$holdout_from, ]
test  <- ps[ps$season >= cfg$holdout_from & ps$player_id %in% train$player_id, ]
message("Train rows: ", nrow(train), "  Test rows: ", nrow(test),
        "  Test players: ", length(unique(test$player_id)))
stopifnot(nrow(test) > 0)

# ---- Step 1: fit on training seasons ----------------------------------------
mod <- cmdstanr::cmdstan_model("07_state_space.stan")
t0 <- Sys.time()
fit_tr <- mod$sample(data = ss_stan_data(train, cfg$exp_center),
                     chains = 4, parallel_chains = 4,
                     iter_warmup = 1000, iter_sampling = 1000,
                     seed = cfg$seed, adapt_delta = 0.9)
message("Train fit seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
summ <- fit_tr$summary(par_names)
print(summ)
message("Max Rhat: ", round(max(summ$rhat), 3), "  (want < 1.01)")

draws_all <- as.data.frame(fit_tr$draws(par_names, format = "df"))
keep  <- round(seq(1, nrow(draws_all), length.out = cfg$ss_draws))
draws <- draws_all[keep, par_names]
p_hat <- as.list(colMeans(draws_all[par_names]))

# ---- Step 2: state-space forecasts, one player at a time ----------------------
hist_by <- split(train, train$player_id)
test_by <- split(test,  test$player_id)

forecast_one <- function(id) {
  h <- hist_by[[id]]; h <- h[order(h$exp), ]
  n <- test_by[[id]]; n <- n[order(n$exp), ]
  fd <- ss_forecast_draws(data.frame(exp = h$exp, y = h$ts_rel, se = h$se_ts),
                          data.frame(exp = n$exp, se = n$se_ts),
                          draws, cfg$exp_center)
  # predictive draws that include sampling noise
  yd <- fd$mean + matrix(rnorm(length(fd$mean)), nrow(fd$mean)) * sqrt(fd$var)
  data.frame(player_id = id, season = n$season,
             rel_ss = colMeans(fd$mean),
             rel_lo = apply(yd, 2, quantile, probs = 0.05),
             rel_hi = apply(yd, 2, quantile, probs = 0.95))
}

set.seed(cfg$seed)
t0 <- Sys.time()
ids <- names(test_by)
fc  <- lapply(seq_along(ids), function(i) {
  if (i %% 50 == 0) message("  forecast player ", i, " of ", length(ids))
  forecast_one(ids[i])
})
message("Forecast seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
fc <- do.call(rbind, fc)
test <- merge(test, fc, by = c("player_id", "season"))
message("Rows scored: ", nrow(test))
stopifnot(!anyNA(test$rel_ss))

# ---- Step 3: rivals ------------------------------------------------------------
tr_ord <- train[order(train$player_id, train$season), ]
last   <- tr_ord[!duplicated(tr_ord$player_id, fromLast = TRUE), c("player_id", "ts", "ts_rel")]
names(last) <- c("player_id", "last_ts", "last_rel")
test <- merge(test, last, by = "player_id")

test$pred_naive_raw <- test$last_ts
test$pred_naive_adj <- test$last_rel + test$lg_ts
test$pred_curve_adj <- ss_curve(test$exp, p_hat$b0, p_hat$b1, p_hat$b2, cfg$exp_center) + test$lg_ts
test$pred_ss        <- test$rel_ss + test$lg_ts
test$lo90 <- test$rel_lo + test$lg_ts
test$hi90 <- test$rel_hi + test$lg_ts

# ---- Step 4: scoring -----------------------------------------------------------
wrmse <- function(obs, pred, w) sqrt(weighted.mean((obs - pred)^2, w))
scores <- data.frame(
  model = c("naive_raw", "naive_adj", "curve_adj", "state_space"),
  wrmse = c(wrmse(test$ts, test$pred_naive_raw, test$tsa),
            wrmse(test$ts, test$pred_naive_adj, test$tsa),
            wrmse(test$ts, test$pred_curve_adj, test$tsa),
            wrmse(test$ts, test$pred_ss,        test$tsa))
)
print(scores)
message("State-space 90% interval coverage: ",
        round(mean(test$ts >= test$lo90 & test$ts <= test$hi90), 3), "  (want near 0.90)")
message("Mean 90% interval width (TS% points): ", round(mean(test$hi90 - test$lo90), 4))

# By experience year, where player-level information should matter most
test$err_naive_adj <- abs(test$ts - test$pred_naive_adj)
test$err_ss        <- abs(test$ts - test$pred_ss)
print(aggregate(cbind(err_naive_adj, err_ss) ~ exp, data = test, FUN = mean))
print(table(test$exp))

saveRDS(test, file.path(cfg$dir_out, "holdout_predictions_ss.rds"))
