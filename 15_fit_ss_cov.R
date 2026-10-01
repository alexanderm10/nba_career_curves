# 15_fit_ss_cov.R
# Stage C, step 2. Fit the state-space model with lagged style as covariates on the training
# seasons, forecast the holdout, and compare with the base state-space model from 09.
# Run 09 first so output/holdout_predictions_ss.rds exists.
#
# Leakage control. Training rows use each player's previous-season style. Holdout rows use
# style as of the last training season for every held-out year, because a real forecast made
# at the cutoff would not know the style of later held-out seasons.

source("00_setup.R")
source("ss_helpers.R")
source("ss_cov_helpers.R")
if (!requireNamespace("cmdstanr", quietly = TRUE)) stop("Install cmdstanr first.")

psx  <- readRDS(file.path(cfg$dir_data, "ps_style.rds"))
asof <- readRDS(file.path(cfg$dir_data, "style_asof.rds"))
base <- readRDS(file.path(cfg$dir_out, "holdout_predictions_ss.rds"))
pcs    <- paste0("pc", seq_len(cfg$style_k))
xcols  <- c(paste0("lag_", pcs), "no_lag")

# ---- Step 1: split and build holdout covariates ------------------------------------------------
train <- psx[psx$season <  cfg$holdout_from, ]
test  <- psx[psx$season >= cfg$holdout_from & psx$player_id %in% train$player_id, ]
message("Train rows: ", nrow(train), "  Test rows: ", nrow(test))

test <- merge(test, asof, by = "player_id", all.x = TRUE)
test$no_lag <- as.integer(is.na(test$asof_pc1))
for (v in pcs) {
  a <- paste0("asof_", v)
  test[[paste0("lag_", v)]] <- ifelse(is.na(test[[a]]), 0, test[[a]])   # overwrite with as-of style
}
message("Test rows with no as-of style: ", sum(test$no_lag))
print(round(sapply(train[xcols], mean), 3))
print(round(sapply(test[xcols],  mean), 3))   # should be close to the training means

# ---- Step 2: fit on training seasons ------------------------------------------------------------------
mod <- cmdstanr::cmdstan_model("14_state_space_cov.stan")
t0  <- Sys.time()
fit <- mod$sample(data = ss_cov_stan_data(train, cfg$exp_center, xcols),
                  chains = 4, parallel_chains = 4,
                  iter_warmup = 1000, iter_sampling = 1000,
                  seed = cfg$seed, adapt_delta = 0.9)
message("Fit seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))

K <- length(xcols)
vars <- c(par_names, paste0("beta[", seq_len(K), "]"))
summ <- fit$summary(vars)
print(summ)
message("Max Rhat: ", round(max(summ$rhat), 3), "  (want < 1.01)")
print(fit$diagnostic_summary())

# Which covariates matter? Intervals that sit clearly away from zero are the ones to report.
beta_tab <- fit$summary(paste0("beta[", seq_len(K), "]"), "mean", "sd",
                        ~quantile(.x, probs = c(0.05, 0.95)))
beta_tab$covariate <- xcols
print(beta_tab[, c("covariate", "mean", "sd", "5%", "95%")])

# ---- Step 3: forecasts with covariates -----------------------------------------------------------------
draws_all <- as.data.frame(fit$draws(vars, format = "df"))
draws <- draws_all[round(seq(1, nrow(draws_all), length.out = cfg$ss_draws)), vars]

hist_by <- split(train, train$player_id)
test_by <- split(test,  test$player_id)

forecast_one <- function(id) {
  h <- hist_by[[id]]; h <- h[order(h$exp), ]
  n <- test_by[[id]]; n <- n[order(n$exp), ]
  fd <- ss_cov_forecast_draws(data.frame(exp = h$exp, y = h$ts_rel, se = h$se_ts),
                              data.frame(exp = n$exp, se = n$se_ts),
                              as.matrix(h[xcols]), as.matrix(n[xcols]),
                              draws, cfg$exp_center)
  yd <- fd$mean + matrix(rnorm(length(fd$mean)), nrow(fd$mean)) * sqrt(fd$var)
  data.frame(player_id = id, season = n$season,
             rel_cov = colMeans(fd$mean),
             rel_cov_lo = apply(yd, 2, quantile, probs = 0.05),
             rel_cov_hi = apply(yd, 2, quantile, probs = 0.95))
}

set.seed(cfg$seed)
ids <- names(test_by)
t0  <- Sys.time()
fc  <- lapply(seq_along(ids), function(i) {
  if (i %% 50 == 0) message("  forecast player ", i, " of ", length(ids))
  forecast_one(ids[i])
})
message("Forecast seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
fc <- do.call(rbind, fc)

# ---- Step 4: compare with the base state-space model on identical rows -------------------------------------
cmp <- merge(base[c("player_id", "season", "ts", "tsa", "lg_ts", "pred_naive_adj",
                    "pred_curve_adj", "pred_ss", "lo90", "hi90")],
             fc, by = c("player_id", "season"))
message("Rows scored in both models: ", nrow(cmp), " of ", nrow(base))
cmp$pred_cov <- cmp$rel_cov + cmp$lg_ts
cmp$lo90_cov <- cmp$rel_cov_lo + cmp$lg_ts
cmp$hi90_cov <- cmp$rel_cov_hi + cmp$lg_ts

wrmse <- function(obs, pred, w) sqrt(weighted.mean((obs - pred)^2, w))
scores <- data.frame(
  model = c("naive_adj", "curve_adj", "state_space", "state_space_style"),
  wrmse = c(wrmse(cmp$ts, cmp$pred_naive_adj, cmp$tsa),
            wrmse(cmp$ts, cmp$pred_curve_adj, cmp$tsa),
            wrmse(cmp$ts, cmp$pred_ss,        cmp$tsa),
            wrmse(cmp$ts, cmp$pred_cov,       cmp$tsa))
)
print(scores)
message("Coverage, base:  ", round(mean(cmp$ts >= cmp$lo90 & cmp$ts <= cmp$hi90), 3))
message("Coverage, style: ", round(mean(cmp$ts >= cmp$lo90_cov & cmp$ts <= cmp$hi90_cov), 3))
message("If state_space_style does not beat state_space by more than the run-to-run noise,")
message("style is a descriptive result here rather than a forecasting gain. Report it that way.")

saveRDS(cmp, file.path(cfg$dir_out, "holdout_predictions_ss_style.rds"))
