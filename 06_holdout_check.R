# 06_holdout_check.R
# The test that matters. Fit on earlier seasons, then predict later seasons for
# players who already have a history. Compare against two simple rivals:
#   naive  = carry each player's last observed TS% forward
#   gam    = the pooled curve from 04
# Report weighted RMSE (weights = attempts in the held-out season) and, for brms,
# how often the 90% predictive interval covers the observed value.

source("00_setup.R")
ps <- readRDS(file.path(cfg$dir_data, "player_season.rds"))

train <- ps[ps$season <  cfg$holdout_from, ]
test  <- ps[ps$season >= cfg$holdout_from & ps$player_id %in% train$player_id, ]
message("Train rows: ", nrow(train), "  Test rows: ", nrow(test),
        "  Test players: ", length(unique(test$player_id)))
stopifnot(nrow(test) > 0)

# ---- rival 1: last observed value -------------------------------------------
tr_ord <- train[order(train$player_id, train$season), ]
last   <- tr_ord[!duplicated(tr_ord$player_id, fromLast = TRUE), c("player_id", "ts")]
names(last)[2] <- "pred_naive"
test <- merge(test, last, by = "player_id")

# ---- rival 2: pooled GAM -----------------------------------------------------
m_gam_tr <- mgcv::gam(ts ~ s(exp, k = 6), data = train, weights = tsa, method = "REML")
test$pred_gam <- as.numeric(predict(m_gam_tr, newdata = test))

# ---- lme4 --------------------------------------------------------------------
m_lmer_tr <- lme4::lmer(ts ~ exp_c + I(exp_c^2) + (1 + exp_c | player_id),
                        data = train, weights = tsa / mean(tsa), REML = TRUE)
test$pred_lmer <- as.numeric(predict(m_lmer_tr, newdata = test, allow.new.levels = FALSE))

# ---- brms --------------------------------------------------------------------
pri <- c(
  brms::prior(normal(0.54, 0.05), class = "Intercept"),
  brms::prior(normal(0, 0.02),    class = "b"),
  brms::prior(exponential(20),    class = "sd"),
  brms::prior(exponential(20),    class = "sigma"),
  brms::prior(lkj(2),             class = "cor")
)
f <- brms::bf(ts | se(se_ts, sigma = TRUE) ~ exp_c + I(exp_c^2) + (1 + exp_c | player_id))

t0 <- Sys.time()
m_brms_tr <- brms::brm(
  f, data = train, prior = pri,
  chains = 4, cores = 4, iter = 2000, seed = cfg$seed, backend = "cmdstanr",
  control = list(adapt_delta = 0.95),
  file = file.path(cfg$dir_out, "m_brms_train")
)
message("brms train fit seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))

epred <- brms::posterior_epred(m_brms_tr, newdata = test)   # draws x rows
test$pred_brms <- colMeans(epred)

ppred <- brms::posterior_predict(m_brms_tr, newdata = test) # includes sampling noise via se_ts
test$lo90 <- apply(ppred, 2, quantile, probs = 0.05)
test$hi90 <- apply(ppred, 2, quantile, probs = 0.95)

# ---- scoring -----------------------------------------------------------------
wrmse <- function(obs, pred, w) sqrt(weighted.mean((obs - pred)^2, w))

scores <- data.frame(
  model = c("naive", "gam", "lmer", "brms"),
  wrmse = c(wrmse(test$ts, test$pred_naive, test$tsa),
            wrmse(test$ts, test$pred_gam,   test$tsa),
            wrmse(test$ts, test$pred_lmer,  test$tsa),
            wrmse(test$ts, test$pred_brms,  test$tsa))
)
print(scores)
message("brms 90% interval coverage: ",
        round(mean(test$ts >= test$lo90 & test$ts <= test$hi90), 3), "  (want near 0.90)")

# Error by experience year, since the hierarchy should help most for young players
test$err_naive <- abs(test$ts - test$pred_naive)
test$err_brms  <- abs(test$ts - test$pred_brms)
print(aggregate(cbind(err_naive, err_brms) ~ exp, data = test, FUN = mean))

saveRDS(test, file.path(cfg$dir_out, "holdout_predictions.rds"))
