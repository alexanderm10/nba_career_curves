# ss_cov_helpers.R
# Covariate versions of the state-space helpers. Source ss_helpers.R first.

# Stan data list with a covariate matrix. Rows are sorted by player then experience.
ss_cov_stan_data <- function(ps, center, xcols) {
  ps   <- ps[order(ps$player_id, ps$exp), ]
  base <- ss_stan_data(ps, center)
  base$K <- length(xcols)
  base$X <- as.matrix(ps[, xcols, drop = FALSE])
  base
}

# Mean on the adjusted scale: curve plus covariates
ss_cov_mean <- function(e, X, p, beta, center) {
  ss_curve(e, p$b0, p$b1, p$b2, center) + as.numeric(X %*% beta)
}

# Predictive mean and variance of new seasons given a player's history, with covariates
ss_cov_forecast_player <- function(hist, new, Xh, Xn, p, beta, center) {
  mu_h <- ss_cov_mean(hist$exp, Xh, p, beta, center)
  mu_n <- ss_cov_mean(new$exp,  Xn, p, beta, center)
  S_hh <- ss_obs_cov(hist, p)
  S_nh <- ss_signal_cov(new$exp, hist$exp, p$tau_a, p$tau_z, p$phi)
  S_nn <- ss_signal_cov(new$exp, new$exp,  p$tau_a, p$tau_z, p$phi)
  diag(S_nn) <- diag(S_nn) + new$se^2 + p$sigma^2
  W <- S_nh %*% solve(S_hh)
  list(mean = mu_n + as.numeric(W %*% (hist$y - mu_h)),
       var  = diag(S_nn - W %*% t(S_nh)))
}

# One forecast per posterior draw. draws must hold par_names and beta[1..K].
ss_cov_forecast_draws <- function(hist, new, Xh, Xn, draws, center) {
  K <- ncol(Xh)
  beta_names <- paste0("beta[", seq_len(K), "]")
  out <- lapply(seq_len(nrow(draws)), function(i) {
    ss_cov_forecast_player(hist, new, Xh, Xn,
                           as.list(draws[i, par_names]),
                           as.numeric(draws[i, beta_names]), center)
  })
  list(mean = do.call(rbind, lapply(out, `[[`, "mean")),
       var  = do.call(rbind, lapply(out, `[[`, "var")))
}
