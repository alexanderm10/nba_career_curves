# ss_helpers.R
# Small single-purpose functions for the state-space model. Sourced by 08 and 09.
# All of them take plain data frames and numbers so each can be checked on its own.

par_names <- c("b0", "b1", "b2", "tau_a", "tau_z", "phi", "sigma")

# Signal covariance between two sets of experience values
ss_signal_cov <- function(e1, e2, tau_a, tau_z, phi) {
  tau_a^2 + tau_z^2 * phi^abs(outer(e1, e2, "-"))
}

# Population development curve on the league-adjusted scale
ss_curve <- function(e, b0, b1, b2, center) {
  ec <- e - center
  b0 + b1 * ec + b2 * ec^2
}

# Stan data list. Rows must be sorted by player then experience.
ss_stan_data <- function(ps, center) {
  ps  <- ps[order(ps$player_id, ps$exp), ]
  len <- as.integer(table(factor(ps$player_id, levels = unique(ps$player_id))))
  stopifnot(sum(len) == nrow(ps))
  list(
    N = nrow(ps), J = length(len),
    start = cumsum(c(1L, len))[seq_along(len)], len = len,
    y = ps$ts_rel, se = ps$se_ts,
    ec = ps$exp - center, e = as.integer(ps$exp)
  )
}

# Observed-data covariance for one player's history: signal plus noise on the diagonal
ss_obs_cov <- function(hist, p) {
  S <- ss_signal_cov(hist$exp, hist$exp, p$tau_a, p$tau_z, p$phi)
  diag(S) <- diag(S) + hist$se^2 + p$sigma^2
  S
}

# Latent talent (curve + player level + form) at a player's observed seasons,
# conditional on that player's observed values. hist needs exp, y, se.
ss_latent_player <- function(hist, p, center) {
  mu <- ss_curve(hist$exp, p$b0, p$b1, p$b2, center)
  K  <- ss_signal_cov(hist$exp, hist$exp, p$tau_a, p$tau_z, p$phi)
  W  <- K %*% solve(ss_obs_cov(hist, p))
  list(mean = mu + as.numeric(W %*% (hist$y - mu)),
       sd   = sqrt(pmax(diag(K - W %*% K), 0)))
}

# Predictive mean and variance of new seasons given a player's history.
# hist needs exp, y, se. new needs exp, se. Variance is for the observed value
# (includes sampling noise), not just latent talent.
ss_forecast_player <- function(hist, new, p, center) {
  mu_h <- ss_curve(hist$exp, p$b0, p$b1, p$b2, center)
  mu_n <- ss_curve(new$exp,  p$b0, p$b1, p$b2, center)
  S_hh <- ss_obs_cov(hist, p)
  S_nh <- ss_signal_cov(new$exp, hist$exp, p$tau_a, p$tau_z, p$phi)
  S_nn <- ss_signal_cov(new$exp, new$exp,  p$tau_a, p$tau_z, p$phi)
  diag(S_nn) <- diag(S_nn) + new$se^2 + p$sigma^2
  W <- S_nh %*% solve(S_hh)
  list(mean = mu_n + as.numeric(W %*% (hist$y - mu_h)),
       var  = diag(S_nn - W %*% t(S_nh)))
}

# Run the forecast once per posterior draw. Returns draws x new-rows matrices.
ss_forecast_draws <- function(hist, new, draws, center) {
  out <- lapply(seq_len(nrow(draws)), function(i) {
    ss_forecast_player(hist, new, as.list(draws[i, par_names]), center)
  })
  list(mean = do.call(rbind, lapply(out, `[[`, "mean")),
       var  = do.call(rbind, lapply(out, `[[`, "var")))
}
