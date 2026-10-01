# 05_fit_models.R
# Step up the model in two stages and time each one.
#   A. lme4: random intercept and slope by player (fast, no measurement error term)
#   B. brms: same structure, but sampling error se_ts is supplied for each row,
#      so seasonal luck on small samples is separated from real variation.
# In B, total variance per row = se_ts^2 + sigma^2. The estimated sigma is the
# real season-to-season wobble once shooting luck has been accounted for.

source("00_setup.R")
ps <- readRDS(file.path(cfg$dir_data, "player_season.rds"))

# ---- A. lme4 ---------------------------------------------------------------
t0 <- Sys.time()
m_lmer <- lme4::lmer(ts ~ exp_c + I(exp_c^2) + (1 + exp_c | player_id),
                     data = ps, weights = tsa / mean(tsa), REML = TRUE)
message("lmer seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))
print(summary(m_lmer))
message("Singular fit? ", lme4::isSingular(m_lmer))
saveRDS(m_lmer, file.path(cfg$dir_out, "m_lmer.rds"))

# ---- B. brms ---------------------------------------------------------------
# Priors are weakly informative and centred on league-typical values.
# Intercept is TS% at exp_c = 0 (year 3). Slopes are small. sd and sigma are in
# TS% units, so exponential(20) has a mean of .05.
pri <- c(
  brms::prior(normal(0.54, 0.05), class = "Intercept"),
  brms::prior(normal(0, 0.02),    class = "b"),
  brms::prior(exponential(20),    class = "sd"),
  brms::prior(exponential(20),    class = "sigma"),
  brms::prior(lkj(2),             class = "cor")
)

f <- brms::bf(ts | se(se_ts, sigma = TRUE) ~ exp_c + I(exp_c^2) + (1 + exp_c | player_id))

t0 <- Sys.time()
m_brms <- brms::brm(
  f, data = ps, prior = pri,
  chains = 4, cores = 4, iter = 2000, seed = cfg$seed, backend = "cmdstanr",
  control = list(adapt_delta = 0.95),
  file = file.path(cfg$dir_out, "m_brms")   # cached after the first fit
)
message("brms seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
        " (near zero means it loaded the cached fit)")

# Diagnostics to read before trusting anything
print(summary(m_brms))
message("Max Rhat: ", round(max(brms::rhat(m_brms), na.rm = TRUE), 3), "  (want < 1.01)")
message("Min n_eff ratio: ", round(min(brms::neff_ratio(m_brms), na.rm = TRUE), 3), "  (want > 0.1)")
np <- brms::nuts_params(m_brms)
message("Divergent transitions: ", sum(np$Value[np$Parameter == "divergent__"]))

# Posterior predictive check on the data scale
print(brms::pp_check(m_brms, ndraws = 50))
