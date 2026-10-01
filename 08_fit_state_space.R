# 08_fit_state_space.R
# Fit the latent talent model to all player-seasons, then look at what it learned.
# Needs cmdstanr and a working CmdStan install (mc-stan.org/cmdstanr).

source("00_setup.R")
source("ss_helpers.R")
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  stop("Install cmdstanr and run cmdstanr::install_cmdstan() first.")
}

ps <- readRDS(file.path(cfg$dir_data, "player_season.rds"))
stopifnot(all(c("ts_rel", "lg_ts") %in% names(ps)))   # re-run 03 if this fails

# ---- Step 1: data list and what it looks like --------------------------------
dat <- ss_stan_data(ps, cfg$exp_center)
message("Rows: ", dat$N, "  Players: ", dat$J)
print(table(dat$len))                       # seasons per player
message("Mean se: ", round(mean(dat$se), 4), "  SD of y: ", round(sd(dat$y), 4))

# ---- Step 2: compile and run a short pilot to time it ------------------------
mod <- cmdstanr::cmdstan_model("07_state_space.stan")

t0 <- Sys.time()
pilot <- mod$sample(data = dat, chains = 2, parallel_chains = 2,
                    iter_warmup = 150, iter_sampling = 150, seed = cfg$seed,
                    refresh = 0)
pilot_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
message("Pilot (300 iterations x 2 chains) seconds: ", round(pilot_secs))
message("Rough full run estimate (4 chains x 2000 iterations, run in parallel): ",
        round(pilot_secs * (2000 / 300) / 60, 1), " minutes")
print(pilot$summary(par_names))

# ---- Step 3: full fit --------------------------------------------------------
t0 <- Sys.time()
fit <- mod$sample(data = dat, chains = 4, parallel_chains = 4,
                  iter_warmup = 1000, iter_sampling = 1000, seed = cfg$seed,
                  adapt_delta = 0.9)
message("Full fit seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
fit$save_object(file.path(cfg$dir_out, "fit_state_space.rds"))

# ---- Step 4: diagnostics to read before trusting anything --------------------
summ <- fit$summary(par_names)
print(summ)
message("Max Rhat: ", round(max(summ$rhat), 3), "  (want < 1.01)")
message("Min ess_bulk: ", round(min(summ$ess_bulk)), "  (want > 400)")
print(fit$diagnostic_summary())             # divergences, treedepth hits

# ---- Step 5: what the model says about the process ---------------------------
draws <- as.data.frame(fit$draws(par_names, format = "df"))

# Where does season-to-season variation come from? Shares of total variance.
vpart <- c(permanent_talent = mean(draws$tau_a^2),
           transient_form   = mean(draws$tau_z^2),
           extra_noise      = mean(draws$sigma^2),
           sampling_luck    = mean(dat$se^2))
print(round(vpart / sum(vpart), 3))
message("Persistence of form (phi), posterior mean: ", round(mean(draws$phi), 3),
        "  (near 1 = form carries over, near 0 = form is gone next season)")
message("Half-life of a form deviation, seasons: ",
        round(mean(log(0.5) / log(draws$phi)), 2))

# Development curve on the league-adjusted scale, with uncertainty
grid <- data.frame(exp = 0:cfg$max_exp)
curve_draws <- sapply(grid$exp, function(e)
  ss_curve(e, draws$b0, draws$b1, draws$b2, cfg$exp_center))
grid$fit <- colMeans(curve_draws)
grid$lo  <- apply(curve_draws, 2, quantile, probs = 0.05)
grid$hi  <- apply(curve_draws, 2, quantile, probs = 0.95)
print(grid)

# ---- Step 6: latent talent for a few players with long careers ---------------
p_hat <- as.list(colMeans(draws[par_names]))   # plug-in: posterior means only
long_ids <- names(sort(table(ps$player_id), decreasing = TRUE))[1:6]

latent_list <- lapply(long_ids, function(id) {
  h <- ps[ps$player_id == id, ]
  h <- h[order(h$exp), ]
  lt <- ss_latent_player(data.frame(exp = h$exp, y = h$ts_rel, se = h$se_ts),
                         p_hat, cfg$exp_center)
  data.frame(player = h$player_name, exp = h$exp, observed = h$ts_rel, se = h$se_ts,
             latent = lt$mean, latent_sd = lt$sd)
})
latent <- do.call(rbind, latent_list)
print(utils::head(latent, 12))
saveRDS(latent, file.path(cfg$dir_out, "latent_examples.rds"))

p <- ggplot2::ggplot(latent, ggplot2::aes(exp)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = latent - 1.645 * latent_sd,
                                    ymax = latent + 1.645 * latent_sd), alpha = 0.25) +
  ggplot2::geom_line(ggplot2::aes(y = latent), linewidth = 0.9) +
  ggplot2::geom_pointrange(ggplot2::aes(y = observed,
                                        ymin = observed - 1.645 * se,
                                        ymax = observed + 1.645 * se),
                           colour = "grey40", size = 0.3) +
  ggplot2::facet_wrap(~ player) +
  ggplot2::labs(x = "Years since debut", y = "TS% relative to league",
                title = "Observed seasons (points, 90% sampling error) vs inferred talent (line, 90% band)",
                subtitle = "Plug-in at posterior mean parameters") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(cfg$dir_out, "08_latent_examples.png"), p,
                width = 9, height = 6, dpi = 150)
