# 17_writeup_figs.R
# Polished figures for the two-page write-up (writeup/two_pager.html).
# Run after 08, 09, 15 and 16. Saves PNGs to output/writeup/ and prints the numbers quoted in the text.
# Figures carry no titles; titles and captions live in the write-up.

source("00_setup.R")
source("ss_helpers.R")
dir_w <- file.path(cfg$dir_out, "writeup")
dir.create(dir_w, showWarnings = FALSE)

ps <- readRDS(file.path(cfg$dir_data, "player_season.rds"))

# ---- look -----------------------------------------------------------------------------------------
pal <- list(ink = "#1c1d1f", ink2 = "#55575c", muted = "#8a8c91", grid = "#e6e7ea", paper = "#fcfcfb",
            blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a")
fam <- "Avenir Next"
th <- ggplot2::theme_minimal(base_family = fam, base_size = 11) +
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = pal$paper, colour = NA),
    panel.grid.major = ggplot2::element_line(colour = pal$grid, linewidth = 0.35),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text  = ggplot2::element_text(colour = pal$ink2, size = 9),
    axis.title = ggplot2::element_text(colour = pal$ink2, size = 9.5),
    strip.text = ggplot2::element_text(colour = pal$ink, size = 10.5, face = "bold", hjust = 0,
                                       family = "Avenir Next Condensed"),
    legend.position = "none",
    plot.margin = ggplot2::margin(6, 10, 4, 4)
  )
save_png <- function(p, name, w, h) {
  ggplot2::ggsave(file.path(dir_w, name), p, width = w, height = h, dpi = 300,
                  device = ragg::agg_png, bg = pal$paper)
}
lab <- function(...) ggplot2::annotate("text", family = fam, size = 3.2, ...)
signed0 <- function(x) ifelse(abs(x) < 1e-9, "0", sprintf("%+.0f", x))
signed1 <- function(x) ifelse(abs(x) < 1e-9, "0", sprintf("%+.1f", x))

# ---- posterior from 08 -------------------------------------------------------------------------------
fit   <- readRDS(file.path(cfg$dir_out, "fit_state_space.rds"))
draws <- as.data.frame(fit$draws(par_names, format = "df"))[par_names]

# ---- Fig 1: the development curve, within-player vs pooled ------------------------------------------
ex <- 0:cfg$max_exp
cd <- sapply(ex, function(e) ss_curve(e, draws$b0, draws$b1, draws$b2, cfg$exp_center)) * 100
within <- data.frame(exp = ex, fit = colMeans(cd),
                     lo = apply(cd, 2, quantile, 0.05), hi = apply(cd, 2, quantile, 0.95))
pooled <- data.frame(exp = ex,
                     fit = as.numeric(tapply(ps$ts_rel, ps$exp, mean)) * 100,
                     se  = as.numeric(tapply(ps$ts_rel, ps$exp, sd) / sqrt(table(ps$exp))) * 100,
                     n   = as.numeric(table(ps$exp)))
print(round(within, 2)); print(round(pooled, 2))

p1 <- ggplot2::ggplot() +
  ggplot2::geom_hline(yintercept = 0, colour = pal$muted, linewidth = 0.4) +
  ggplot2::geom_ribbon(data = within, ggplot2::aes(exp, ymin = lo, ymax = hi), fill = pal$blue, alpha = 0.16) +
  ggplot2::geom_line(data = within, ggplot2::aes(exp, fit), colour = pal$blue, linewidth = 1.1) +
  ggplot2::geom_linerange(data = pooled, ggplot2::aes(exp, ymin = fit - 2 * se, ymax = fit + 2 * se),
                          colour = pal$orange, linewidth = 0.6) +
  ggplot2::geom_line(data = pooled, ggplot2::aes(exp, fit), colour = pal$orange, linewidth = 0.7, linetype = "22") +
  ggplot2::geom_point(data = pooled, ggplot2::aes(exp, fit), colour = pal$orange, fill = pal$paper,
                      shape = 21, size = 2.4, stroke = 1) +
  lab(x = 0, y = 1.35, label = "Average of everyone still in the league",
      colour = pal$orange, hjust = 0, fontface = "bold") +
  lab(x = 0, y = 1.0, label = "flat, because the survivors are better players",
      colour = pal$ink2, hjust = 0) +
  lab(x = 7.4, y = -1.9, label = "The typical player, compared with himself",
      colour = pal$blue, hjust = 0, fontface = "bold") +
  lab(x = 7.4, y = -2.3, label = "peaks around year 6, then declines",
      colour = pal$ink2, hjust = 0) +
  ggplot2::scale_x_continuous(breaks = ex, expand = ggplot2::expansion(add = c(0.3, 0.3))) +
  ggplot2::scale_y_continuous(labels = signed0, breaks = seq(-3, 1, 1)) +
  ggplot2::coord_cartesian(xlim = c(0, 12.6), ylim = c(-2.8, 1.5)) +
  ggplot2::labs(x = "Years since debut", y = "True shooting vs. league average (points)") + th
save_png(p1, "fig1_curve.png", 7.2, 3.6)

# ---- Fig 2: three careers, observed seasons vs inferred talent ----------------------------------------
p_hat <- as.list(colMeans(draws))
who <- c("LeBron James", "Carlos Boozer", "Mike Dunleavy")
lat <- do.call(rbind, lapply(who, function(nm) {
  h <- ps[ps$player_name == nm, ]; h <- h[order(h$exp), ]
  lt <- ss_latent_player(data.frame(exp = h$exp, y = h$ts_rel, se = h$se_ts), p_hat, cfg$exp_center)
  data.frame(player = nm, exp = h$exp, obs = h$ts_rel * 100, se = h$se_ts * 100,
             lat = lt$mean * 100, lsd = lt$sd * 100)
}))
lat$player <- factor(lat$player, levels = who)
p2 <- ggplot2::ggplot(lat, ggplot2::aes(exp)) +
  ggplot2::geom_hline(yintercept = 0, colour = pal$muted, linewidth = 0.4) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lat - 1.645 * lsd, ymax = lat + 1.645 * lsd),
                       fill = pal$blue, alpha = 0.16) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = obs - 1.645 * se, ymax = obs + 1.645 * se),
                          colour = pal$muted, linewidth = 0.45) +
  ggplot2::geom_point(ggplot2::aes(y = obs), colour = pal$ink2, size = 1.5) +
  ggplot2::geom_line(ggplot2::aes(y = lat), colour = pal$blue, linewidth = 1) +
  ggplot2::facet_wrap(~ player, nrow = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 12, 3)) +
  ggplot2::scale_y_continuous(labels = signed0) +
  ggplot2::labs(x = "Years since debut", y = "TS% vs. league (points)") + th +
  ggplot2::theme(panel.spacing = ggplot2::unit(14, "pt"))
save_png(p2, "fig2_players.png", 7.2, 2.5)

# ---- Fig 3: where a season's number comes from ---------------------------------------------------------
se2 <- mean(ps$se_ts^2)
vs <- c(`Permanent talent` = mean(draws$tau_a^2), `Short-term form` = mean(draws$tau_z^2),
        `Sampling luck` = se2, `Other noise` = mean(draws$sigma^2))
vd <- data.frame(part = names(vs), share = as.numeric(vs / sum(vs)) * 100)
vd$part <- factor(vd$part, levels = rev(vd$part))
print(vd)
p3 <- ggplot2::ggplot(vd, ggplot2::aes(share, part)) +
  ggplot2::geom_col(fill = c(pal$blue, pal$aqua, pal$orange, pal$muted), width = 0.62) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", share)), hjust = -0.15, family = fam,
                     size = 3.4, colour = pal$ink, fontface = "bold") +
  ggplot2::scale_x_continuous(limits = c(0, 48), breaks = NULL, expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = NULL) + th +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(colour = pal$ink, size = 10))
save_png(p3, "fig3_variance.png", 3.5, 2.1)

# ---- Fig 4: how fast a hot (or cold) season fades -------------------------------------------------------
gg <- seq(0, 6, by = 0.1)
dm <- sapply(gg, function(g) draws$phi^g)
decay <- data.frame(g = gg, fit = colMeans(dm), lo = apply(dm, 2, quantile, 0.05),
                    hi = apply(dm, 2, quantile, 0.95))
hl <- mean(log(0.5) / log(draws$phi))
message("phi ", round(mean(draws$phi), 3), "  half-life ", round(hl, 2))
p4 <- ggplot2::ggplot(decay, ggplot2::aes(g, fit * 100)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo * 100, ymax = hi * 100), fill = pal$aqua, alpha = 0.18) +
  ggplot2::geom_line(colour = pal$aqua, linewidth = 1.1) +
  ggplot2::geom_segment(x = hl, xend = hl, y = 0, yend = 50, colour = pal$muted, linetype = "22", linewidth = 0.4) +
  ggplot2::geom_point(data = data.frame(g = hl, fit = 0.5), colour = pal$aqua, fill = pal$paper,
                      shape = 21, size = 2.6, stroke = 1.1) +
  lab(x = hl + 0.2, y = 56, label = sprintf("half gone after %.1f seasons", hl), colour = pal$ink, hjust = 0) +
  ggplot2::scale_x_continuous(breaks = 0:6) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 25, 50, 75, 100)) +
  ggplot2::coord_cartesian(ylim = c(0, 100)) +
  ggplot2::labs(x = "Seasons later", y = "Form still present") + th
save_png(p4, "fig4_decay.png", 3.5, 2.1)

# ---- Fig 5: two clocks ------------------------------------------------------------------------------------
pm <- readRDS(file.path(cfg$dir_data, "prior_minutes.rds"))
pd <- merge(ps, pm, by = c("player_id", "season"))
pd$pid <- factor(pd$player_id); pd$w <- pd$tsa / mean(pd$tsa)
fit_b <- function(f) mgcv::bam(f, data = pd, weights = w, method = "fREML", discrete = TRUE)
m_e <- fit_b(ts_rel ~ s(exp, k = 6) + s(pid, bs = "re"))
m_m <- fit_b(ts_rel ~ s(lcm, k = 6) + s(pid, bs = "re"))
curve_of <- function(m, var, grid) {
  nd <- data.frame(exp = 3, lcm = median(pd$lcm), pid = pd$pid[1])[rep(1, length(grid)), ]
  nd[[var]] <- grid
  pr <- predict(m, newdata = nd, se.fit = TRUE, exclude = "s(pid)")
  data.frame(fit = pr$fit * 100, lo = (pr$fit - 1.96 * pr$se.fit) * 100, hi = (pr$fit + 1.96 * pr$se.fit) * 100)
}
cmax <- as.numeric(quantile(pd$cm, 0.99))
cg <- seq(0, cmax, length.out = 80)
c5 <- rbind(cbind(clock = "Years since debut", x = ex, curve_of(m_e, "exp", ex)),
            cbind(clock = "Career minutes before the season (thousands)", x = cg, curve_of(m_m, "lcm", log1p(cg))))
c5$clock <- factor(c5$clock, levels = unique(c5$clock))
p5 <- ggplot2::ggplot(c5, ggplot2::aes(x, fit)) +
  ggplot2::geom_hline(yintercept = 0, colour = pal$muted, linewidth = 0.4) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = pal$blue, alpha = 0.16) +
  ggplot2::geom_line(colour = pal$blue, linewidth = 1.1) +
  ggplot2::facet_wrap(~ clock, scales = "free_x") +
  ggplot2::scale_x_continuous(breaks = function(l) if (l[2] < 15) seq(0, 12, 3) else seq(0, 25, 5)) +
  ggplot2::scale_y_continuous(labels = signed1) +
  ggplot2::labs(x = NULL, y = "TS% vs. league (points)") + th +
  ggplot2::theme(panel.spacing = ggplot2::unit(18, "pt"))
save_png(p5, "fig5_clocks.png", 7.2, 2.4)

# ---- Fig 6: the scoreboard --------------------------------------------------------------------------------
ss  <- readRDS(file.path(cfg$dir_out, "holdout_predictions_ss.rds"))
br  <- readRDS(file.path(cfg$dir_out, "holdout_predictions.rds"))
sty <- readRDS(file.path(cfg$dir_out, "holdout_predictions_ss_style.rds"))
wrmse <- function(obs, pred, w) sqrt(weighted.mean((obs - pred)^2, w))
sb <- data.frame(
  model = c("Last season, carried forward", "Last season, league-adjusted", "Average development curve",
            "Bayesian growth curve (brms)", "State space + last season's style", "State space"),
  err = c(wrmse(ss$ts, ss$pred_naive_raw, ss$tsa), wrmse(ss$ts, ss$pred_naive_adj, ss$tsa),
          wrmse(ss$ts, ss$pred_curve_adj, ss$tsa), wrmse(br$ts, br$pred_brms, br$tsa),
          wrmse(sty$ts, sty$pred_cov, sty$tsa), wrmse(ss$ts, ss$pred_ss, ss$tsa)) * 100)
sb <- sb[order(-sb$err), ]
sb$model <- factor(sb$model, levels = sb$model)
sb$best <- sb$model %in% c("State space", "State space + last season's style")
print(sb)
message("State-space 90% coverage: ", round(mean(ss$ts >= ss$lo90 & ss$ts <= ss$hi90), 3),
        "  rows: ", nrow(ss), "  players: ", length(unique(ss$player_id)))
p6 <- ggplot2::ggplot(sb, ggplot2::aes(err, model)) +
  ggplot2::geom_segment(ggplot2::aes(x = 3, xend = err, yend = model), colour = pal$grid, linewidth = 1.6) +
  ggplot2::geom_point(ggplot2::aes(colour = best), size = 3.4) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", err)), nudge_x = 0.06, hjust = 0, family = fam,
                     size = 3.2, colour = pal$ink) +
  ggplot2::scale_colour_manual(values = c(`TRUE` = pal$blue, `FALSE` = pal$muted)) +
  ggplot2::scale_x_continuous(limits = c(3, 4.75), breaks = seq(3, 4.5, 0.5), expand = c(0, 0)) +
  ggplot2::labs(x = "Forecast error, 2023 to 2026 seasons (TS% points, weighted RMSE)", y = NULL) + th +
  ggplot2::theme(axis.text.y = ggplot2::element_text(colour = pal$ink, size = 9.5),
                 panel.grid.major.y = ggplot2::element_blank())
save_png(p6, "fig6_scoreboard.png", 7.2, 2.3)

message("Saved to ", dir_w)
