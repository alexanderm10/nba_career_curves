# 04_baseline.R
# Simplest reasonable model: one pooled curve for everyone, ignoring that the
# same player appears in several rows. Anything fancier has to beat this.

source("00_setup.R")
ps <- readRDS(file.path(cfg$dir_data, "player_season.rds"))

t0 <- Sys.time()
m_gam <- mgcv::gam(ts ~ s(exp, k = 6), data = ps, weights = tsa, method = "REML")
message("GAM fit seconds: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2))

print(summary(m_gam))
mgcv::gam.check(m_gam)   # prints k-index check, and draws residual plots

grid <- data.frame(exp = 0:cfg$max_exp)
pr <- predict(m_gam, newdata = grid, se.fit = TRUE)
grid$fit <- pr$fit
grid$lo  <- pr$fit - 1.96 * pr$se.fit
grid$hi  <- pr$fit + 1.96 * pr$se.fit
print(grid)

p <- ggplot2::ggplot(ps, ggplot2::aes(exp, ts)) +
  ggplot2::geom_jitter(ggplot2::aes(size = tsa), width = 0.15, alpha = 0.08, colour = "grey30") +
  ggplot2::geom_ribbon(data = grid, ggplot2::aes(y = fit, ymin = lo, ymax = hi), alpha = 0.3) +
  ggplot2::geom_line(data = grid, ggplot2::aes(y = fit), linewidth = 1) +
  ggplot2::coord_cartesian(ylim = c(0.40, 0.70)) +
  ggplot2::labs(x = "Years since debut", y = "True shooting %", size = "Attempts",
                title = "Pooled baseline: TS% by experience") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(cfg$dir_out, "04_baseline.png"), p, width = 7, height = 5, dpi = 150)

saveRDS(m_gam, file.path(cfg$dir_out, "m_gam.rds"))
