# 12_team_style.R
# Stage B. Do strong teams occupy a different region of style space than weak teams,
# and does that gap change across eras? Team-season unit of analysis.
#   centroid   minutes-weighted mean style score of the players on the team
#   dispersion minutes-weighted mean distance of players from that centroid (roster variety)
#   quality    mean point margin per game
# Needs the vegan and permute packages.

source("00_setup.R")
source("style_helpers.R")
for (pkg in c("vegan", "permute")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install the ", pkg, " package first.")
}

sc  <- readRDS(file.path(cfg$dir_data, "style_scores.rds"))
tm  <- readRDS(file.path(cfg$dir_data, "team_season_margin.rds"))
pts <- readRDS(file.path(cfg$dir_data, "player_team_season.rds"))
pcs  <- paste0("pc", seq_len(cfg$style_k))
cpcs <- paste0("c_", pcs)

# ---- Step 1: team centroids and dispersion -------------------------------------------
cen <- sty_team_centroid(sc, pcs)
message("Team-season centroids: ", nrow(cen))

# How much of each team's minutes carry a style score? Low coverage makes a centroid unreliable.
tot <- aggregate(min ~ team_id + season, data = pts, FUN = sum)
names(tot)[3] <- "team_min"
cen <- merge(cen, tot, by = c("team_id", "season"))
cen$coverage <- cen$scored_min / cen$team_min
print(summary(cen$coverage))
message("Team-seasons with coverage below 0.6: ", sum(cen$coverage < 0.6))

# ---- Step 2: attach team quality ---------------------------------------------------------
d <- merge(cen, tm, by = c("team_id", "season"))
message("Rows after merging margins: ", nrow(d), " of ", nrow(cen))
stopifnot(nrow(d) > 0)
print(summary(d[c(cpcs, "disp", "margin_pg")]))

# ---- Step 3: tiers within season and era blocks ------------------------------------------
d$rank   <- ave(-d$margin_pg, d$season, FUN = function(x) rank(x, ties.method = "first"))
d$n_team <- ave(d$margin_pg, d$season, FUN = length)
d$tier   <- ifelse(d$rank <= cfg$tier_n, "top",
                   ifelse(d$rank > d$n_team - cfg$tier_n, "bottom", "middle"))
d$era    <- cut(d$season, cfg$era_cuts, labels = c("early", "mid", "late"), right = FALSE)   # [2002,2009) [2009,2016) [2016,...)
print(table(d$era, d$tier))

# ---- Step 4: how far apart are top and bottom teams, season by season? -----------------------
mean_by <- aggregate(d[c(cpcs, "disp")], by = list(season = d$season, tier = d$tier), FUN = mean)
top <- mean_by[mean_by$tier == "top", ]
bot <- mean_by[mean_by$tier == "bottom", ]
gap <- merge(top, bot, by = "season", suffixes = c("_top", "_bot"))
gap_long <- do.call(rbind, lapply(c(cpcs, "disp"), function(v) {
  data.frame(season = gap$season, metric = v, gap = gap[[paste0(v, "_top")]] - gap[[paste0(v, "_bot")]])
}))
print(round(tapply(gap_long$gap, list(gap_long$season, gap_long$metric), mean), 3))

p_gap <- ggplot2::ggplot(gap_long, ggplot2::aes(season, gap)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
  ggplot2::geom_line() + ggplot2::geom_point(size = 1) +
  ggplot2::facet_wrap(~ metric, scales = "free_y") +
  ggplot2::labs(x = "Season", y = "Top tier minus bottom tier",
                title = "Style gap between strong and weak teams over time") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(cfg$dir_out, "12_style_gap.png"), p_gap, width = 9, height = 6, dpi = 150)

p_ord <- ggplot2::ggplot(d[d$tier != "middle", ], ggplot2::aes(c_pc1, c_pc2, colour = tier)) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::stat_ellipse(level = 0.68) +
  ggplot2::facet_wrap(~ era) +
  ggplot2::labs(x = "Team centroid, PC1", y = "Team centroid, PC2",
                title = "Team style centroids, top vs bottom tier") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(cfg$dir_out, "12_ordination.png"), p_ord, width = 9, height = 4, dpi = 150)

# ---- Step 5: continuous version, margin on centroid and dispersion --------------------------
frm <- as.formula(paste("margin_pg ~", paste(c(cpcs, "disp"), collapse = " + "), "+ factor(season)"))

coef_table <- function(dd, formula) {
  fit <- lm(formula, data = dd)
  cf  <- summary(fit)$coefficients
  cf  <- cf[!grepl("factor", rownames(cf)), , drop = FALSE]
  data.frame(term = rownames(cf), est = cf[, "Estimate"], se = cf[, "Std. Error"],
             p = cf[, "Pr(>|t|)"], r2 = summary(fit)$r.squared, row.names = NULL)
}

all_tab <- coef_table(d, frm)
print(transform(all_tab, est = round(est, 3), se = round(se, 3), p = round(p, 4), r2 = round(r2, 3)))
era_tab <- do.call(rbind, lapply(levels(d$era), function(e) {
  cbind(era = e, coef_table(d[d$era == e, ], frm))
}))
print(transform(era_tab, est = round(est, 3), se = round(se, 3), p = round(p, 4), r2 = round(r2, 3)))

# ---- Step 6: PERMANOVA (centroid location) and betadisper (spread), top vs bottom ---------------
run_perm <- function(dd, vars) {
  dst  <- dist(dd[vars])
  perm <- permute::how(nperm = cfg$n_perm, blocks = factor(dd$season))
  ad   <- vegan::adonis2(dst ~ tier, data = dd, permutations = perm)
  bd   <- vegan::betadisper(dst, factor(dd$tier))
  pt   <- vegan::permutest(bd, permutations = cfg$n_perm)
  data.frame(n = nrow(dd), R2_location = ad$R2[1], p_location = ad[["Pr(>F)"]][1],
             p_spread = pt$tab[["Pr(>F)"]][1])
}

tb <- d[d$tier %in% c("top", "bottom"), ]
perm_tab <- rbind(cbind(era = "all", run_perm(tb, cpcs)),
                  do.call(rbind, lapply(levels(d$era), function(e) {
                    cbind(era = e, run_perm(tb[tb$era == e, ], cpcs))
                  })))
print(transform(perm_tab, R2_location = round(R2_location, 3)))
message("PERMANOVA is sensitive to differences in spread as well as location, so read p_location")
message("alongside p_spread. A small p_spread means the location test may be picking up spread.")

# ---- Step 7: repeat without talent-heavy components ----------------------------------------------
tal_file <- file.path(cfg$dir_out, "pc_talent_cor.rds")
if (file.exists(tal_file)) {
  tal     <- readRDS(tal_file)
  flagged <- tal$pc[!is.na(tal$max_abs) & tal$max_abs >= cfg$talent_cor]
  keep    <- setdiff(pcs, flagged)
  message("Flagged as talent-heavy: ", if (length(flagged)) paste(flagged, collapse = ", ") else "none")
  if (length(flagged) > 0 && length(keep) >= 2) {
    ckeep <- paste0("c_", keep)
    print(cbind(era = "all", run_perm(tb, ckeep)))
    frm_k <- as.formula(paste("margin_pg ~", paste(c(ckeep, "disp"), collapse = " + "), "+ factor(season)"))
    print(transform(coef_table(d, frm_k), est = round(est, 3), se = round(se, 3), p = round(p, 4)))
  }
} else {
  message("No pc_talent_cor.rds, run 11 first to enable the talent-heavy check")
}

saveRDS(d, file.path(cfg$dir_out, "team_style.rds"))
write.csv(perm_tab, file.path(cfg$dir_out, "12_permanova_by_era.csv"), row.names = FALSE)
write.csv(era_tab,  file.path(cfg$dir_out, "12_regression_by_era.csv"), row.names = FALSE)
