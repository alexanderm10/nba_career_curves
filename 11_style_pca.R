# 11_style_pca.R
# Stage A, step 2. PCA on era-standardized per-36 rates, fit on a reference period and
# projected onto all seasons. Prints what each component means and whether it is stable.

source("00_setup.R")
source("style_helpers.R")

pts <- readRDS(file.path(cfg$dir_data, "player_team_season.rds"))
pcs <- paste0("pc", seq_len(cfg$style_k))
zv  <- paste0("z_", style_vars)

# ---- Step 1: eligible rows -----------------------------------------------------
elig <- pts[pts$min >= cfg$style_min_min & pts$fga > 0, ]
message("Eligible player-team-seasons: ", nrow(elig))
print(table(elig$season))

# ---- Step 2: transform the skewed rates, check skew before and after -----------
skew_before <- sapply(elig[style_vars], sty_skew)
for (v in cfg$style_sqrt) elig[[v]] <- sqrt(elig[[v]])
skew_after <- sapply(elig[style_vars], sty_skew)
print(round(cbind(before = skew_before, after = skew_after), 2))
# If any variable still has |skew| above about 1.5, add it to cfg$style_sqrt and re-run.

# ---- Step 3: standardize within season -----------------------------------------
elig <- sty_era_z(elig, style_vars)
stopifnot(!anyNA(elig[zv]))

# ---- Step 4: fit on the reference period -----------------------------------------
ref <- elig[elig$season >= cfg$style_ref_from & elig$season <= cfg$style_ref_to, ]
message("Reference rows: ", nrow(ref))
pc_ref <- prcomp(ref[zv], center = TRUE, scale. = TRUE)
pc_ref$rotation <- sty_fix_sign(pc_ref$rotation)

# ---- Step 5: scree -----------------------------------------------------------------
scree <- data.frame(pc = seq_along(pc_ref$sdev), sd = pc_ref$sdev,
                    prop = pc_ref$sdev^2 / sum(pc_ref$sdev^2))
scree$cum <- cumsum(scree$prop)
print(round(scree, 3))
message("Choose cfg$style_k from this table and the loadings below. Currently ", cfg$style_k)
p_scree <- ggplot2::ggplot(scree, ggplot2::aes(pc, prop)) +
  ggplot2::geom_col() + ggplot2::geom_line() +
  ggplot2::scale_x_continuous(breaks = scree$pc) +
  ggplot2::labs(x = "Component", y = "Share of variance", title = "Scree, reference period") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(cfg$dir_out, "11_scree.png"), p_scree, width = 6, height = 4, dpi = 150)

# ---- Step 6: loadings and what they mean -------------------------------------------
load <- pc_ref$rotation[, seq_len(cfg$style_k), drop = FALSE]
colnames(load) <- pcs
rownames(load) <- style_vars
print(round(load, 2))
print(sty_top_loadings(load, n = 3))
write.csv(round(load, 4), file.path(cfg$dir_out, "11_loadings.csv"))

ld <- data.frame(var = rep(style_vars, times = cfg$style_k),
                 pc  = rep(pcs, each = length(style_vars)),
                 loading = as.vector(load))
p_load <- ggplot2::ggplot(ld, ggplot2::aes(loading, var)) +
  ggplot2::geom_col() + ggplot2::facet_wrap(~ pc, nrow = 1) +
  ggplot2::labs(x = "Loading", y = NULL, title = "Style component loadings") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(cfg$dir_out, "11_loadings.png"), p_load, width = 9, height = 4, dpi = 150)

# ---- Step 7: project every eligible row onto the reference components ----------------
sc_all <- predict(pc_ref, newdata = elig[zv])
elig[pcs] <- sc_all[, seq_len(cfg$style_k)]
print(round(sapply(elig[pcs], sd), 2))
print(round(tapply(elig$pc1, elig$season, mean), 2))   # should hover near 0 across eras

# ---- Step 8: face validity by listed position ----------------------------------------
if (any(!is.na(elig$pos))) {
  by_pos <- aggregate(elig[pcs[seq_len(min(3, cfg$style_k))]], by = list(pos = elig$pos), FUN = mean)
  by_pos$n <- as.integer(table(elig$pos)[by_pos$pos])
  print(by_pos[order(-by_pos$n), ])
} else {
  message("No position column, skipping the position check")
}

# ---- Step 9: how talent-heavy is each component? --------------------------------------
tab <- data.frame(pc = pcs, cor_pm36 = NA_real_, cor_ts_rel = NA_real_)
if (any(!is.na(elig$pm36))) {
  tab$cor_pm36 <- as.numeric(cor(elig[pcs], elig$pm36, use = "pairwise.complete.obs"))
}
ps_file <- file.path(cfg$dir_data, "player_season.rds")
if (file.exists(ps_file)) {
  ps <- readRDS(ps_file)
  mg <- merge(elig[c("player_id", "season", pcs)], ps[c("player_id", "season", "ts_rel")],
              by = c("player_id", "season"))
  message("Rows matched to player_season for the efficiency check: ", nrow(mg))
  tab$cor_ts_rel <- as.numeric(cor(mg[pcs], mg$ts_rel, use = "pairwise.complete.obs"))
}
tab$max_abs <- pmax(abs(tab$cor_pm36), abs(tab$cor_ts_rel), na.rm = TRUE)
print(round(tab[, -1], 3))
message("Components at or above ", cfg$talent_cor, " are treated as talent-heavy in 12.")
saveRDS(tab, file.path(cfg$dir_out, "pc_talent_cor.rds"))

# ---- Step 10: stability, reference fit vs a late-period fit ----------------------------
late <- elig[elig$season >= cfg$style_late_from, ]
message("Late-period rows: ", nrow(late))
pc_late <- prcomp(late[zv], center = TRUE, scale. = TRUE)
cong <- sty_congruence_matrix(pc_ref$rotation[, seq_len(cfg$style_k)],
                              pc_late$rotation[, seq_len(cfg$style_k)])
dimnames(cong) <- list(paste0("ref_", pcs), paste0("late_", pcs))
print(round(cong, 2))
message("Values near 1 on the diagonal mean the same axes appear in both eras.")
message("Large off-diagonal values mean components swapped or rotated between eras.")

# ---- Step 11: face validity, extreme players among high-minute rows ---------------------
big <- elig[elig$min >= 1500, ]
for (pc in pcs[seq_len(min(3, cfg$style_k))]) {
  o <- order(big[[pc]])
  message(pc, " highest:")
  print(big[rev(tail(o, 5)), c("player_name", "season", pc)])
  message(pc, " lowest:")
  print(big[head(o, 5), c("player_name", "season", pc)])
}

# ---- Save -----------------------------------------------------------------------------------
saveRDS(elig[c("player_id", "player_name", "team_id", "season", "min", "pos", pcs)],
        file.path(cfg$dir_data, "style_scores.rds"))
saveRDS(pc_ref, file.path(cfg$dir_out, "pc_ref.rds"))
