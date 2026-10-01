# style_helpers.R
# Small single-purpose functions for the style space and team study (scripts 10 to 13).
# Each takes plain data frames so it can be checked on its own.

style_vars <- c("fga36", "fg3a_rate", "fta_rate", "ast36", "tov36",
                "orb36", "drb36", "stl36", "blk36", "pf36")
# Shooting efficiency (TS%, FG%, 3P%) is deliberately left out of the style inputs so
# that style stays separate from shooting skill, which is the outcome in the state-space model.

count_cols <- c("pts", "fga", "fta", "fg3a", "oreb", "dreb", "ast", "stl", "blk", "tov", "pf")

sty_num <- function(x) suppressWarnings(as.numeric(x))

# Stop if required columns are missing, warn if optional ones are
sty_check_cols <- function(box, cols2) {
  optional <- c("pm", "pos")
  need   <- unlist(cols2[setdiff(names(cols2), optional)])
  absent <- setdiff(need, names(box))
  if (length(absent) > 0) {
    stop("Columns not found: ", paste(absent, collapse = ", "),
         "\nEdit cols2 in 00_setup.R to match names(box).")
  }
  miss_opt <- setdiff(unlist(cols2[optional]), names(box))
  if (length(miss_opt) > 0) {
    message("Optional columns missing, related checks will be skipped: ",
            paste(miss_opt, collapse = ", "))
  }
  invisible(TRUE)
}

# One small data frame with standard names and types
sty_standardize_box <- function(box, cols, cols2) {
  pick_num <- function(key) {
    nm <- cols2[[key]]
    if (nm %in% names(box)) sty_num(box[[nm]]) else rep(NA_real_, nrow(box))
  }
  pos <- if (cols2$pos %in% names(box)) as.character(box[[cols2$pos]]) else rep(NA_character_, nrow(box))
  data.frame(
    player_id   = as.character(box[[cols$id]]),
    player_name = box[[cols$name]],
    season      = as.integer(box[[cols$season]]),
    season_type = box[[cols$season_type]],
    game_id     = as.character(box[[cols2$game_id]]),
    team_id     = as.character(box[[cols2$team_id]]),
    minutes     = sty_num(box[[cols$minutes]]),
    pts         = sty_num(box[[cols$points]]),
    fga         = sty_num(box[[cols$fga]]),
    fta         = sty_num(box[[cols$fta]]),
    fg3a        = pick_num("fg3a"),
    oreb        = pick_num("oreb"),
    dreb        = pick_num("dreb"),
    ast         = pick_num("ast"),
    stl         = pick_num("stl"),
    blk         = pick_num("blk"),
    tov         = pick_num("tov"),
    pf          = pick_num("pf"),
    pm          = pick_num("pm"),
    pos         = pos,
    dnp         = box[[cols$dnp]] %in% TRUE,
    stringsAsFactors = FALSE
  )
}

# Regular season rows where the player actually played
sty_played <- function(std) {
  ok <- std$season_type == 2 & !std$dnp & !is.na(std$minutes) & std$minutes > 0
  std[ok, ]
}

# Rows with every counting stat present
sty_complete <- function(played) {
  played[complete.cases(played[, count_cols]), ]
}

# Number of distinct teams per game. Should be exactly 2 almost everywhere.
sty_game_check <- function(played) {
  table(tapply(played$team_id, played$game_id, function(x) length(unique(x))))
}

# Mean point margin per game for each team-season, from summed player points
sty_team_margin <- function(played) {
  played <- played[!is.na(played$pts), ]
  tg  <- aggregate(pts ~ game_id + team_id + season, data = played, FUN = sum)
  opp <- tg[, c("game_id", "team_id", "pts")]
  names(opp) <- c("game_id", "opp_id", "opp_pts")
  m <- merge(tg, opp, by = "game_id")
  m <- m[m$team_id != m$opp_id, ]
  m$margin <- m$pts - m$opp_pts
  out <- aggregate(margin ~ team_id + season, data = m, FUN = mean)
  names(out)[3] <- "margin_pg"
  gm <- aggregate(margin ~ team_id + season, data = m, FUN = length)
  out$games <- gm$margin[match(paste(out$team_id, out$season), paste(gm$team_id, gm$season))]
  out
}

# Totals by player, team, and season (traded players get one row per team)
sty_player_team_season <- function(d) {
  out <- d |>
    dplyr::group_by(player_id, player_name, team_id, season) |>
    dplyr::summarise(
      games = dplyr::n(), min = sum(minutes),
      pts = sum(pts), fga = sum(fga), fta = sum(fta), fg3a = sum(fg3a),
      oreb = sum(oreb), dreb = sum(dreb), ast = sum(ast), stl = sum(stl),
      blk = sum(blk), tov = sum(tov), pf = sum(pf),
      pm = sum(pm, na.rm = TRUE), pm_n = sum(!is.na(pm)),
      pos = dplyr::first(pos),
      .groups = "drop"
    )
  as.data.frame(out)
}

# Per-36 rates and shot-mix shares
sty_rates <- function(d) {
  per36 <- function(x) x / d$min * 36
  d$fga36     <- per36(d$fga)
  d$ast36     <- per36(d$ast)
  d$tov36     <- per36(d$tov)
  d$orb36     <- per36(d$oreb)
  d$drb36     <- per36(d$dreb)
  d$stl36     <- per36(d$stl)
  d$blk36     <- per36(d$blk)
  d$pf36      <- per36(d$pf)
  d$fg3a_rate <- ifelse(d$fga > 0, d$fg3a / d$fga, NA_real_)
  d$fta_rate  <- ifelse(d$fga > 0, d$fta / d$fga, NA_real_)
  d$pm36      <- ifelse(d$pm_n > 0, d$pm / d$min * 36, NA_real_)
  d
}

sty_skew <- function(x) {
  x <- x[is.finite(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

# z-score each variable within season so era drift does not become a component
sty_era_z <- function(d, vars) {
  z <- d[, vars, drop = FALSE]
  for (s in unique(d$season)) {
    i <- d$season == s
    if (sum(i) > 1) z[i, ] <- scale(d[i, vars, drop = FALSE])
  }
  names(z) <- paste0("z_", vars)
  cbind(d, z)
}

# Flip each component so its largest loading is positive (prcomp signs are arbitrary)
sty_fix_sign <- function(rot) {
  for (j in seq_len(ncol(rot))) {
    if (rot[which.max(abs(rot[, j])), j] < 0) rot[, j] <- -rot[, j]
  }
  rot
}

# Highest and lowest loading variables for each component
sty_top_loadings <- function(load, n = 3) {
  do.call(rbind, lapply(colnames(load), function(pc) {
    v <- load[, pc]
    o <- order(v)
    data.frame(pc = pc,
               high = paste(rownames(load)[rev(tail(o, n))], collapse = ", "),
               low  = paste(rownames(load)[head(o, n)], collapse = ", "))
  }))
}

# Tucker congruence between two loading matrices (absolute, sign-free)
sty_congruence_matrix <- function(A, B) {
  abs(t(A) %*% B) / outer(sqrt(colSums(A^2)), sqrt(colSums(B^2)))
}

# Minutes-weighted centroid and dispersion of player scores for each team-season
sty_team_centroid <- function(sc, pcs) {
  key <- paste(sc$team_id, sc$season)
  parts <- lapply(split(seq_len(nrow(sc)), key), function(ix) {
    w   <- sc$min[ix] / sum(sc$min[ix])
    m   <- as.matrix(sc[ix, pcs, drop = FALSE])
    cen <- colSums(m * w)
    dst <- sqrt(rowSums(sweep(m, 2, cen)^2))
    info <- data.frame(team_id = sc$team_id[ix[1]], season = sc$season[ix[1]],
                       n_players = length(ix), scored_min = sum(sc$min[ix]),
                       disp = sum(w * dst))
    cen_df <- as.data.frame(t(cen))
    names(cen_df) <- paste0("c_", pcs)
    cbind(info, cen_df)
  })
  do.call(rbind, parts)
}

# Minutes-weighted style scores for each player-season (across a player's team stints)
sty_player_season_scores <- function(sc, pcs) {
  key <- paste(sc$player_id, sc$season)
  parts <- lapply(split(seq_len(nrow(sc)), key), function(ix) {
    w <- sc$min[ix] / sum(sc$min[ix])
    m <- as.matrix(sc[ix, pcs, drop = FALSE])
    r <- as.data.frame(t(colSums(m * w)))
    names(r) <- pcs
    cbind(data.frame(player_id = sc$player_id[ix[1]], season = sc$season[ix[1]],
                     style_min = sum(sc$min[ix])), r)
  })
  do.call(rbind, parts)
}
