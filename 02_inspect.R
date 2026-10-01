# 02_inspect.R
# Look at the raw pull before building anything on it.

source("00_setup.R")
box <- readRDS(file.path(cfg$dir_data, "box_raw.rds"))

# 1. Do the columns we need exist under the names in cfg?
need <- unlist(cols)
absent <- setdiff(need, names(box))
if (length(absent) > 0) {
  stop("Columns not found: ", paste(absent, collapse = ", "),
       "\nEdit the `cols` list in 00_setup.R to match names(box).")
}

# 2. Season types and games per season (should be a steady ~1200 regular season games
#    per season, times ~20 to 30 player rows per game)
print(table(box[[cols$season_type]], useNA = "ifany"))
print(table(box[[cols$season]], box[[cols$season_type]]))

# 3. Missingness in the fields that drive the metric
key_cols <- c(cols$minutes, cols$points, cols$fga, cols$fta)
print(sapply(box[key_cols], function(x) sum(is.na(x))))

# 4. Type check. These should be numeric. If they are character, 03 converts them.
print(sapply(box[key_cols], class))

# 5. Duplicate player-game rows would double count. Game id column name varies
#    by hoopR version, so this looks for it.
gid <- intersect(c("game_id", "gameId"), names(box))[1]
if (!is.na(gid)) {
  dups <- sum(duplicated(box[, c(gid, cols$id)]))
  message("Duplicate player-game rows: ", dups)
} else {
  message("No game id column found, skipping duplicate check")
}

# 6. A quick look at a few rows
print(utils::head(box[, need], 10))
