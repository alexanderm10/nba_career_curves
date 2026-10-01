# 01_pull_box.R
# Pull game-level player box scores and save them untouched.

source("00_setup.R")

t0 <- Sys.time()
box_raw <- hoopR::load_nba_player_box(seasons = cfg$seasons)
message("Pull took ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " seconds")

saveRDS(box_raw, file.path(cfg$dir_data, "box_raw.rds"))

# Checks to eyeball
message("Rows: ", nrow(box_raw), "  Columns: ", ncol(box_raw))
print(table(box_raw[[cols$season]]))
print(names(box_raw))
