# 18_build_writeup.R
# Embed the figures from 17 into the two-page write-up as data URIs, so the page is one self-contained file.
#   in:  writeup/two_pager_src.html  (placeholders {{fig1}} ... {{fig6}})
#   out: writeup/two_pager.html

src  <- paste(readLines("writeup/two_pager_src.html", warn = FALSE), collapse = "\n")
figs <- c(fig1 = "fig1_curve.png", fig2 = "fig2_players.png", fig3 = "fig3_variance.png",
          fig4 = "fig4_decay.png", fig5 = "fig5_clocks.png", fig6 = "fig6_scoreboard.png")
for (k in names(figs)) {
  path <- file.path("output", "writeup", figs[[k]])
  uri  <- paste0("data:image/png;base64,", base64enc::base64encode(path))
  src  <- gsub(paste0("{{", k, "}}"), uri, src, fixed = TRUE)
}
stopifnot(!grepl("{{", src, fixed = TRUE))
writeLines(src, "writeup/two_pager.html")
message("Wrote writeup/two_pager.html (", round(file.size("writeup/two_pager.html") / 1e6, 2), " MB)")
