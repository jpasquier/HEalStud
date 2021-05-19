cb0 <- cb$all
for (tps in c(0, 0.5)) {
  cb0[[sub("\\.", "", paste0("T", tps))]] <- sapply(1:nrow(cb0), function(i) {
    v <- cb0[i, "variable"]
    k <- cb0[i, "code"]
    as.integer(tps %in% unique(hs[hs[[v]] == k, "Temps"]))
  })
}
