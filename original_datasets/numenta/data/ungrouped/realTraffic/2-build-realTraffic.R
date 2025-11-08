build_nab_realTraffic <- function() {
  files <- list.files(path = "original_datasets/numenta/data/ungrouped/realTraffic", pattern = "\\.RData$", full.names = TRUE)
  out <- list()
  for (i in seq_along(files)) {
    load(files[i]) # loads 'series'
    df <- series
    df$event <- as.logical(df$event)
    idx <- 1:nrow(df)
    df <- cbind(data.frame(idx), df)
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
    nm <- tools::file_path_sans_ext(basename(files[i]))
    names(out)[i] <- nm
  }
  out
}

nab_realTraffic <- build_nab_realTraffic()
save(nab_realTraffic, file = "data/nab_realTraffic.RData", compress = "xz")
