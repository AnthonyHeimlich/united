build_nab_realKnownCause <- function() {
  files <- list.files(path = "original_datasets/numenta/data/ungrouped/realKnownCause", pattern = "\\.RData$", full.names = TRUE)
  out <- list()
  for (i in seq_along(files)) {
    load(files[i])
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

nab_realKnownCause <- build_nab_realKnownCause()
save(nab_realKnownCause, file = "data/nab_realKnownCause.RData", compress = "xz")

