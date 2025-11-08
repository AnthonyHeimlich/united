build_nab_realAWSCloudwatch <- function() {
  files <- list.files(path = "original_datasets/numenta/data/ungrouped/realAWSCloudwatch", pattern = "\\.RData$", full.names = TRUE)
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

nab_realAWSCloudwatch <- build_nab_realAWSCloudwatch()
save(nab_realAWSCloudwatch, file = "data/nab_realAWSCloudwatch.RData", compress = "xz")

