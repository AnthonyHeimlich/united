build_nab_artificialWithAnomaly <- function() {
  files <- list.files(path = "original_datasets/numenta/data/ungrouped/artificialWithAnomaly", pattern = "\\.RData$", full.names = TRUE)
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

nab_artificialWithAnomaly <- build_nab_artificialWithAnomaly()
save(nab_artificialWithAnomaly, file = "data/nab_artificialWithAnomaly.RData", compress = "xz")

