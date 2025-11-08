build_nab_realAdExchange <- function() {
  files <- list.files(path = "original_datasets/numenta/data/ungrouped/realAdExchange", pattern = "\\.RData$", full.names = TRUE)
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

nab_realAdExchange <- build_nab_realAdExchange()
save(nab_realAdExchange, file = "data/nab_realAdExchange.RData", compress = "xz")

