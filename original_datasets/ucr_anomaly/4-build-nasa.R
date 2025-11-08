build_ucr_nasa <- function() {
  load(file.path("original_datasets", "ucr_anomaly", "data", "labeled", "ucr_nasa.RData"))
  out <- list()
  for (i in seq_along(ucr_nasa)) {
    idx <- 1:nrow(ucr_nasa[[i]])
    df <- cbind(data.frame(idx), ucr_nasa[[i]])
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
    names(out)[i] <- names(ucr_nasa)[i]
  }
  out
}

ucr_nasa <- build_ucr_nasa()
save(ucr_nasa, file = "data/ucr_nasa.RData", compress = "xz")
