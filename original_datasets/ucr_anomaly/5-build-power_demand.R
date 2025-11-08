build_ucr_power_demand <- function() {
  load(file.path("original_datasets", "ucr_anomaly", "data", "labeled", "ucr_power_demand.RData"))
  out <- list()
  for (i in seq_along(ucr_power_demand)) {
    idx <- 1:nrow(ucr_power_demand[[i]])
    df <- cbind(data.frame(idx), ucr_power_demand[[i]])
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
    names(out)[i] <- names(ucr_power_demand)[i]
  }
  out
}

ucr_power_demand <- build_ucr_power_demand()
save(ucr_power_demand, file = "data/ucr_power_demand.RData", compress = "xz")
