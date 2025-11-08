load("data/oil_3w_Type_8.RData")

# Standardize structure for package data
data <- oil_3w_Type_8$Type_8

# Helper to derive change points for a series with 'class'
derive_cpd <- function(cls, base_cls = 108) {
  labels <- data.frame(class = cls)
  labels$cpd <- 0
  cp <- FALSE
  for (i in 1:nrow(labels)){
    if (!cp && !is.na(labels$class[i]) && labels$class[i] != 0){
      labels$cpd[i] <- 1
      cp_idx <- i
      cp <- TRUE
    }
  }
  cp_idx <- cp_idx + 1
  cp <- FALSE
  for (i in cp_idx:nrow(labels)){
    if (!cp && !is.na(labels$class[i]) && labels$class[i] != base_cls){
      labels$cpd[i] <- 1
      cp_idx <- i
      cp  <- TRUE
    }
  }
  labels$cpd
}

# Apply to first three series as per original script
for (s in 1:min(3, length(data))) {
  data[[s]]$event <- derive_cpd(data[[s]]$class)
  data[[s]]$class <- NULL
}

# Normalize names and event type for all series
for (i in 1:length(data)){
  data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  names(data[[i]]) <- tolower(names(data[[i]]))
}

# Build indexed list
oil_3w_Type_8 <- list()
for (i in 1:(length(data))) {
  idx <- 1:nrow(data[[i]])
  oil_3w_Type_8[[i]] <- cbind(data.frame(idx), data[[i]])
  names(oil_3w_Type_8)[i] <- names(data)[i]
}
for (i in 1:length(oil_3w_Type_8)){
  oil_3w_Type_8[[i]]$type <- ""
  oil_3w_Type_8[[i]]$type[oil_3w_Type_8[[i]]$event] <- "Change Point"
}

save(oil_3w_Type_8, file = "data/oil_3w_Type_8.RData", compress = "xz")

