data(oil_3w_Type_5)

# Standardize structure for package data
data <- oil_3w_Type_5$Type_5

# Series 1 and 2: derive changepoint events from 'class'
for (s in 1:min(2, length(data))) {
  labels <- data.frame(class = data[[s]]$class)
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
  base_cls <- 105
  for (i in cp_idx:nrow(labels)){
    if (!cp && !is.na(labels$class[i]) && labels$class[i] != base_cls){
      labels$cpd[i] <- 1
      cp_idx <- i
      cp  <- TRUE
    }
  }
  data[[s]]$event <- labels$cpd
  data[[s]]$class <- NULL
}

# Normalize names and event type for all series
for (i in 1:length(data)){
  data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  names(data[[i]]) <- tolower(names(data[[i]]))
}

# Build indexed list
oil_3w_Type_5 <- list()
for (i in 1:(length(data))) {
  idx <- 1:nrow(data[[i]])
  oil_3w_Type_5[[i]] <- cbind(data.frame(idx), data[[i]])
  names(oil_3w_Type_5)[i] <- names(data)[i]
}
for (i in 1:length(oil_3w_Type_5)){
  oil_3w_Type_5[[i]]$type <- ""
  oil_3w_Type_5[[i]]$type[oil_3w_Type_5[[i]]$event] <- "Change Point"
}

save(oil_3w_Type_5, file = "data/oil_3w_Type_5.RData", compress = "xz")

