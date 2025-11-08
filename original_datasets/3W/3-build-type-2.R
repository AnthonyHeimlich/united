data(oil_3w_Type_2)

# Prepare and standardize structure
data <- oil_3w_Type_2$Type_2

# Series 1: derive changepoint events from 'class'
labels <- data.frame(class = data[[1]]$class)
labels$cpd <- 0
class_label <- max(na.omit(labels$class))

cp <- FALSE
for (i in 1:nrow(labels)){
  if (!cp && !is.na(labels$class[i]) && labels$class[i] != 0){
    message("Change point located at:", i)
    labels$cpd[i] <- 1
    cp_idx <- i
    cp <- TRUE
  }
}

cp_idx <- cp_idx + 1
cp <- FALSE
for (i in cp_idx:nrow(labels)){
  if (!cp && !is.na(labels$class[i]) && labels$class[i] != class_label){
    message("Change point located at:", i)
    labels$cpd[i] <- 1
    cp_idx <- i
    cp  <- TRUE
  }
}

data[[1]]$event <- labels$cpd
data[[1]]$class <- NULL

# Normalize names and event type for all series
for (i in 1:length(data)){
  data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  names(data[[i]]) <- tolower(names(data[[i]]))
}

# Build indexed list
oil_3w_Type_2 <- list()
for (i in 1:(length(data))) {
  idx <- 1:nrow(data[[i]])
  oil_3w_Type_2[[i]] <- cbind(data.frame(idx), data[[i]])
  names(oil_3w_Type_2)[i] <- names(data)[i]
}

for (i in 1:length(oil_3w_Type_2)){
  oil_3w_Type_2[[i]]$type <- ""
  oil_3w_Type_2[[i]]$type[oil_3w_Type_2[[i]]$event] <- "Change Point"
}

save(oil_3w_Type_2, file = "data/oil_3w_Type_2.RData", compress = "xz")

