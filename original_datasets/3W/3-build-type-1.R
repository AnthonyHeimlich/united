data(oil_3w_Type_1)

# Prepare and standardize structure
data <- oil_3w_Type_1$Type_1

# Remove legacy 'class' labels and normalize names
for (i in 1:length(data)){
  data[[i]]$class <- NULL
  names(data[[i]])[8] <- "event"
  names(data[[i]]) <- tolower(names(data[[i]]))
}

# Ensure event is logical
for (i in 1:length(data)){
  data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
}

# Build indexed list for package data
oil_3w_Type_1 <- list()
for (i in 1:(length(data))) {
  idx <- 1:nrow(data[[i]])
  oil_3w_Type_1[[i]] <- cbind(data.frame(idx), data[[i]])
  names(oil_3w_Type_1)[i] <- names(data)[i]
}

for (i in 1:length(oil_3w_Type_1)){
  oil_3w_Type_1[[i]]$type <- ""
  oil_3w_Type_1[[i]]$type[oil_3w_Type_1[[i]]$event] <- "Change Point"
}

# Persist
save(oil_3w_Type_1, file = "data/oil_3w_Type_1.RData", compress = "xz")
