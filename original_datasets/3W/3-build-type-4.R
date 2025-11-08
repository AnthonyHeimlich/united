load("original_datasets/3W/data/grouped/parquet/4/data_3w_tp4_sample.RData")

# Standardize structure for package data
data <- data_3w_tp4_sample

# Subset expected variables and class
var_exp <- c("P-JUS-CKGL", "P-MON-CKP", "P-PDG", "P-TPT", "QGL",
             "T-JUS-CKP", "T-PDG", "T-TPT", "class")
for (i in seq_along(data)) {
  data[[i]] <- data[[i]][, var_exp]
}

# Add change point labels derived from class transitions
for (j in seq_along(data)) {
  labels <- data.frame(class = data[[j]]$class)
  labels$cpd <- 0
  # first change from 0 to any non-zero
  found <- FALSE
  for (i in 1:nrow(labels)){
    if (!found && !is.na(labels$class[i]) && labels$class[i] != 0){
      labels$cpd[i] <- 1
      first_idx <- i
      found <- TRUE
    }
  }
  # second change to a different non-zero class value
  if (found) {
    base_cls <- labels$class[first_idx]
    found2 <- FALSE
    for (i in (first_idx + 1):nrow(labels)){
      if (!found2 && !is.na(labels$class[i]) && labels$class[i] != base_cls){
        labels$cpd[i] <- 1
        found2 <- TRUE
      }
    }
  }
  data[[j]]$event <- labels$cpd
}

# Remove old class column and normalize names
for (i in seq_along(data)){
  data[[i]]$class <- NULL
  names(data[[i]]) <- tolower(c("P_JUS_CKGL", "P_MON_CKP", "P_PDG", "P_TPT",
                                "QGL", "T_JUS_CKP", "T_PDG", "T_TPT", "event"))
  data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
}

# Build indexed list
oil_3w_Type_4 <- list()
for (i in seq_along(data)) {
  idx <- 1:nrow(data[[i]])
  oil_3w_Type_4[[i]] <- cbind(data.frame(idx), data[[i]])
  names(oil_3w_Type_4)[i] <- names(data)[i]
}
for (i in seq_along(oil_3w_Type_4)){
  oil_3w_Type_4[[i]]$type <- ""
  oil_3w_Type_4[[i]]$type[oil_3w_Type_4[[i]]$event] <- "Change Point"
}

save(oil_3w_Type_4, file = "data/oil_3w_Type_4.RData", compress = "xz")

