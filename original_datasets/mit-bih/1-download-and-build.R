url <- Sys.getenv("MIT_BIH_DATA_URL", "https://canopus.eic.cefet-rj.br/data/MIT-BIH/MIT-BIH-Dataset.RData")

# Create a local temporary file for the download
destfile <- tempfile(fileext = ".RData")

# Download dataset archive and load into memory
download.file(url, destfile, mode = "wb")
load(destfile)

# Build channel-specific datasets (MLII, V1, V2, V5)
levels <- c('\'', '!', '"', '(', ')', '*', '/', '?', '@', '[', ']', '^', '`',
            '|', '~', '+', '=', 'A', 'a', 'B', 'D', 'e', 'E', 'F', 'f', 'J',
            'j', 'L', 'N', 'n', 'p', 'Q', 'R', 'r', 'S', 's', 't', 'T', 'u',
            'V', 'x')

# MLII
j <- 1
mit_bih_MLII <- list()
for (i in 1:length(dataset)) {
  if (!is.null(dataset[[i]]$MLII)) {
    data <- dataset[[i]]$MLII$signal
    data <- data[, 1:4]
    colnames(data) <- c("idx", "value", "event", "seq")
    data$seqlen <- 50
    data$seq <- factor(data$seq, levels = levels)
    data$event <- FALSE
    data$event[!is.na(data$seq)] <- TRUE
    mit_bih_MLII[[j]] <- data
    names(mit_bih_MLII)[j] <- sprintf("%s_MLII", names(dataset[i]))
    j <- j + 1
  }
  if (j > 5) break
}
save(mit_bih_MLII, file = "data/mit_bih_MLII.RData", compress = "xz")

# V1
j <- 1
mit_bih_V1 <- list()
for (i in 1:length(dataset)) {
  if (!is.null(dataset[[i]]$V1)) {
    data <- dataset[[i]]$V1$signal
    data <- data[, 1:4]
    colnames(data) <- c("idx", "value", "event", "seq")
    data$seqlen <- 50
    data$seq <- factor(data$seq, levels = levels)
    data$event <- FALSE
    data$event[!is.na(data$seq)] <- TRUE
    mit_bih_V1[[j]] <- data
    names(mit_bih_V1)[j] <- sprintf("%s_V1", names(dataset[i]))
    j <- j + 1
  }
  if (j > 5) break
}
save(mit_bih_V1, file = "data/mit_bih_V1.RData", compress = "xz")

# V2
j <- 1
mit_bih_V2 <- list()
for (i in 1:length(dataset)) {
  if (!is.null(dataset[[i]]$V2)) {
    data <- dataset[[i]]$V2$signal
    data <- data[, 1:4]
    colnames(data) <- c("idx", "value", "event", "seq")
    data$seqlen <- 50
    data$seq <- factor(data$seq, levels = levels)
    data$event <- FALSE
    data$event[!is.na(data$seq)] <- TRUE
    mit_bih_V2[[j]] <- data
    names(mit_bih_V2)[j] <- sprintf("%s_V2", names(dataset[i]))
    j <- j + 1
  }
  if (j > 5) break
}
save(mit_bih_V2, file = "data/mit_bih_V2.RData", compress = "xz")

# V5
j <- 1
mit_bih_V5 <- list()
for (i in 1:length(dataset)) {
  if (!is.null(dataset[[i]]$V5)) {
    data <- dataset[[i]]$V5$signal
    data <- data[, 1:4]
    colnames(data) <- c("idx", "value", "event", "seq")
    data$seqlen <- 50
    data$seq <- factor(data$seq, levels = levels)
    data$event <- FALSE
    data$event[!is.na(data$seq)] <- TRUE
    mit_bih_V5[[j]] <- data
    names(mit_bih_V5)[j] <- sprintf("%s_V5", names(dataset[i]))
    j <- j + 1
  }
  if (j > 5) break
}
save(mit_bih_V5, file = "data/mit_bih_V5.RData", compress = "xz")

