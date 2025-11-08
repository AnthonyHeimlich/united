#########################################################
## UCI 3W - Build parquet-based groups (Types 3, 4, 9)
#########################################################

series_description <- c("Type_3",
                        "Type_4",
                        "Type_9")

series_path <- c("3","4","9")


# Create the complete dataset organized as a list

create_dataset <- function(group = c("3","4","9"), nm = NA) {
  require(stringr)
  require(readr)
  require(arrow)

  series_path <- group
  ungp_path <- "data/ungrouped/"

  #Dataset with groups organized into a list
  dataset <- list()

  #Iteration on series groups
  for (k in 1:length(series_path)) {
    # Group - Series
    #files_sr <- list.files(path = series_path[k], pattern = "*.RData")
    files_sr <- list.files(path = paste(ungp_path, series_path[k], sep = ""), pattern = "*.parquet")

    #Dataset organized with series into a list
    group <- list()

    #Iteration in the series of each group
    i <- 1
    for (i in 1:length(files_sr)) {
      series_file <- paste(ungp_path, series_path[k], "/", files_sr[i], sep = "")

      # Read parquet series
      series <- read_parquet(series_file)
      group[[i]] <- series

    }
    #Each dataframe is given the name of the series in the list
    names(group) <- str_sub(files_sr, end = -9)
    dataset[[k]] <- group

  }
  #Each group is named after the original documentation
  names(dataset) <- nm

  # Return complete dataset
  return(dataset)
}


# Persist grouped artifacts for each parquet-based type
for (j in seq_along(series_path)) {
  out_file <- paste("data/grouped/oil_3w_", series_description[j], ".RData", sep = "")
  grp <- create_dataset(group = series_path[j], nm = series_description[j])
  save(grp, file = out_file, compress = TRUE)
}
