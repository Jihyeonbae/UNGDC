library(jsonlite)
library(dplyr)

# Data available at: https://github.com/sjankin/UnitedNations/tree/master

## Reading in all text files into one dataframe.

# directory containing the txt files
directory <- "data/raw/txt_files"

# list to store data frames for each file
files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)


df <- data.frame(
  ccode_iso = character(),
  session = numeric(),
  year = numeric(),
  text = character(),
  stringsAsFactors = FALSE
)

# Loop through each file
for (file in files) {
  # Read the content of the file
  content <- readLines(file, warn = FALSE)

  # Extract information from the file name
  file_info <- strsplit(basename(file), "_|\\.")[[1]]

  file_df <- data.frame(
    ccode_iso = file_info[1],
    session = as.numeric(file_info[2]),
    year = as.numeric(file_info[3]),
    text = paste(content, collapse = " "),
    stringsAsFactors = FALSE
  )

  df <- bind_rows(df, file_df)
}

saveRDS(df, file = "data/interim/UNGDC.rds")
