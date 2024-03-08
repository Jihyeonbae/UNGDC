# TO DO: Fix paragraph parsing df [141, 5] has empty text.

library(jsonlite)
library(dplyr)

# Data available at: https://github.com/sjankin/UnitedNations/tree/master

## Reading in all text files into one dataframe.
# Define directory and sample files
directory <- "~/Desktop/UNGDC/data/raw/txt_files"
files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)
selected_files <- sample(files, 50)

# Initialize dataframe
df <- data.frame(
  ccode_iso = character(),
  session = numeric(),
  year = numeric(),
  paragraph_index = integer(),
  text = character(),
  stringsAsFactors = FALSE
)

# trial 2
for (file in selected_files) {
  print(paste("Processing file:", basename(file)))  # Debugging output

  # Read the content of the file with error handling
  content <- tryCatch(readLines(file, warn = FALSE), error = function(e) character(0))

    # Extract information from the file name
    file_info <- strsplit(basename(file), "_|\\.txt")[[1]]  # Adjusted splitting pattern

    # Initialize an empty vector to store paragraph texts
    paragraph_texts <- character()

    # Loop through each paragraph and extract text
    for (i in seq_along(content)) {
      paragraph_text <- gsub("^\\d+\\.\\s*", "", content[i])  # Remove leading numbers
      paragraph_texts <- c(paragraph_texts, paragraph_text)  # Append paragraph text
    }

    # Create a data frame with paragraph index and text
    paragraph_df <- data.frame(
      ccode_iso = file_info[1],
      session = as.numeric(file_info[2]),
      year = as.numeric(file_info[3]),
      paragraph_index = seq_along(paragraph_texts),
      text = paragraph_texts,
      stringsAsFactors = FALSE
    )

    # Append the data for this file to the main dataframe
    df <- bind_rows(df, paragraph_df)
  }

write.csv(df, "data/processed/paragraph.csv")



