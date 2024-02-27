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

# Loop through each file
for (file in selected_files) {
  # Read file content
  content <- readLines(file, warn = FALSE)

  # Initialize variables for paragraph text and index
  paragraph_texts <- character()
  paragraph_index <- 1

  # Loop through each line of the file text
  for (line in content) {
    # Check if the line is empty
    if (line == "") {
      # If the paragraph text is not empty, store it in the dataframe
      if (length(paragraph_texts) > 0) {
        # Remove \f character and extra spaces from the paragraph texts
        cleaned_texts <- gsub("\\s+", " ", gsub("\\f", "", paragraph_texts))

        # Create a dataframe row for the paragraph
        df_row <- data.frame(
          ccode_iso = file_info[1],  # Replace with actual values
          session = as.numeric(file_info[2]),  # Replace with actual values
          year = as.numeric(file_info[3]),  # Replace with actual values
          paragraph_index = paragraph_index,
          text = paste(cleaned_texts, collapse = " "),  # Concatenate cleaned paragraph texts
          stringsAsFactors = FALSE
        )
        # Append the dataframe row to the main dataframe
        df <- rbind(df, df_row)
        # Reset paragraph texts
        paragraph_texts <- character()
        # Increment paragraph index
        paragraph_index <- paragraph_index + 1
      }
    } else {
      # Append the line to the current paragraph text
      paragraph_texts <- c(paragraph_texts, line)
    }
  }
}

# Write the dataframe to a CSV file
write.csv(df, "paragraphs.csv", row.names = FALSE)





##############################################################################


# trial 2
for (file in files) {
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



