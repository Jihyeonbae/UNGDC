library(jsonlite)
library(dplyr)

# Data available at: https://github.com/sjankin/UnitedNations/tree/master
# Create a list to store dataframes
df_list <- list()


# TXT files into JSON files
# Loop through each file
for (file in files) {
  # Read the content of the file
  content <- readLines(file, warn = FALSE)

  # Extract information from the file name
  file_info <- strsplit(basename(file), "_|\\.")[[1]]

  # Create a dataframe for the current file
  file_df <- data.frame(
    CountryCode = file_info[1],
    SessionNumber = as.numeric(file_info[2]),
    Year = as.numeric(file_info[3]),
    Text = paste(content, collapse = " "),
    stringsAsFactors = FALSE
  )

  # Append the dataframe to the list
  df_list[[file]] <- file_df
}

# Save each dataframe as a JSON file
for (i in seq_along(df_list)) {
  json_file <- sub("\\.txt$", ".json", files[i])
  jsonlite::write_json(df_list[[i]], json_file)
}

# Print the list of JSON files
print(paste("JSON files saved in directory:", dirname(files)))

# bash commands for file organization
#mkdir -p "/txt_files"
#mv *.txt txt_files/

