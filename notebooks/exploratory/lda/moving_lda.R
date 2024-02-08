# Topic Modeling

library(plyr)
library(dplyr)
library(tm)
library(ggplot2)
library(quanteda)
library(readr)
library(seededlda)
library(slam)
library(jsonlite)


light<-readRDS("data/processed/cleaned.RDS")



#Set up the parameters

duration <- 1945:2022
interval <- 10


library(quanteda)
library(topicmodels)

lda_generator <- function(corpus, start_year, end_year, window_size = 10, overlap = 5, num_topics = 10) {
  output_dir <- "output/lda/moving_0207"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  while (end_year <= 2022) {
    # Subset the corpus for the current time window
    subset <- corpus[corpus$year >= start_year & corpus$year <= end_year, ]

    # Preprocess the subset
    subset_dfm <- dfm(subset$text,
                      remove_numbers = TRUE,
                      split_hyphens = TRUE,
                      stem = TRUE,
                      remove_punct = TRUE,
                      tolower = TRUE,
                      remove = stopwords("english"))

    # Convert to TF-IDF matrix
    dfm_idf <- dfm_tfidf(subset_dfm,
                         scheme_tf = "count",
                         scheme_df = "inverse")

    # Save TF-IDF matrix
    dfm_output_file <- file.path(output_dir, paste0("dfm_", start_year, "-", end_year, ".RDS"))
    saveRDS(convert(dfm_idf, to = "data.frame"), dfm_output_file)

    # Train LDA model
    tmod_lda <- textmodel_lda(subset_dfm, k = num_topics)

    # Save LDA model
    lda_output_file <- file.path(output_dir, paste0("lda_model_", start_year, "-", end_year, ".RDS"))
    saveRDS(tmod_lda, lda_output_file)

    # Move the window with overlap
    start_year <- start_year + overlap
    end_year <- end_year + overlap
  }
}


# Example usage
lda_generator(light, 1945, 1955)  # Adjust the start and end year according to your needs

dfm_data <- readRDS("output/lda/moving_0207/dfm_1945-2022.RDS")
lda_model <- readRDS("output/lda/moving_0207/lda_model_1945-2022.RDS")
