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

light_interval <- light %>%
  dplyr::mutate(span = as.factor(cut(year,
                                     breaks = c(seq(from = 1945, to = 2022, by = 10), 2022)))) %>%
  dplyr::arrange(year)




#Topic Modeling


lda_generator <- function(corpus, span_levels, num_topics = 10) {
  output_dir <- "output/lda/decade_0203"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  for (i in span_levels) {
    subset <- corpus[corpus$span == i, ]
    subset_dfm <- dfm(subset$text,
                      remove_numbers = TRUE,
                      split_hyphens = TRUE,
                      stem = TRUE,
                      remove_punct = TRUE,
                      tolower = TRUE,
                      remove = stopwords("english"))
    dfm_idf <- dfm_tfidf(subset_dfm,
                         scheme_tf = "count",
                         scheme_df = "inverse")

    timing <- system.time({
      dfm_df <- convert(dfm_idf, to = "data.frame")
      dfm_output_file <- file.path(output_dir, paste0("dfm_", i, ".RDS"))
      saveRDS(dfm_df, dfm_output_file)

      tmod_lda <- textmodel_lda(subset_dfm, k = num_topics)
      lda_output_file <- file.path(output_dir, paste0("lda_model_", i, ".RDS"))
      saveRDS(tmod_lda, lda_output_file)
    })

    cat(sprintf("Time taken for %s: %s seconds\n", i, timing[3]))
  }
}


span_levels <- levels(light_interval$span)
lda_generator(light_interval, span_levels)


