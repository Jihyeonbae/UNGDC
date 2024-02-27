# Time Series Topic Modeling

light<-readRDS("light.RDS")
library(plyr)
library(dplyr)
library(tm)
library(ggplot2)
library(quanteda)
library(readr)
library(seededlda)
library(slam)
library(jsonlite)

#Set up the parameters

duration <- 1945:2022
interval <- 10

light_interval <- light %>%
  dplyr::mutate(span = as.factor(cut(year,
                                     breaks = c(seq(from = 1945, to = 2022, by = 10), 2022)))) %>%
  dplyr::arrange(year)


# Function for generating tf_idf and plots.
lapply(levels(light_interval$span), function(i) {
  subset_i <- light_interval %>% dplyr::filter(span %in% i)
  corpus_subset <- Corpus(VectorSource(subset_i$text_processed))
  tdm <- TermDocumentMatrix(corpus_subset,
                            control = list(weighting = weightTfIdf,
                                           removePunctuation = TRUE,
                                           stemming = TRUE,
                                           removeNumbers = TRUE))
  top_terms <- slam::row_sums(as.matrix(tdm))
  top_terms_df <- data.frame(term = names(top_terms), tfidf = top_terms) %>%
    top_n(20, tfidf)
  figure_i <- ggplot(top_terms_df, aes(x = reorder(term, tfidf), y = tfidf)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme_minimal() +
    labs(title = "Top 20 Terms by TF-IDF",
         x = "Terms",
         y = "TF-IDF Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  output_file <- file.path("figs/", paste0("plot_", i, ".png"))
  ggsave(output_file, figure_i, width = 8, height = 5, units = "in")
})


#Topic Modeling

library(jsonlite)


lda_generator <- function(corpus, span_levels, num_topics = 10) {
  output_dir <- "output/lda/decade_0114"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  for (i in span_levels) {
    subset <- corpus[corpus$span == i, ]
    subset_dfm <- dfm(subset$text_processed,
                      remove_punct = TRUE,
                      remove_numbers = TRUE,
                      stem = TRUE,
                      tolower = TRUE,
                      remove = stopwords("english"))

    timing <- system.time({
      dfm_df <- convert(subset_dfm, to = "data.frame")
      dfm_output_file <- file.path(output_dir, paste0("dfm_", i, ".json"))
      write_json(dfm_df, dfm_output_file)

      tmod_lda <- textmodel_lda(subset_dfm, k = num_topics)
      topics_df <- as.data.frame(topics(tmod_lda))  # Extract topics
      lda_output_file <- file.path(output_dir, paste0("lda_model_", i, ".json"))
      toJSON(topics_df, pretty = TRUE, auto_unbox = TRUE) %>% writeLines(lda_output_file)
    })

    cat(sprintf("Time taken for %s: %s seconds\n", i, timing[3]))
  }
}


span_levels <- levels(light_interval$span)
lda_generator(light_interval, span_levels)

lda_model_df <- fromJSON("/output/lda/decade_0114/lda_model_(1945, 1955].json")

