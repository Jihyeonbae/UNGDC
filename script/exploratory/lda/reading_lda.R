
read_lda_models <- function(span_levels, output_dir = "output/lda/decade_0120") {
  lda_models <- list()

  for (i in span_levels) {
    lda_output_file <- file.path(output_dir, paste0("lda_model_", i, ".RDS"))

    if (file.exists(lda_output_file)) {
      lda_model <- readRDS(lda_output_file)
      lda_models[[i]] <- lda_model
      cat(sprintf("LDA model for %s successfully loaded.\n", i))
    } else {
      cat(sprintf("LDA model file for %s not found.\n", i))
    }
  }

  return(lda_models)
}


output_directory <- "output/lda/decade_0120"

lda_models <- read_lda_models(span_levels, output_directory)


topic_tables <- function(lda_models, span_levels) {
  topic_tables <- list()

  for (i in span_levels) {
    if (i %in% names(lda_models)) {
      lda_model <- lda_models[[i]]
      terms <- terms(lda_model, 10)
      topic_table <- data.frame(Topic = 1:ncol(terms), Terms = terms)
      topic_tables[[i]] <- topic_table
    } else {
      cat(sprintf("LDA model for %s not found.\n", i))
    }
  }

  all_topics <- do.call(rbind, topic_tables)
  return(all_topics)
}


topic_tables <- topic_tables(lda_models, span_levels)


topic_tables_0120<-knitr::kable((topic_tables))

