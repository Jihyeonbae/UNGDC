library(tidyverse)
library(tidytext)
library(SnowballC)
library(plyr)
library(dplyr)
library(widyr)
library(furrr)
library(quanteda)

light<-readRDS("data/interim/light.RDS")

tidy_light <- light %>%
  select(text_processed, ) %>%
  unnest_tokens(word, text_processed) %>%
  add_count(word) %>%
  filter(n >= 50) %>%
  select(-n)

nested_words<-tidy_light %>%
  nest(words=c(word))

slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, 
    ~.x, 
    .after = window_size - 1, 
    .step = 1, 
    .complete = TRUE
  )
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))
  
  out %>%
    transpose() %>%
    pluck("result") %>%
    compact() %>%
    bind_rows()
}

plan(multisession)

tidy_pmi <- nested_words %>%
  mutate(words = future_map(words, slide_windows, 4L)) %>%
  unnest(words) %>%
  unite(window_id, document_id, window_id) %>%
  pairwise_pmi(word, window_id)

tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )


nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) / 
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        
        
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vectors %>%
  nearest_neighbors("sovereignty")
