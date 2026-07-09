# ========================================================================= #
# Project: Lexical Ambiguity in Political Rhetoric (BJPolS)
# - Script: Load required packages and custom functions
# - Author: Patrick Kraft (patrickwilli.kraft@uc3m.es)
# ========================================================================= #


# Required packages -------------------------------------------------------

## general purpose
library("tidyverse")
library("rsample")
library("widyr")
library("caret")
library("here")

## text analysis
library("tidytext")
library("readtext")
library("quanteda")
library("text2vec")
library("stringr")
library("conText")

## plots / tables
library("gridExtra")
library("xtable")


# Custom functions --------------------------------------------------------

## ggplot theme
theme_mft <- function(base_size = 8){
  theme_minimal(base_size = base_size) %+replace%
    theme(legend.background = element_rect(colour = 'gray80', 
                                           fill = 'white', 
                                           linetype='solid'),
          legend.key.height = unit(0, "cm"),
          legend.title = element_blank(),
          legend.position = c(1,0),
          legend.justification = c(1,0)
    )
}

## compute mft dictionary percentages for tibble of documents
mftPercent <- function(x){
  # ---------------------------------------------
  # INPUT
  # - x: tibble of documents
  # OUTPUT
  # - tibble of docvars and mft percentages
  # ---------------------------------------------
  x <- corpus(x)
  out <- tokens(x) %>% 
    tokens_lookup(dictionary = dict) %>%
    dfm() %>% 
    convert("data.frame") %>%
    transmute(sovereignty = sovereignty,
              trade = trade,
              rights = rights) %>%
    mutate_all(~(100 * . / ntoken(x)))
  
  bind_cols(docvars(x), out) %>%
    as_tibble()
}

## extract mft percentages from bootstrap samples
mftBoot <- function(x){
  # ---------------------------------------------
  # INPUT
  # - x: bootstrap splits
  # OUTPUT
  # - tibble of mft percentages
  # ---------------------------------------------
  analysis(x) %>%
    gather(-dd_regime, -year, -ccode_iso, -ccode, -statenme, -gwcode, -session) %>%
    group_by(dd_regime) 
}

## compute between vs. within party cosine similarities of sentence embeddings
cosineComparison <- function(x) {
  # ---------------------------------------------
  # INPUT
  # - x: sentence embeddings & party info tibble
  # OUTPUT
  # - original tibble including average difference in cosine similarities
  # ---------------------------------------------
  ## compute raw cosine matrix
  tmp <- x %>%
    select(starts_with("X")) %>%
    t() %>%
    lsa::cosine()
  
  ## remove diagonal elements (i.e., same sentence comparisons) 
  diag(tmp) <- NA
  
  ## add sentence ids
  colnames(tmp) <- rownames(tmp) <- x$sentence_id
  
  ## remove within-document comparisons
  for(i in unique(x$doc_id)){
    tmp[as.character(x$sentence_id[x$doc_id == i]), 
        as.character(x$sentence_id[x$doc_id == i])] <- NA
  }
  
  ## identify sentence IDs belonging to each party family
  id_repcon <- as.character(x$sentence_id[x$party_group == "Rep/Con"])
  id_demlab <- as.character(x$sentence_id[x$party_group == "Dem/Lab"])
  
  ## compute average difference within party - between party
  x %>% 
    mutate(cosine_diff = ifelse(x$party_group == "Dem/Lab", 1, -1) * 
             (apply(tmp[id_demlab,], 2, mean, na.rm = T) - 
                apply(tmp[id_repcon,], 2, mean, na.rm = T)
             )) %>% 
    group_by(doc_id) %>% 
    summarize(avg_cosine_diff = mean(cosine_diff, na.rm = T))
}

## Bootstrap CIs for cosine differences
cosineBoot <- function(x){
  # ---------------------------------------------
  # INPUT
  # - x: bootstrap splits
  # OUTPUT
  # - tibble of cosine diff. averages
  # ---------------------------------------------
  analysis(x) %>%
    gather(foundation, avg, -doc_id, -party, -year, -speaker, -type) %>%
    group_by(foundation) %>%
    summarize(avg = mean(avg, na.rm = T)) %>%
    mutate(foundation = factor(foundation, 
                               levels = rev(c("Care", "Fairness", "Loyalty", 
                                              "Authority", "Sanctity"))))
}

## Preprocess documents and run conText embedding regression for MFT dictionary terms
embedReg <- function(x, pattern, dict = mfd_list, type = NA, seed = NULL){
  # ---------------------------------------------
  # INPUT
  # - x: speeches to process (tibble)
  # - pattern: mft dimension (string)
  # - dict: mfd terms (character vector)
  # - type: speech type (string)
  # - seed: seed (numeric)
  # OUTPUT
  # - tibble of normed conText coefficients
  # ---------------------------------------------
  set.seed(seed)
  
  toks_dict <- corpus(x) %>% 
    # tokenize corpus removing unnecessary (i.e. semantically uninformative) elements
    tokens(remove_punct = T, 
           remove_symbols = T, 
           remove_numbers = T, 
           remove_separators = T) %>% 
    # clean out stopwords and words with 2 or fewer characters
    tokens_select(pattern = stopwords("en"), 
                  selection = "remove", 
                  min_nchar = 3) %>% 
    # replace dictionary terms
    tokens_replace(pattern = dict[[pattern]], 
                   replacement = rep(pattern, length(dict[[pattern]])), 
                   valuetype = "fixed")
  
  # only use features that appear at least 5 times in the corpus
  feats <- toks_dict %>% 
    dfm(tolower = T, 
        verbose = TRUE) %>% 
    dfm_trim(min_termfreq = 5) %>% 
    featnames()
  
  # leave the pads so that non-adjacent words will not become adjacent
  toks <- tokens_select(toks_dict, feats, padding = TRUE)
  
  # embedding regression
  res <- conText(formula = as.formula(paste0("'", pattern, "'", " ~ party")), 
                 data = toks, pre_trained = glove, 
                 transform = TRUE, transform_matrix = khodakA, 
                 bootstrap = TRUE, num_bootstraps = 10, 
                 stratify = TRUE, permute = TRUE, 
                 num_permutations = 100, window = 6, 
                 case_insensitive = TRUE,  verbose = FALSE)
  
  # return normed coefficients
  res@normed_cofficients %>% 
    as_tibble() %>% 
    mutate(type = type,
           foundation = pattern)
}
