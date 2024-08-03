
# Load packages and custom functions --------------------------------------
library(readr)
library(conText)
library(dplyr)
library(tidytext)
library(quanteda)
library(word2vec)
library(widyr)
source(here::here("script/exploratory/static/00-func.R"))

meta <- read_csv("~/Desktop/UNGDC/data/processed/meta.csv")



# Load dictionary ---------------------------------------------------------

dict <- dictionary(file=here("dictionary.yml"), format="YAML")


# Load speeches, convert to common tibble form
ungdc <-  corpus(meta)

# Preprocess sentences ----------------------------------------------------

## Remove special characters etc.
sentence <- meta %>%
  mutate(
    text = enc2utf8(text),
    text = gsub("Mr.", "Mr", text),
    text = gsub("Ms.", "Ms", text),
    text = gsub("Mrs.", "Mrs", text),
    text = gsub("St.", "St", text),
    text = gsub("Dr.", "Dr", text),
    text = gsub("(\\(|\\[)(A|a)pplause\\S*(\\)|\\])\\S*"," ", text),
    text = gsub("§\\s+\"*", " ", text),
    text = gsub("…", " ", text),
    text = gsub("<\\w\\w>", " ", text),            # remove special characters encoded as e.g. <92>
    text = gsub("�"," ", text),                    # remove special characters encoded as �
    text = gsub("(\\[\\]|\\(\\))", " ", text),     # replace empty parentheses
    text = gsub("\n(-+|•)", " ", text),            # remove bullet points
    text = gsub("\\<\\d+\\>", " ", text),          # remove digits
    text = gsub("(:|;)(\\s+|\n+)", ". ", text),    # replace colon & semicolon and with period
    text = gsub(", \n*([A-HJ-Z])", ". \\1", text), # replace instances where comma was used as period
    text = gsub("\\.(\\s+|\\s*\n+)([a-z])", ". \\U\\2", text, perl = TRUE) # upper case after period
  ) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number())


# Export for analysis -----------------------------------------------------

save(dictionary, sentence,
     file = here("data/processed/sentence.Rdata"))
write_csv(sentence, here("data/processed/sentence.csv"))
write_lines(sentence$sentence, here("data/processed/sentence.txt"))
