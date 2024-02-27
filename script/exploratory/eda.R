#EDA

light<-readRDS("cleaned.RDS")

light_token<- light %>%
  unnest_tokens(word, text_processed, token = "ngrams", n = 5, to_lower = FALSE)

top_phrases<-light_token %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(20)

# Assuming top_phrases is a data frame with columns 'word' and 'n'
ggplot(top_phrases, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Top Phrases",
       x = "Phrases",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(quanteda)
corpus <- corpus(light, text_field="text_processed")
summarise(group_by(summary(corpus, n=10568), year), mean(Tokens), mean(Sentences))

tok <- tokens(corpus, what = "word",
              remove_punct=TRUE,
              remove_symbols = TRUE,
              verbose = TRUE,
              include_docvars = TRUE)

dfm <- dfm(tok, tolower=TRUE,
           stem=TRUE,
           verbose=TRUE)

dfm_weighted<-dfm_tfidf(dfm)


tdm <- TermDocumentMatrix(corpus,
                          control=list(weighting=weightTfIdf,
                                       removePunctuation=T,
                                       stemming=T))
inspect(tdm)

library(ggplot2)

top_terms <- topfeatures(dfm_weighted, n = 30)


# Convert to data frame for plotting
top_terms_df <- data.frame(term = names(top_terms), tfidf = unname(top_terms))

# Create bar plot
ggplot(top_terms_df, aes(x = reorder(term, tfidf), y = tfidf)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Top 10 Terms by TF-IDF",
       x = "Terms",
       y = "TF-IDF Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figs/top_terms_0111.png")

# LDA Model
tmod_lda<-textmodel_lda(dfm, k=10)
save(tmod_lda, file="lda_output.0111.RData")
terms(tmod_lda, 10)


