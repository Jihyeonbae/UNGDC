# This Rscript generates a descriptive plot that shows the number of keywords appearing in the corpus.


## load csv file
library(tidyverse)
library(dplyr)
library(tidyr)
ungdc <- read.csv("data/cleaned.csv")
library(quanteda)

## make a corpus of the Texts

ungdc <- cbind(rownames(ungdc), ungdc)
rownames(ungdc) <- NULL
colnames(ungdc) <- c("id","country","session", "year", "text")

UNSpeechcodes_gen<-select(ungdc,"id","country", "year", "text") %>%
  dplyr::group_by(year)%>%
  summarise_all(funs(toString))

corpus_gen<-corpus(UNSpeechcodes_gen, docid_field="id",text_field="text")#creates a corpus

my_dict<-dictionary(list(Sovereignty=c("sovereignty","non-intervention","non-interference","territorial integrity"),
                         Human_Rights=c("human rights", "Responsibility to protect")))


dfm_keywords<-dfm(corpus_gen,remove=stopwords("english"),remove_punct=TRUE,dictionary=my_dict)

keywords<-convert(dfm_keywords,to="data.frame")
write.csv(keywords, "keywords.csv")


keywords$year<-sub(".*\\b(\\d{4})\\b.*", "\\1", keywords$doc_id)

data <- keywords%>%
  group_by(year) %>%
  summarize(
    Sovereignty = sum(Sovereignty),
    Human_Rights = sum(Human_Rights)
  )



png("plot.png", width = 6, height = 4, units = 'in', res = 1600)

ggplot(data, aes(x = as.numeric(year))) +
  geom_smooth(aes(y = Sovereignty, color = "Sovereignty"), size = 1.5) +
  geom_smooth(aes(y = Human_Rights, color = "Human_Rights"), size = 1.5) +
  theme_classic() +
  labs(x = "Year", y = "Count") +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_color_manual(values = c(Sovereignty = "red", Human_Rights = "blue"),
                     labels = c("Sovereignty", "Human_Rights")) +
  labs(color = "Category") +
  theme(legend.position = "top")

dev.off()



