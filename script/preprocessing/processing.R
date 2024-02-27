library(tidytext)
library(stringr)
library(tm)
library(plyr)
library(dplyr)

# Reading in data

raw<- readRDS("data/interim/UNGDC.rds")
cleaned<-raw

#removing pattern: white spaces(multiple spaces and tab) and digits followed by a dot
cleaned$text <- gsub("\\s+\\d+\\.", "", cleaned$text)

cleaned$text <- gsub("\\d+\\.\\s+", "", cleaned$text)

# first sentence
cleaned$text <- gsub("^\\d+\\.", "", cleaned$text, perl = TRUE)

#remove tabs
cleaned$text<-gsub("\\t", "", cleaned$text)

# generate word_count variable
cleaned$word_count <- str_count(cleaned$text, "\\S+")

saveRDS(cleaned, "data/interim/cleaned.RDS")
write.csv(cleaned, "data/interim/cleaned.csv")

# remove stop words
library(tm)
light<-cleaned
light$text_processed<-removeWords(light$text, stopwords("en"))
light$text_processed<-str_squish(light$text_processed)



library(dplyr)

light<-light%>% select(-text)

saveRDS(light, "data/interim/light.RDS")
