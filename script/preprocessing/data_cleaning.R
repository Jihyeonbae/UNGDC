data <- readRDS("~/Desktop/UNGDC/UNGDC.rds")
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)
ed
data<- data %>%
  separate(file_name, c('country', 'session', 'year'))

#convert all text to lower case
data$file_text <- tolower(data$file_text)

# Remove slashes
data$file_text <- gsub("\\d+\\.", "", data$file_text)

data$file_text <- gsub("\\\\", "", data$file_text)

data$file_text <- gsub("\n*", "", data$file_text)

data$file_text <- gsub("\t", "", data$file_text)

data$processed <- removeWords(data$file_text, stopwords("en"))

data$processed <- str_squish(data$processed)

ungdc.processed<-data%>%
  select(country, session, year, processed)

saveRDS(ungdc.processed, "cleaned_ungdc.rds")


#sovereignty
ungdc.processed <- ungdc.processed %>%
  mutate(sovereignty = if_else(str_detect(processed, regex("sovereignty", ignore_case = TRUE)),1,0))
