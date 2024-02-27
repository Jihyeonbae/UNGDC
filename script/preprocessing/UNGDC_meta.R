### Meta Data

# This merges country-level meta data with the UNGDC's speaker country. 

library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(countrycode)

cleaned<-readRDS("data/processed/cleaned.RDS")


country <- read_csv("~/Desktop/UNGDC/data/interim/controls.csv")
country<-country%>%select(-...1)


country_controls <- country %>%
  mutate(ccode_iso= countrycode(ccode, origin = 'cown',
                                destination = 'iso3c'))


meta<-cleaned%>%
  left_join(country_controls, by=c("year","ccode_iso"))

write_csv(meta, "data/interim/UNGDC_meta.csv")



