### Meta Data

# This merges country-level meta data with the UNGDC's speaker country. 

library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(countrycode)

cleaned<-read_csv("~/Desktop/UNGDC/data/processed/cleaned.csv") %>%
  select(-`...1`)

main <- readRDS("~/Downloads/main_updated_May_2026 (1).rds")
library(haven)
monadic <- read_dta("~/Desktop/Replication Files/monadic.dta")

monadic <- monadic %>%
  select(year, ccode, locationconflict, 
         fariss_hr = theta_mean, 
         hro_counts = hrcountfilled, 
         lnhrcountfilled, 
         shaming = NEWHROshamingnowikiDULAP)



country_controls<- main%>%
  left_join(monadic, by=c("ccode", "year"))


meta <- cleaned %>%
  mutate(ccode_iso = as.character(ccode_iso)) %>%
  left_join(country_controls %>% mutate(ccode = as.character(ccode)),
            by = c("year", "ccode_iso" = "ccode"))

write_csv(meta, "data/interim/UNGDC_meta.csv")



