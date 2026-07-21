### Meta Data

# This merges country-level meta data with the UNGDC's speaker country. 

library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(countrycode)

# These can be safely grouped by ISO3C. 
cleaned<-read_csv("~/Desktop/UNGDC/data/processed/cleaned.csv") %>%
  select(-`...1`)

sanctions <- read_rds( "~/Desktop/sanction.rds") %>%
  rename(ccode_iso = target )


cleaned <- cleaned %>%
  left_join(sanctions, by=c("ccode_iso", "year"))

# and most of the other country-cy control variables are in COW country code format. 
main <- readRDS("~/Downloads/main_updated_May_2026 (1).rds")
library(haven)
monadic <- read_dta("~/Desktop/Replication Files/monadic.dta")

monadic <- monadic %>%
  select(year, ccode, locationconflict, 
         fariss_hr = theta_mean, 
         hro_counts = hrcountfilled, 
         lnhrcountfilled, 
         shaming = NEWHROshamingnowikiDULAP)


chisols <- read_rds("~/Desktop/chisols.rds")

country_controls<- main%>%
  left_join(monadic, by=c("ccode", "year"))%>%
  left_join(chisols, by=c("ccode", "year"))


# the tricky part is merging country code (COW) with ISO 3 character code. 
# adding countrycode to ISO3C. 

cleaned <- cleaned %>%
  mutate(ccode = countrycode(ccode_iso, "iso3c", "cown"))

cleaned <- cleaned %>%
  mutate(ccode = case_when(
    ccode_iso == "CSK" ~ 315, # this is Czechoslovakia
    ccode_iso == "DDR" ~ 265,
    ccode_iso == "SRB"  ~ 345,
    ccode_iso == "YUG" ~ 345,
    ccode_iso == "YMD" ~ 680,
    ccode_iso == "CZK" ~ 316, # this is Czech Republic. 
    ccode_iso == "EU"  ~ NA_real_,
    ccode_iso == "PSE" ~ NA_real_,   # unresolved -- check COW codebook before assuming
    TRUE ~ ccode
  ))


meta <- cleaned %>%
  left_join(country_controls, by = c("year", "ccode", "ccode_iso")) 

write_rds(meta, "~/Desktop/UNGDC/data/interim/UNGDC_meta.rds")



