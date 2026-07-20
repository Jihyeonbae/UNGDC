library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(countrycode)

ties <- read_excel("~/Downloads/TIESv4-1.xls", col_types = "text")

ties <- ties %>%
  mutate(across(c(startyear, endyear, targetstate, dsanctions), as.numeric))

ties <- ties%>%
  select(startyear, endyear, targetstate, threatenedtargetinterest,
         dsanctions) %>%  
  rename(cow = targetstate) 

ties <- ties%>%
  mutate(ccode_iso = countrycode(cow, "cown", "iso3c")) 

ties <- ties %>%
  mutate(ccode_iso = countrycode(cow, "cown", "iso3c",
                                 custom_match = c(`260` = "DEU", 
                                                  `265` = "DDR",   # pre-unification Germany
                                                  `315` = "CSK",                  # Czechoslovakia 
                                                  `345` = "YUG",                  # Yugoslavia 
                                                  `680` = "YMD",                  # South Yemen 
                                                  `817` = "VNM")))                # South Vietnam 


# i need to parse the multi-value interest string into dummies
interest_labels <- c("1"="general","2"="regime_leadership","3"="business",
                     "4"="political","5"="military","6"="other")

ties <- ties %>%
  mutate(interest_codes = str_split(as.character(threatenedtargetinterest), "\\s+"))

for (code in names(interest_labels)) {
  ties[[paste0("interest_", interest_labels[[code]])]] <-
    map_lgl(ties$interest_codes, ~ code %in% .x)
}

# expand to one row per active year
ties_long <- ties %>%
  mutate(endyear = if_else(is.na(endyear) | endyear < startyear, startyear, endyear)) %>%
  rowwise() %>%
  mutate(year = list(startyear:endyear)) %>%
  ungroup() %>%
  unnest(year)

ties_long<- ties_long %>%
  select(ccode_iso, year, dsanctions, interest_general, interest_regime_leadership, interest_business, interest_political, interest_military, interest_other )

# collapse to target-country-year
ties_cy <- ties_long %>%
  group_by(ccode_iso, year) %>%
  summarise(
    n_episodes        = n(),
    sanctioned        = as.integer(n_episodes > 0),
    sanctions_imposed = if (all(is.na(dsanctions))) NA_real_ else max(dsanctions, na.rm = TRUE),
    across(starts_with("interest_"), ~ as.integer(any(.x))),
    .groups = "drop"
  )
