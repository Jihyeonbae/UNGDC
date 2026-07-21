library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(countrycode)

library(haven)
gsdb <- read_dta("~/Downloads/gsdb_v4/GSDB_V4_dyadic.dta")

gsdb <- gsdb%>%
  select(target = sanctioned_state_iso3,
         sender = sanctioning_state_iso3,
         year, objective, arms, military, trade, financial, travel) %>%
  filter(!is.na(target))



gsdb_cy <- gsdb %>%
  group_by(target, year) %>%
  summarise(
    n_senders  = n_distinct(sender),                       # how many countries sanctioning that year
    sanctioned = 1L,                                         # every row here is a sanction by construction
    arms       = as.integer(any(arms == 1)),
    military   = as.integer(any(military == 1)),
    trade      = as.integer(any(trade == 1)),
    financial  = as.integer(any(financial == 1)),
    travel     = as.integer(any(travel == 1)),
    objectives = paste(unique(unlist(str_split(objective, ","))), collapse = ";"),
    .groups = "drop"
  ) %>%
  mutate(n_types = arms + military + trade + financial + travel)  # breadth of sanction types = simple intensity proxy

library(tidyr)

# one row per target-sender-year-objective
gsdb_long <- gsdb %>%
  mutate(objective = str_split(objective, ",")) %>%
  unnest(objective) %>%
  mutate(objective = str_trim(objective))

# one dummy column per distinct objective type actually observed in the data
gsdb_obj <- gsdb_long %>%
  distinct(target, year, objective) %>%
  mutate(present = 1L) %>%
  pivot_wider(names_from = objective, values_from = present,
              values_fill = 0, names_prefix = "obj_")

gsdb_cy <- gsdb_cy %>%
  left_join(gsdb_obj, by = c("target", "year"))



# full target x year grid so gaps become explicit rows
all_targets <- unique(gsdb_cy$target)
all_years   <- min(gsdb$year):max(gsdb$year)

gsdb_cy_full <- expand_grid(target = all_targets, year = all_years) %>%
  left_join(gsdb_cy, by = c("target", "year"))

gsdb_cols <- setdiff(names(gsdb_cy), c("target", "year", "objectives"))

gsdb_cy_full <- gsdb_cy_full %>%
  mutate(across(all_of(gsdb_cols), ~ replace_na(.x, 0)))

gsdb_cy_full <- gsdb_cy_full %>%
  filter(!is.na(target))

write_rds(gsdb_cy_full, "~/Desktop/sanction.rds")
