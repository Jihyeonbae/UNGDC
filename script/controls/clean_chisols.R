library(haven)
chisols <- read_dta("~/Downloads/CHISOLSstyr5_0/CHISOLSstyr5_0.dta")
View(chisols) 

chisols <- chisols %>%
  select(ccode, year, totalldrtrans, solschange)

write_rds(chisols, "~/Desktop/chisols.rds")
  