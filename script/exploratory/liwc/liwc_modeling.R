#LIWC Modeling

library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(countrycode)
library(lmtest)
library(collapse)
library(plm)
liwc_df<-read.csv("data/interim/liwc_meta.csv")

liwc_df<-liwc_df%>%
  select(-X)%>%
  select(-session)%>%
  select(-ccode)%>%
  select(-gwcode)

liwc_df$year<-as.numeric(liwc_df$year)

liwc_df <- pdata.frame(liwc_df,
                             index=c("ccode_iso","year"),
                             drop.index=TRUE, row.names=TRUE)

liwc_df<-liwc_df%>%
  mutate(grepl("sovereignty", text, ignore.case = TRUE))

library(tidyverse)

liwc_df <- liwc_df %>%
  mutate(sovereignty = ifelse(str_detect(text, "sovereignty"), 1, 0),
         intervention = ifelse(str_detect(text, "intervention"), 1, 0),
         human_rights = ifelse(str_detect(text, "human rights"), 1, 0))



## Modeling

model0 <-plm( Authentic~ democracy + human_rights  ,
            data = liwc_df,
            index = c("ccode_iso", "year"),
            model = "within"
)
summary(model0, vcovBK(model0))

model1 <-plm(emo_neg ~ sovereignty * democracy + wdi_gdpcapcon2015 + cow_num_civil +
               cow_num_inter + kofgi_dr_eg + v2x_libdem,
             data = liwc_df,
             index = c("ccode_iso", "year"),
             model = "within"
)
summary(model1, vcovBK(model1))

#an increase in sovereignty is associated with a decrease in emo_neg when democracy increases

