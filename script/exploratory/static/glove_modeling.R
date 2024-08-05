library(readr)

emb <- read_csv("~/Desktop/UNGDC/output/embedding_glove/liwc_meta_with_keyword_distances.csv")


library(plm)
model1 <- plm(distance_between_liberal_sovereign~dd_regime+  
               Clout + ethnicity + conflict + emo_anx + risk + focusfuture ,
              effect = "twoways", 
              index = c("ccode_iso", "year"),
              data = emb
                      )
summary(model1)

