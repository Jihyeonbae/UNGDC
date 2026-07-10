
sovereignty <- read_csv("~/Desktop/UNGDC/output/embedding_glove/sovereignty_keywords_cosine.csv")

sovereignty <- sovereignty %>%
  select(ccode_iso, year, session, text, ccode, gwcode, 
         wdi_log_gdpcapcon2015, polity2, dd_regime, dd_democracy, v2x_polyarchy, ht_colonial, similarity_to_colonialism, similarity_to_territory,
         similarity_to_democracy, similarity_to_territorial, similarity_to_liberty, similarity_to_responsibility, similarity_to_independence,
         similarity_to_colonialism, similarity_to_intervention)

library(plm)

model1 <- plm(similarity_to_colonialism~v2x_polyarchy, 
              data = sovereignty
              )

summary(model1)


