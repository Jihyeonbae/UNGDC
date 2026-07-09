library(readr)
library(dplyr)
library(plm)
library(broom)
library(modelsummary)
library(ggplot2)
library(stringr)

# this data is about "DISTANCE"=1-cosine similarity. 
emb <- read_csv("/Users/jihyeonbae/Desktop/UNGDC/output/embedding_static/glove/liwc_meta_with_keyword_distances_June2025.csv")

# Clean column names and apply log if appropriate
emb <- emb %>%
  rename_with(~ gsub("distance_", "", .x)) %>%
  rename_with(~ gsub("between_", "", .x))

regime_labels <- c(
  "Parliamentary democracy",
  "Mixed (semi-presidential) democracy",
  "Presidential democracy",
  "Civilian dictatorship",
  "Military dictatorship",
  "Royal dictatorship"
)
emb$dd_regime <- factor(emb$dd_regime, levels = 0:5, labels = regime_labels)

# Define distance-based DV variables
cosine_vars <- c(
  "sovereignty_territory_cosine",
  "sovereignty_colonial_cosine",
  "sovereignty_westphalia_cosine",
  "sovereignty_interfere_cosine",
  "sovereignty_intervene_cosine"
)

# Standardize the distance variables
emb <- emb %>%
  dplyr::mutate(across(all_of(cosine_vars), scale, .names = "{.col}_z"))


models_year <- list()
covariates <- c("risk", "WC", "WPS", "tone_pos","tone_neg", "negate", "focuspast", "mid_num_dispute", "ht_colonial")

emb$dd_regime <- as.factor(emb$dd_regime)

for (dep_var in paste0(cosine_vars, "_z")) {
  formula_str <- paste(dep_var, "~ dd_regime +", paste(covariates, collapse = " + "))
  models_year[[dep_var]] <- plm(
    as.formula(formula_str), 
    effect = "time", 
    model = "within", 
    index = c("ccode_iso", "year"),
    data = emb
  )
}


# Coefficient plot
plot_data <- lapply(names(models_year), function(name) {
  tidy(models_year[[name]]) %>%
    mutate(`Dependent Variable` = name)
}) %>% bind_rows()

model_results_year <- plot_data



dep_label_map <- c(
  "sovereignty_colonial_cosine_z"   = "Colonial",
  "sovereignty_territory_cosine_z"  = "Territory",
  "sovereignty_westphalia_cosine_z" = "Westphalia",
  "sovereignty_interfere_cosine_z"  = "Interfere",
  "sovereignty_intervene_cosine_z"  = "Intervene"
)


model_results_year <- model_results_year %>%
  dplyr::mutate(`Dependent Variable` = dplyr::recode(`Dependent Variable`, !!!dep_label_map))


model_results_year <- model_results_year %>%
  dplyr::mutate(term = str_remove(term, "dd_regime"))

model_results_year <- model_results_year %>%
  dplyr::mutate(term = case_when(
    term == "risk" ~ "Risk",
    term == "WC" ~ "Word Count",
    term == "WPS" ~ "Words Per Sentence",
    term == "tone_pos" ~ "Positive Tone",
    term == "tone_neg" ~ "Negative Tone",
    term == "negate" ~ "Negation",
    term == "Mixed (semi-presidential) democracy" ~ "Mixed Democracy",
    term == "Presidential democracy" ~ "Presidential Democracy",
    term == "Civilian dictatorship" ~ "Civilian Dictatorship",
    term == "Military dictatorship" ~ "Military Dictatorship",
    term == "Royal dictatorship" ~ "Royal Dictatorship",
    term == "focuspast" ~ "Focus Past",
    term == "mid_num_dispute" ~ "Number of Disputes",
    term == "ht_colonial" ~ "Colonial",
    TRUE ~ term
  ))


# Define different shapes for dependent variables
shape_mapping <- c(
  "Territory" = 17,
  "Colonial" = 18,
  "Westphalia" = 19,
  "Interfere" = 21,
  "Intervene" = 22
)

custom_order <- c(
  "Risk",
  "Number of Disputes",
  "Colonial",
  "Focus Past",
  "Positive Tone",
  "Negative Tone",
  "Negation",
  "Word Count",
  "Words Per Sentence",
  "Mixed Democracy",
  "Presidential Democracy",
  "Civilian Dictatorship",
  "Military Dictatorship",
  "Royal Dictatorship"
)

model_results_year <- model_results_year %>%
  mutate(term = factor(term, levels = custom_order))

plot_year <- model_results_year %>%
  ggplot(aes(x = term, y = estimate, shape = `Dependent Variable`)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = 0.3, position = position_dodge(width = 0.5)) +
  labs(
    title = "Coefficients for Semantic Distance",
    subtitle = "Estimated coefficients with 95% confidence intervals",
    x = "Independent Variables",
    y = "Coefficient Estimate",
    caption = "Note: Models include year fixed effects."
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic")
  ) +
  scale_shape_manual(values = shape_mapping)

plot_year

ggsave(
  filename ="~/Desktop/UNGDC/script/paper_chapter2/plot/sixfold_coef_plot.png",
  width = 10,
  height = 6)

modelsummary(
  models_year,
  output = "html",
  stars = TRUE,
  statistic = c("std.error", "p.value"),
  coef_omit = "(Intercept)",  # Omitting intercept for clarity
  coef_map = list(
    "dd_regimeMixed (semi-presidential) democracy" = "Mixed Democracy",
    "dd_regimePresidential democracy" = "Presidential Democracy",
    "dd_regimeCivilian dictatorship" = "Civilian Dictatorship",
    "dd_regimeMilitary dictatorship" = "Military Dictatorship",
    "dd_regimeRoyal dictatorship" = "Royal Dictatorship",
    "risk" = "Perceived Risk",
    "WC" = "Word Count",
    "WPS" = "Words per Sentence",
    "tone_neg" = "Negative Tone",
    "tone_pos" = "Positive Tone", 
    "negate" = "Negation",
    "focuspast" = "Focus on Past",
    "mid_num_dispute" = "Disputes",
    "ht_colonial" = "Colonial"
  ),
  notes = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05",
  title = "Year Fixed Effects Models: Impact of Regime Types on Sovereignty Rhetoric"
)




##############################################
###################additional tests for h3
##############################################
emb <- emb %>%
  mutate(sovereignty_mentions = str_count(text, "\\bsovereignty\\b")) %>%
  mutate(sovereignty_freq = (sovereignty_mentions/WC)*100)


sovereignty_tallies <- emb %>%
  dplyr::group_by(ccode_iso, year, as.factor(dd_regime)) %>%
  dplyr::summarise(
    total_mentions = mean(sovereignty_freq, na.rm = FALSE),
    avg_risk = mean(risk, na.rm = FALSE),
    avg_mid_num_dispute = mean(mid_num_dispute, na.rm = FALSE),
    colonial = mean(ht_colonial, na.rm = FALSE),
    focuspast = mean(focuspast, na.rm = FALSE),
    WC = mean(WC, na.rm = FALSE),
    WPS = mean(WPS, na.rm = FALSE),
    tone_neg = mean(tone_neg, na.rm = FALSE),
    tone_pos = mean(tone_pos, na.rm = FALSE),
    negation = mean(negate, na.rm = FALSE)
  ) %>%
  ungroup() 


# Step 4: Convert to Panel Data Frame
sovereignty_tallies <- pdata.frame(sovereignty_tallies, index = c("ccode_iso", "year"))


# Step 5: Fit a Regression Model with Year Fixed Effects
fe_model <- plm(
  total_mentions ~ avg_risk * as.factor.dd_regime. +
    avg_mid_num_dispute + colonial + WC + WPS + tone_neg + tone_pos + negation,
  data = sovereignty_tallies,
  model = "within",                    
  effect = "time"                
)


summary(fe_model)


modelsummary(
  fe_model, 
  output = "html", 
  stars = TRUE
)

coefficients_data <- tidy(fe_model) %>%
  filter(term != "(Intercept)")  

# Calculate total effects by regime type
total_effects_country_year <- coefficients_data %>%
  filter(grepl("avg_risk", term)) %>%
  mutate(
    regime_type = case_when(
      term == "avg_risk" ~ "Parliamentary Democracy",
      term == "avg_risk:as.factor.dd_regime.Mixed (semi-presidential) democracy" ~ "Mixed Democracy",
      term == "avg_risk:as.factor.dd_regime.Presidential democracy" ~ "Presidential Democracy",
      term == "avg_risk:as.factor.dd_regime.Civilian dictatorship" ~ "Civilian Dictatorship",
      term == "avg_risk:as.factor.dd_regime.Military dictatorship" ~ "Military Dictatorship",
      term == "avg_risk:as.factor.dd_regime.Royal dictatorship" ~ "Royal Dictatorship"
    ),
    total_effect = case_when(
      term == "avg_risk" ~ estimate,
      TRUE ~ estimate + coefficients_data$estimate[coefficients_data$term == "avg_risk"]
    ),
    lower_ci = total_effect - 1.96 * std.error,
    upper_ci = total_effect + 1.96 * std.error
  )

# Reorder the regime_type factor based on the desired order
total_effects_country_year$regime_type <- factor(
  total_effects_country_year$regime_type,
  levels = c(
    "Parliamentary Democracy",
    "Mixed Democracy",
    "Presidential Democracy",
    "Civilian Dictatorship",
    "Military Dictatorship",
    "Royal Dictatorship"
  )
)

# Replot with the corrected order
interaction_plot <- ggplot(total_effects_country_year, aes(x = regime_type, y = total_effect)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(
    title = "Total Effect of Risk on Sovereignty Mentions by Regime Type",
    x = "Regime Type",
    y = "Total Effect",
    caption = "Error bars indicate 95% confidence intervals."
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

interaction_plot

ggsave( "~/Desktop/UNGDC/script/exploratory/static/plot/interaction_regime_risk.png", interaction_plot,
        width = 10,
        height = 6)


