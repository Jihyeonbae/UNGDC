library(readr)
library(dplyr)
library(plm)
library(broom)
library(modelsummary)
library(ggplot2)
library(stringr)
library(extrafont)
loadfonts(quiet = TRUE)

# this data is about "DISTANCE"=1-cosine similarity.
# Variant of fourfold_regression_Sep2025.R using the newly regenerated distance
# measures: 300-dimensional GloVe, lemmatized text, and the corrected pair-
# averaged cosine distance formula (average of distance-to-keyword1 and
# distance-to-keyword2, per footnote 10 of the paper).
emb <- read_csv("~/Desktop/UNGDC/output/embedding_glove/liwc_meta_with_keyword_distances_300d_Jul2026.csv")

# Our columns are named distance_sovereignty_<keyword2> (no "_cosine" suffix,
# since this pipeline only ever computes cosine distance). Rename to match
# what the rest of this script expects, so the downstream logic is unchanged.
emb <- emb %>%
  rename(
    distance_sovereignty_colonial_cosine   = distance_sovereignty_colonial,
    distance_sovereignty_territory_cosine  = distance_sovereignty_territory,
    distance_sovereignty_westphalia_cosine = distance_sovereignty_westphalia,
    distance_sovereignty_intervene_cosine  = distance_sovereignty_intervene,
    distance_sovereignty_interfere_cosine  = distance_sovereignty_interfere
  )

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

emb$dd_regime_pooled <- factor(dplyr::case_when(
  emb$dd_regime %in% c("Parliamentary democracy",
                       "Mixed (semi-presidential) democracy",
                       "Presidential democracy") ~ "Democracy",
  emb$dd_regime == "Civilian dictatorship" ~ "Civilian dictatorship",
  emb$dd_regime == "Military dictatorship" ~ "Military dictatorship",
  emb$dd_regime == "Royal dictatorship"    ~ "Royal dictatorship"
), levels = c("Democracy",
              "Civilian dictatorship",
              "Military dictatorship",
              "Royal dictatorship"))



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

emb$dd_regime_pooled <- as.factor(emb$dd_regime_pooled)

for (dep_var in paste0(cosine_vars, "_z")) {
  formula_str <- paste(dep_var, "~ dd_regime_pooled +", paste(covariates, collapse = " + "))
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
  dplyr::mutate(term = str_remove(term, "dd_regime_pooled"))

model_results_year <- model_results_year %>%
  dplyr::mutate(term = case_when(
    term == "risk" ~ "Risk",
    term == "WC" ~ "Word Count",
    term == "WPS" ~ "Words Per Sentence",
    term == "tone_pos" ~ "Positive Tone",
    term == "tone_neg" ~ "Negative Tone",
    term == "negate" ~ "Negation",
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
  "Civilian Dictatorship",
  "Military Dictatorship",
  "Royal Dictatorship"
)

model_results_year <- model_results_year %>%
  mutate(term = factor(term, levels = custom_order))

cat("\n=== Fourfold regime coefficients (300d) ===\n")
print(model_results_year %>% select(`Dependent Variable`, term, estimate, std.error, p.value))

plot_year <- model_results_year %>%
  ggplot(aes(x = term, y = estimate, shape = `Dependent Variable`)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = 0.3, position = position_dodge(width = 0.5)) +
  labs(
    title = "Coefficients for Semantic Distance (300d)",
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
  filename ="~/Desktop/UNGDC/script/paper_chapter2/plot/fourfold_coef_plot_300d_Jul2026.png",
  width = 10,
  height = 6)

modelsummary(
  models_year,
  output = "html",
  stars = TRUE,
  statistic = c("std.error", "p.value"),
  coef_omit = "(Intercept)",  # Omitting intercept for clarity
  coef_map = list(
    "dd_regime_pooledCivilian dictatorship" = "Civilian Dictatorship",
    "dd_regime_pooledMilitary dictatorship" = "Military Dictatorship",
    "dd_regime_pooledRoyal dictatorship"    = "Royal Dictatorship",
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
  title = "Year Fixed Effects Models: Impact of Regime Types on Sovereignty Rhetoric (300d)"
)




##############################################
###################additional tests for h3
##############################################
emb <- emb %>%
  mutate(sovereignty_mentions = str_count(text, "\\bsovereignty\\b")) %>%
  mutate(sovereignty_freq = (sovereignty_mentions/WC)*100)


sovereignty_tallies <- emb %>%
  dplyr::group_by(ccode_iso, year, as.factor(dd_regime_pooled)) %>%
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
  dplyr::ungroup()


# Step 4: Convert to Panel Data Frame
sovereignty_tallies <- pdata.frame(sovereignty_tallies, index = c("ccode_iso", "year"))


# Step 5: Fit a Regression Model with Year Fixed Effects
fe_model <- plm(
  total_mentions ~ avg_risk * as.factor.dd_regime_pooled. +
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

base_risk <- coefficients_data$estimate[coefficients_data$term == "avg_risk"]

total_effects_country_year <- coefficients_data %>%
  dplyr::filter(grepl("^avg_risk($|:)", term)) %>%
  dplyr::mutate(
    regime_type = dplyr::case_when(
      term == "avg_risk" ~ "Democracy",
      term == "avg_risk:as.factor.dd_regime_pooled.Civilian dictatorship" ~ "Civilian Dictatorship",
      term == "avg_risk:as.factor.dd_regime_pooled.Military dictatorship" ~ "Military Dictatorship",
      term == "avg_risk:as.factor.dd_regime_pooled.Royal dictatorship"    ~ "Royal Dictatorship",
      TRUE ~ term
    ),
    total_effect = dplyr::case_when(
      term == "avg_risk" ~ estimate,
      TRUE ~ estimate + base_risk
    ),
    lower_ci = total_effect - 1.96 * std.error,
    upper_ci = total_effect + 1.96 * std.error
  ) %>%
  dplyr::select(regime_type, total_effect, lower_ci, upper_ci)

cat("\n=== Fourfold total risk effect by regime type (300d) ===\n")
print(total_effects_country_year)

# Reorder the regime_type factor based on the desired order
total_effects_country_year$regime_type <- factor(
  total_effects_country_year$regime_type,
  levels = c("Democracy",
             "Civilian Dictatorship",
             "Military Dictatorship",
             "Royal Dictatorship")
)


# Replot with the corrected order
interaction_plot <- ggplot(total_effects_country_year,
                           aes(x = regime_type, y = total_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(
    title = "Marginal Effect of Risk on Sovereignty Mentions (300d)",
    x = "Regime Type",
    y = "Marginal Effect of risk",
    caption = "95% confidence intervals shown."
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(size=13, angle = 45, hjust = 1),
    plot.title = element_text(size=14, face="bold")
  )


interaction_plot

ggsave( "~/Desktop/UNGDC/script/paper_chapter2/plot/fourfold_interaction_regime_risk_300d_Jul2026.png", interaction_plot,
        width = 10,
        height = 6)
