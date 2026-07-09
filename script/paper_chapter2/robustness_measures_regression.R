library(readr)
library(dplyr)
library(plm)
library(broom)
library(modelsummary)
library(ggplot2)
library(stringr)

emb <- read_csv("/Users/jihyeonbae/Desktop/UNGDC/output/embedding_static/glove/liwc_meta_with_keyword_distances_June2025.csv")

emb <- emb %>%
  rename_with(~ gsub("distance_", "", .x)) %>%
  rename_with(~ gsub("between_", "", .x))

cosine_vars <- c(
  "sovereignty_territory_cosine",
  "sovereignty_colonial_cosine",
  "sovereignty_westphalia_cosine",
  "sovereignty_interfere_cosine",
  "sovereignty_intervene_cosine"
)

emb <- emb %>%
  dplyr::mutate(across(all_of(cosine_vars), scale, .names = "{.col}_z"))

models_year <- list()
covariates <- c("risk", "WC", "WPS", "tone_pos","tone_neg", "negate", "focuspast", "mid_num_dispute", "ht_colonial")

for (dep_var in paste0(cosine_vars, "_z")) {
  formula_str <- paste(dep_var, "~ v2x_polyarchy +", paste(covariates, collapse = " + "))
  models_year[[dep_var]] <- plm(
    as.formula(formula_str), 
    effect = "time", 
    model = "within", 
    index = c("ccode_iso", "year"),
    data = emb
  )
}

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
  dplyr::mutate(term = case_when(
    term == "v2x_polyarchy" ~ "Polyarchy",
    term == "risk" ~ "Risk",
    term == "WC" ~ "Word Count",
    term == "WPS" ~ "Words Per Sentence",
    term == "tone_pos" ~ "Positive Tone",
    term == "tone_neg" ~ "Negative Tone",
    term == "negate" ~ "Negation",
    term == "focuspast" ~ "Focus Past",
    term == "mid_num_dispute" ~ "Number of Disputes",
    term == "ht_colonial" ~ "Colonial",
    TRUE ~ term
  ))

shape_mapping <- c(
  "Territory" = 17,
  "Colonial" = 18,
  "Westphalia" = 19,
  "Interfere" = 21,
  "Intervene" = 22
)

custom_order <- c(
  "Polyarchy",
  "Risk",
  "Number of Disputes",
  "Colonial",
  "Focus Past",
  "Positive Tone",
  "Negative Tone",
  "Negation",
  "Word Count",
  "Words Per Sentence"
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
  filename ="~/Desktop/UNGDC/script/paper_chapter2/plot/polyarchy_coef_plot.png",
  width = 10,
  height = 6)

modelsummary(
  models_year,
  output = "html",
  stars = TRUE,
  statistic = c("std.error", "p.value"),
  coef_omit = "(Intercept)",
  coef_map = list(
    "v2x_polyarchy" = "Polyarchy",
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
  title = "Year Fixed Effects Models: Polyarchy and Sovereignty Rhetoric"
)


#########################################################
## polity2 as a robustness checker
models_year <- list()

for (dep_var in paste0(cosine_vars, "_z")) {
  formula_str <- paste(dep_var, "~ polity2 +", paste(covariates, collapse = " + "))
  models_year[[dep_var]] <- plm(
    as.formula(formula_str), 
    effect = "time", 
    model = "within", 
    index = c("ccode_iso", "year"),
    data = emb
  )
}

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
  dplyr::mutate(term = case_when(
    term == "polity2" ~ "Polity2",
    term == "risk" ~ "Risk",
    term == "WC" ~ "Word Count",
    term == "WPS" ~ "Words Per Sentence",
    term == "tone_pos" ~ "Positive Tone",
    term == "tone_neg" ~ "Negative Tone",
    term == "negate" ~ "Negation",
    term == "focuspast" ~ "Focus Past",
    term == "mid_num_dispute" ~ "Number of Disputes",
    term == "ht_colonial" ~ "Colonial",
    TRUE ~ term
  ))

shape_mapping <- c(
  "Territory" = 17,
  "Colonial" = 18,
  "Westphalia" = 19,
  "Interfere" = 21,
  "Intervene" = 22
)

custom_order <- c(
  "Polity2",
  "Risk",
  "Number of Disputes",
  "Colonial",
  "Focus Past",
  "Positive Tone",
  "Negative Tone",
  "Negation",
  "Word Count",
  "Words Per Sentence"
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
  filename ="~/Desktop/UNGDC/script/paper_chapter2/plot/polity2_coef_plot.png",
  width = 10,
  height = 6)

modelsummary(
  models_year,
  output = "html",
  stars = TRUE,
  statistic = c("std.error", "p.value"),
  coef_omit = "(Intercept)",
  coef_map = list(
    "polity2" = "Polity2",
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
  title = "Year Fixed Effects Models: Polity2 and Sovereignty Rhetoric"
)



