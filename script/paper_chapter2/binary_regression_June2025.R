library(readr)
library(dplyr)
library(plm)
library(broom)
library(modelsummary)
library(ggplot2)
library(stringr)

# this data is about "DISTANCE"=1-cosine similarity. 
emb <- read_csv("~/Downloads/liwc_meta_with_keyword_distances_June2025.csv")

top5_interfere <- emb %>%
  dplyr::ungroup() %>%
  dplyr::arrange(distance_sovereignty_interfere_cosine) %>%  # smallest distance = closest meaning
  #dplyr::slice(1:20) %>%
  dplyr::select(ccode_iso, year, distance_sovereignty_interfere_cosine, text)

cat(top5_interfere$text[1])


# Clean column names and apply log if appropriate
emb <- emb %>%
  rename_with(~ gsub("distance_", "", .x)) %>%
  rename_with(~ gsub("between_", "", .x))

# Define distance-based DV variables
cosine_vars <- c(
  "sovereignty_territory_cosine",
  "sovereignty_colonial_cosine",
  "sovereignty_westphalia_cosine",
  "sovereignty_intervene_cosine",
  "sovereignty_interfere_cosine"
)

# Standardize the distance variables
emb <- emb %>%
  dplyr::mutate(across(all_of(cosine_vars), scale, .names = "{.col}_z"))

# Create binary democracy variable
emb <- emb %>%
  dplyr::mutate(democracy = ifelse(v2x_polyarchy >= 0.5, 1, 0))


### find excerpt
top5_distant <- emb %>%
  ungroup() %>%
  arrange(sovereignty_territory_cosine) %>%
  slice(1:5) %>%
  select(ccode_iso, year, sovereignty_territory_cosine, text)


print(top5_distant)
write_csv(top5_distant, "~/Desktop/top5_territory_distance.csv")


# Run year fixed effects regression
models_year <- list()
covariates <- c("risk", "WC", "WPS", "tone_neg", "tone_pos", "negate" ,"focuspast", "mid_num_dispute", "ht_colonial")

for (dep_var in paste0(cosine_vars, "_z")) {
  formula_str <- paste(dep_var, "~ democracy +", paste(covariates, collapse = " + "))
  models_year[[dep_var]] <- plm(
    as.formula(formula_str),
    data = emb,
    effect = "time",
    model = "within",
    index = c("ccode_iso", "year")
  )
}

# Display regression results
modelsummary(
  models_year,
  stars = TRUE,
  output = "html",
  title = "Year FE Models with Binary Democracy Indicator"
)

# Coefficient plot
plot_data <- lapply(names(models_year), function(name) {
  tidy(models_year[[name]]) %>%
    mutate(`Dependent Variable` = name)
}) %>% bind_rows()

plot_data <- plot_data %>%
  filter(term == "democracy") %>%
  mutate(`Dependent Variable` = str_replace(`Dependent Variable`, "_z", ""))

#  simplified labels
plot_data <- plot_data %>%
  mutate(dep_label = case_when(
    `Dependent Variable` == "sovereignty_colonial_cosine"   ~ "Colonial",
    `Dependent Variable` == "sovereignty_territory_cosine"  ~ "Territory",
    `Dependent Variable`== "sovereignty_westphalia_cosine" ~ "Westphalia",
    `Dependent Variable`== "sovereignty_intervene_cosine" ~ "Intervene",
    `Dependent Variable`== "sovereignty_interfere_cosine" ~ "Interfere",
    TRUE ~`Dependent Variable`
  ))


ggplot(plot_data, aes(x = dep_label, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  ylim(min=0.1, max=0.5)+
  labs(
    title = "Effect of Democracy on Semantic Distance",
    x = "Sovereignty Pair",
    y = "Coefficient Estimate",
    caption = "Year Fixed Effects; 95% CI; Semantic Distance(1 - Cosine similarity)"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption = element_text(size = 12)
  )


ggsave(
  filename ="~/Desktop/UNGDC/script/exploratory/static/plot/democracy_binary_coef_plot.png",
  width = 10,
  height = 6)


######## table itslef###

names(models_year) <- c("Territory", "Colonial", "Westphalia", "Intervene", "Interfere")

modelsummary(
  models_year,
  output = "html",
  stars = TRUE,
  title = " Coefficient Plots for Semantic Distance (Year Fixed Effects)",
  coef_omit = "(Intercept)",
  statistic = c("std.error", "p.value"),
  coef_map = c(
    "democracy" = "Democracy",
    "risk" = "Perceived Risk",
    "WC" = "Word Count",
    "WPS" = "Words per Sentence",
    "tone_neg" = "Negative Tone",
    "tone_pos" = "Positive Tone",
    "negate" = "Negation",
    "focuspast" = "Focus on Past",
    "mid_num_dispute" = "Number of Disputes",
    "ht_colonial" = "Colonial History"
  ),
  notes = "Standardized dependent variables; Year fixed effects included. *** p < 0.001; ** p < 0.01; * p < 0.05"
) 


############################another version

# Extract & combine tidy model results
plot_data2 <- lapply(names(models_year), function(name) {
  tidy(models_year[[name]]) %>%
    mutate(`Dependent Variable` = name)
}) %>% bind_rows()

# Filter to key IV and relabel
plot_data2 <- plot_data2 %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "democracy" ~ "Democracy",
      term == "risk" ~ "Perceived Risk",
      term == "WC" ~ "Word Count",
      term == "WPS" ~ "Words Per Sentence",
      term == "tone_neg" ~ "Negative Tone",
      term == "tone_pos" ~ "Positive Tone",
      term == "negate" ~ "Negation",
      term == "focuspast" ~ "Focus on Past",
      term == "mid_num_dispute" ~ "Number of Disputes",
      term == "ht_colonial" ~ "Colonial History",
      TRUE ~ term
    ),
    `Dependent Variable` = str_remove(`Dependent Variable`, "_z"),
    `Dependent Variable` = case_when(
      `Dependent Variable` == "sovereignty_territory" ~ "Territory",
      `Dependent Variable` == "sovereignty_colonial" ~ "Colonial",
      `Dependent Variable` == "sovereignty_westphalia" ~ "Westphalia",
      `Dependent Variable` == "sovereignty_intervene" ~ "Intervene",
      `Dependent Variable` == "sovereignty_interfere" ~ "Interfere",
      TRUE ~ `Dependent Variable`
    )
  )

# Ensure variable order
term_order <- c("Democracy", "Perceived Risk", "Colonial History", "Focus on Past", 
                "Word Count", "Words Per Sentence", "Negative Tone", "Positive Tone", "Negation", "Number of Disputes")

plot_data2$term <- factor(plot_data2$term, levels = term_order)

# Plot
ggplot(plot_data2, aes(x = term, y = estimate, shape = `Dependent Variable`)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2,
                position = position_dodge(width = 0.6)) +
  labs(
    title = "Semantic Distance from Sovereignty",
    x = "Independent Variables",
    y = "Coefficient Estimate",
    caption = "Year Fixed Effects; 95% CI; Distance (1 - Cosine Similarity)"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  scale_shape_manual(values = c(17, 18, 19, 21, 22))

ggsave("~/Desktop/UNGDC/script/exploratory/static/plot/democracy_binary_plot_ver2.png",
       width = 10, height = 6, dpi = 300)


# Plot with coefficient numeric values. 
ggplot(plot_data2, aes(x = term, y = estimate, shape = `Dependent Variable`)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2,
                position = position_dodge(width = 0.6)) +
  labs(
    title = "Semantic Distance from Sovereignty",
    x = "Independent Variables",
    y = "Coefficient Estimate",
    caption = "Year Fixed Effects; 95% CI; Distance (1 - Cosine Similarity)"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  scale_shape_manual(values = c(17, 18, 19, 21, 22)) +
  geom_text(
    aes(label = sprintf("%.2f", estimate)),
    position = position_jitter(width = 0, height = 0.1),
    hjust = -0.3,
    size = 3.5
  )



ggsave("~/Desktop/UNGDC/script/exploratory/static/plot/democracy_binary_plot_ver3.png",
       width = 10, height = 6, dpi = 300)


#### Interaction Term ####
# Compute sovereignty frequency
emb <- emb %>%
  dplyr::mutate(
    sovereignty_mentions = str_count(text, "\\bsovereignty\\b"),
    sovereignty_freq = (sovereignty_mentions / WC) * 100
  )


# Aggregate by country-year-democracy
sovereignty_tallies <- emb %>%
  dplyr::group_by(ccode_iso, year, democracy) %>%
  dplyr::summarise(
    total_mentions = mean(sovereignty_freq, na.rm=TRUE),
    risk = mean(risk, na.rm=TRUE),
    mid_num_dispute = mean(mid_num_dispute, na.rm=TRUE),
    colonial = mean(ht_colonial, na.rm=TRUE),
    focuspast = mean(focuspast, na.rm=TRUE),
    WC = mean(WC, na.rm=TRUE),
    WPS = mean(WPS, na.rm=TRUE),
    tone_neg = mean(tone_neg, na.rm=TRUE),
    tone_pos = mean(tone_pos, na.rm=TRUE),
    negation = mean(negate, na.rm=TRUE)
  ) %>%
  ungroup()

# Create panel
sovereignty_tallies <- pdata.frame(sovereignty_tallies, index = c("ccode_iso", "year"))

# Fit model
fe_model <- plm(
  total_mentions ~ risk * as.factor(democracy) +
    WC + WPS + tone_neg + tone_pos + negation + focuspast + colonial + mid_num_dispute,
  data = sovereignty_tallies,
  model = "within",
  effect = "time"
)

# View summary
summary(fe_model)
modelsummary(fe_model,
             stars = TRUE,
             output = "html",
             title = "Year FE Models with Binary Democracy * Risk Interaction")

# Extract coefficients
coefficients_data <- tidy(fe_model) %>%
  filter(term != "(Intercept)")

# Check term names!
print(coefficients_data$term)

# Calculate total effects
total_effects_country_year <- coefficients_data %>%
  filter(str_detect(term, "risk")) %>%
  mutate(
    regime_type = case_when(
      term == "risk" ~ "Autocracy",
      term == "risk:as.factor(democracy)1" ~ "Democracy"
    ),
    total_effect = case_when(
      term == "risk" ~ estimate,
      TRUE ~ estimate + coefficients_data$estimate[coefficients_data$term == "risk"]
    ),
    lower_ci = total_effect - 1.96 * std.error,
    upper_ci = total_effect + 1.96 * std.error
  )

# Plot
interaction_plot <- ggplot(total_effects_country_year, aes(x = regime_type, y = total_effect)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(
    title = "Total Effect of Risk on Sovereignty Mentions by Regime Type",
    x = "Regime Type",
    y = "Total Effect",
    caption = "Error bars: 95% confidence intervals."
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

interaction_plot

ggsave("~/Desktop/UNGDC/script/exploratory/static/plot/binary_interaction_plot.png",
       width = 10, height = 6, dpi = 300)


