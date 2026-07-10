library(readr)
library(dplyr)
library(plm)
library(broom)
library(ggplot2)

df <- read_csv("~/Desktop/UNGDC/output/embedding_glove/liwc_meta_with_keyword_distances_300d_Jul2026.csv",
               show_col_types = FALSE)

# Rename the 5 distance columns to short labels matching the paper's Plot 1 legend
df <- df %>%
  rename(
    colonial   = distance_sovereignty_colonial,
    territory  = distance_sovereignty_territory,
    westphalia = distance_sovereignty_westphalia,
    intervene  = distance_sovereignty_intervene,
    interfere  = distance_sovereignty_interfere
  )

dep_vars <- c("colonial", "territory", "westphalia", "intervene", "interfere")

# Standardize each distance measure (z-score), per paper's stated method
for (v in dep_vars) {
  df[[v]] <- as.numeric(scale(df[[v]]))
}

covariates <- c("democracy", "risk", "ht_colonial", "focuspast", "WC", "WPS",
                 "tone_neg", "tone_pos", "negate", "mid_num_dispute")

models_twoway <- list()
for (dep_var in dep_vars) {
  formula_str <- paste(dep_var, "~", paste(covariates, collapse = " + "))
  models_twoway[[dep_var]] <- plm(
    as.formula(formula_str),
    effect = "twoways",
    model = "within",
    index = c("ccode_iso", "year"),
    data = df
  )
}

# Combine tidy coefficients across all 5 models
model_results <- bind_rows(lapply(names(models_twoway), function(dep_var) {
  tidy(models_twoway[[dep_var]]) %>%
    mutate(Dependent_Var = dep_var)
}))

term_labels <- c(
  democracy = "Democracy",
  risk = "Perceived Risk",
  ht_colonial = "Colonial History",
  focuspast = "Focus on Past",
  WC = "Word Count",
  WPS = "Words Per Sentence",
  tone_neg = "Negative Tone",
  tone_pos = "Positive Tone",
  negate = "Negation",
  mid_num_dispute = "Number of Disputes"
)

plot_data <- model_results %>%
  filter(!is.na(estimate), !is.na(std.error)) %>%
  mutate(
    term_label = factor(term_labels[term], levels = term_labels[covariates]),
    Dependent_Var = factor(Dependent_Var, levels = dep_vars,
                            labels = c("Colonial", "Territory", "Westphalia", "Intervene", "Interfere"))
  )

p <- ggplot(plot_data, aes(x = term_label, y = estimate, shape = Dependent_Var)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = 0.3, position = position_dodge(width = 0.6)) +
  labs(
    title = "Semantic Distance from Sovereignty (Replication, 300d GloVe, Two-Way FE)",
    x = "Independent Variables",
    y = "Coefficient Estimate",
    shape = "Dependent Variable",
    caption = "Country + Year Fixed Effects; 95% CI; Distance (1 - Cosine Similarity)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("~/Desktop/UNGDC/script/paper_chapter2/plot/plot1_replication_300d_twoway.png", p, width = 10, height = 6)

cat("\n=== Coefficient table (democracy row only, all 5 DVs) ===\n")
print(model_results %>% filter(term == "democracy") %>% select(Dependent_Var, estimate, std.error, p.value))

cat("\nSaved plot to script/paper_chapter2/plot/plot1_replication_300d_twoway.png\n")
