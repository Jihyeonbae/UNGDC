# ========================================================================= #
# Project: Lexical War on International Law
# - Script: Compare average usage of legal principles
# ========================================================================= #


# Load packages and custom functions --------------------------------------

source(here::here("script/exploratory/static/00-func.R"))


# Load dictionaries & speeches --------------------------------------------

load(here("data/processed/sentence.Rdata"))

# Extract dictionary counts -----------------------------------------------

dic_counts <- mftPercent(meta)

# Plot averages by regime --------------------------------------------------

set.seed(42)

sov_percentage <- dic_counts %>%
  group_by(dd_regime) %>%
  summarize(sovereignty = mean(sovereignty),
            cilo = quantile(sovereignty, probs = .025),
            cihi = quantile(sovereignty, probs = .975)) %>%
  ggplot(aes(x = dd_regime, y = sovereignty, 
             ymin = cilo, ymax = cihi)) +
  geom_point(size = 1.5, position=position_dodge(width=-0.3)) +
  geom_errorbar(width=0, position=position_dodge(width=-0.3)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = paste0("Rights (N = ", nrow(dic_counts), ")"), 
       y = "Percent", x = NULL) +
  ylim(0,1) + coord_flip() 



# time and dictionary --------------------------------
dic_counts %>%
  group_by(year, dd_regime) %>%
  summarize(sovereignty = mean(sovereignty),
            cilo = quantile(sovereignty, probs = .025),
            cihi = quantile(sovereignty, probs = .975)) %>%
  ggplot(aes(x = year, y = sovereignty, 
             ymin = cilo, ymax = cihi)) +
  geom_point(size = 1.5, position=position_dodge(width=-0.3)) +
  geom_errorbar(width=0, position=position_dodge(width=-0.3)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = paste0("Trade (N = ", nrow(dic_counts), ")"), 
       y = "Percent", x = "Year") +
  ylim(0,0.5) + coord_flip()


# time and dictionary --------------------------------
library(dplyr)
library(ggplot2)


# Reshape data to long format
dic_counts_long <- dic_counts %>%
  filter(!is.na(democracy)) %>%
  select(year, democracy, sovereignty, rights, trade) %>%
  pivot_longer(cols = c(sovereignty, rights, trade), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(year, democracy, variable) %>%
  summarize(mean_value = mean(value, na.rm = TRUE),
            cilo = quantile(value, probs = .025, na.rm = TRUE),
            cihi = quantile(value, probs = .975, na.rm = TRUE),
            .groups = 'drop')

# Create the combined plot
combined_plot <- dic_counts_long %>%
  ggplot(aes(x = year, y = mean_value, color = as.factor(democracy), fill = as.factor(democracy), group = democracy)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_ribbon(aes(ymin = cilo, ymax = cihi), alpha = 0.05) +
  scale_color_manual(values = c("0" = "gray17", "1" = "grey"), labels = c("0" = "Non-Democracy", "1" = "Democracy")) +
  scale_fill_manual(values = c("0" = "gray17", "1" = "grey"), labels = c("0" = "Non-Democracy", "1" = "Democracy")) +
  labs(y = "Percent", x = "Year", color = "Regime Type", fill = "Regime Type") +
  scale_x_continuous(breaks = seq(min(dic_counts$year, na.rm = TRUE), max(dic_counts$year, na.rm = TRUE), by = 5)) +
  ylim(0, 1.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "serif"),
    axis.title.x = element_text(size = 12, family = "serif"),
    axis.title.y = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 10, family = "serif"),
    legend.title = element_text(size = 12, face = "bold", family = "serif"),
    legend.text = element_text(size = 10, family = "serif"),
    panel.grid.major = element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold", family = "serif"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
    ) +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = c(sovereignty = "Sovereignty", rights = "Rights", trade = "Trade")))+
  theme(legend.position = "bottom")

print(combined_plot)

ggsave("plot/combined_plot.png", plot = combined_plot, width = 12, height = 8, dpi = 300)

# sixfold regime type-----------------------

dic_counts_long_regime <- dic_counts %>%
  filter(!is.na(dd_regime)) %>%
  select(year, dd_regime, sovereignty, rights, trade) %>%
  pivot_longer(cols = c(sovereignty, rights, trade), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(year, dd_regime, variable) %>%
  summarize(mean_value = mean(value, na.rm = TRUE),
            cilo = quantile(value, probs = .025, na.rm = TRUE),
            cihi = quantile(value, probs = .975, na.rm = TRUE),
            .groups = 'drop')

color_palette <- c("0" = "grey90",   # Parliamentary democracy
                   "1" = "grey77", # Mixed democracy
                   "2" = "grey60", # Presidential democracy
                   "3" = "grey45", # Civilian dictatorship
                   "4" = "grey30", # Military dictatorship
                   "5" = "grey15")  # Royal dictatorship


# Create the combined plot based on 6fold regime
combined_plot <- dic_counts_long_regime %>%
  ggplot(aes(x = year, y = mean_value, color = as.factor(dd_regime), fill = as.factor(dd_regime), group = dd_regime)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +  
  scale_color_manual(values = color_palette, 
                      labels = c(
                         "0" = "Parliamentary democracy",
                         "1" = "Mixed (semi-presidential) democracy",
                         "2" = "Presidential democracy",
                         "3" = "Civilian dictatorship",
                         "4" = "Military dictatorship",
                         "5" = "Royal dictatorship"
                      )
                     ) +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(y = "Percent", x = "Year", color = "Regime Type", fill = "Regime Type") +
  scale_x_continuous(breaks = seq(min(dic_counts$year, na.rm = TRUE), max(dic_counts$year, na.rm = TRUE), by = 5)) +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "serif"),
    axis.title.x = element_text(size = 12, family = "serif"),
    axis.title.y = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 10, family = "serif"),
    legend.title = element_text(size = 12, face = "bold", family = "serif"),
    legend.text = element_text(size = 10, family = "serif"),
    panel.grid.major = element_line(size = 0.1, linetype = 'dashed', colour = "grey"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold", family = "serif"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "bottom"
  ) +
  facet_wrap(~variable, scales = "free_y", labeller = labeller(variable = c(sovereignty = "Sovereignty", rights = "Rights", trade = "Trade")))

print(combined_plot)

ggsave("plot/sixfold_regime.png", plot = combined_plot, width = 12, height = 8, dpi = 300)
