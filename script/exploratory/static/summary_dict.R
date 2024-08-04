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

dic_counts %>%
  filter(!is.na(democracy)) %>%
  group_by(year, democracy) %>%
  summarize(sovereignty = mean(sovereignty),
            cilo = quantile(sovereignty, probs = .025),
            cihi = quantile(sovereignty, probs = .975),
            .groups = 'drop') %>%
  ggplot(aes(x = year, y = sovereignty, color = as.factor(democracy), fill = as.factor(democracy), group = democracy)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_ribbon(aes(ymin = cilo, ymax = cihi), alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = paste0("Sovereignty (N = ", nrow(dic_counts), ")"), 
       y = "Percent", x = "Year", color = "Regime Type", fill = "Regime Type") +
  ylim(0, 0.5) +
  theme_minimal() 

dic_counts %>%
  filter(!is.na(dd_regime)) %>%
  group_by(year, dd_regime) %>%
  summarize(rights = mean(rights),
            cilo = quantile(rights, probs = .025),
            cihi = quantile(rights, probs = .975),
            .groups = 'drop') %>%
  ggplot(aes(x = year, y = rights, color = as.factor(dd_regime), fill = as.factor(dd_regime), group = dd_regime)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_ribbon(aes(ymin = cilo, ymax = cihi), alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = paste0("Sovereignty (N = ", nrow(dic_counts), ")"), 
       y = "Percent", x = "Year", color = "Regime Type", fill = "Regime Type") +
  ylim(0, 0.5) +
  theme_minimal() 

dic_counts %>%
  filter(!is.na(dd_regime)) %>%
  group_by(year, dd_regime) %>%
  summarize(trade = mean(trade),
            cilo = quantile(trade, probs = .025),
            cihi = quantile(trade, probs = .975),
            .groups = 'drop') %>%
  ggplot(aes(x = year, y = trade, color = as.factor(dd_regime), fill = as.factor(dd_regime), group = dd_regime)) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_ribbon(aes(ymin = cilo, ymax = cihi), alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = paste0("Sovereignty (N = ", nrow(dic_counts), ")"), 
       y = "Percent", x = "Year", color = "Regime Type", fill = "Regime Type") +
  ylim(0, 0.5) +
  theme_minimal() 

