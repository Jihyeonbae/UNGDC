library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

emb <- read_csv("~/Desktop/UNGDC/output/embedding_glove/liwc_meta_with_keyword_distances.csv")

# Create a long format dataset
distance_data_long <- emb %>%
  filter(!is.na(dd_regime)) %>%
  select(year, dd_regime, distance_between_sovereign_westphalia) %>%
  group_by(year, dd_regime) %>%
  summarize(mean_distance = mean(distance_between_sovereign_westphalia, na.rm = TRUE),
            cilo = quantile(distance_between_sovereign_westphalia, probs = .025, na.rm = TRUE),
            cihi = quantile(distance_between_sovereign_westphalia, probs = .975, na.rm = TRUE),
            .groups = 'drop')

# Define the color palette
color_palette <- c("0" = "grey82",   # Parliamentary democracy
                   "1" = "grey75",  # Mixed democracy
                   "2" = "grey60",  # Presidential democracy
                   "3" = "grey35",  # Civilian dictatorship
                   "4" = "grey20",  # Military dictatorship
                   "5" = "grey0")  # Royal dictatorship

# Create the plot
distance_plot <- distance_data_long %>%
  ggplot(aes(x = year, y = mean_distance, color = as.factor(dd_regime), fill = as.factor(dd_regime), group = dd_regime)) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  geom_ribbon(aes(ymin = cilo, ymax = cihi), alpha = 0.1, color = NA) + # Confidence interval
  scale_color_manual(values = color_palette, 
                     labels = c(
                       "0" = "Parliamentary democracy",
                       "1" = "Mixed (semi-presidential) democracy",
                       "2" = "Presidential democracy",
                       "3" = "Civilian dictatorship",
                       "4" = "Military dictatorship",
                       "5" = "Royal dictatorship"
                     )) +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(y = "Distance", x = "Year", color = "Regime Type", fill = "Regime Type") +
  scale_x_continuous(breaks = seq(min(emb$year, na.rm = TRUE), max(emb$year, na.rm = TRUE), by = 5)) +
  ylim(0.75, max(distance_data_long$cihi, na.rm = TRUE)) +
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
  )

# Print the plot
print(distance_plot)

# Save the plot
ggsave("~/Desktop/UNGDC/script/exploratory/static/plot/distance_sovereign_westphalia_with_ci.png", plot = distance_plot, width = 12, height = 8, dpi = 300)
