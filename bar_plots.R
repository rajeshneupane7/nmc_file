library(tidyverse)
setwd("C:/Users/rajes/Desktop/nmc_files")

# Read the data
data <- read.csv("C:\\Users\\rajes\\Desktop\\nmc_files\\cleaned_model_metrics_summary_with_se.csv")



# Define metrics and standard error columns
metrics <- c("Accuracy", "Precision", "Recall", "F1.Score", "ROC.AUC")
se_cols <- paste0(metrics, "..SE")

# Pivot the data to long format for plotting
df_long <- data %>%
  pivot_longer(
    cols = all_of(metrics),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  left_join(
    data %>%
      pivot_longer(
        cols = all_of(se_cols),
        names_to = "Metric",
        values_to = "SE"
      ) %>%
      mutate(Metric = gsub("\\.\\.SE", "", Metric)),
    by = c("Algorithm", "Features.Used", "Metric")
  )

# Plot
ggplot(df_long, aes(x = `Features.Used`, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = Value - SE, ymax = Value + SE),
    position = position_dodge(width = 0.9),
    width = 0.3
  ) +
  facet_wrap(~ Algorithm, ncol = 1) +
  labs(
    title = "Selected Model Metrics by Feature Set",
    x = "Feature Set",
    y = "",
    fill = "Metric"
  ) +
  ylim(0, 1.05) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set2")

# Save the plot
ggsave('model_metrics.png', dpi = 300, width = 6, height = 10, units = 'in')
