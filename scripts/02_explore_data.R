library(readxl)
library(tidyverse)
library(janitor)
library(skimr)
library(GGally)

happiness <- read_excel("data/world_happiness.xlsx") |>
  clean_names()

# Numeric summary
happiness |>
  summarise(across(where(is.numeric), list(mean = mean, median = median), na.rm = TRUE))

# Now save it as a CSV file
write.csv(summary_stats, "output/summary_stats.csv", row.names = FALSE)

# save this as an object
summary_stats <- happiness |>
  summarise(across(where(is.numeric), 
                   list(mean = mean, median = median), 
                   na.rm = TRUE))

# Reshape the data for a boxplot
happiness_long <- happiness |>
  select(where(is.numeric)) |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

# Now plotit
ggplot(happiness_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribution of Key Happiness Drivers",
    x = NULL,
    y = "Value"
  ) +
  theme_minimal()

# Let's try looking at this another way with segments
happiness_segmented <- happiness |>
  mutate(
    happiness_group = case_when(
      happiness_score >= quantile(happiness_score, 0.75, na.rm = TRUE) ~ "Top 25%",
      happiness_score <= quantile(happiness_score, 0.25, na.rm = TRUE) ~ "Bottom 25%",
      TRUE ~ "Middle 50%"
    )
  )

# Then reshape this table
happiness_long <- happiness_segmented |>
  select(happiness_group,
         economy_gdp_per_capita,
         family,
         health_life_expectancy,
         freedom,
         trust_government_corruption,
         generosity) |>
  pivot_longer(
    -happiness_group,
    names_to = "variable",
    values_to = "value"
  )

# Relable the variables so they are more readable
happiness_long <- happiness_long |>
  mutate(variable = recode(variable,
                           economy_gdp_per_capita = "GDP",
                           health_life_expectancy = "Health",
                           family = "Social Support",
                           trust_government_corruption = "Trust",
                           generosity = "Generosity",
                           freedom = "Freedom"
  ))

# Then plot these results
ggplot(happiness_long, aes(x = happiness_group, y = value, fill = happiness_group)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "How Key Drivers Differ Across Happiness Segments",
    x = NULL,
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Let's make that data an object
plot_segment_box <- ggplot(happiness_long, aes(x = happiness_group, y = value, fill = happiness_group)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "How Key Drivers Differ Across Happiness Segments",
    x = NULL,
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Then plot and save it
ggsave(
  filename = "output/figures/segment_boxplot.png",
  plot = plot_segment_box,
  width = 10,
  height = 6,
  dpi = 300
)

# additional formats 
ggsave("output/figures/segment_boxplot.pdf", plot_segment_box, width = 10, height = 6)
ggsave("output/figures/segment_boxplot.jpg", plot_segment_box, width = 10, height = 6, dpi = 300)

# Correlations
num_data <- happiness |> select(where(is.numeric))
cor(num_data, use = "complete.obs")

# Rename variables for pair plot for main drivers
num_data_clean <- num_data |>
  select(
    Happiness = happiness_score,
    GDP = economy_gdp_per_capita,
    Social_Support = family,
    Health = health_life_expectancy,
    Freedom = freedom,
    Trust = trust_government_corruption,
    Generosity = generosity
  )

# now plot it
library(GGally)

plot_pairs <- ggpairs(
  num_data_clean,
  upper = list(continuous = wrap("cor", size = 4)),
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1)),
  diag  = list(continuous = wrap("densityDiag"))
) +
  theme_minimal(base_size = 12) +
  ggtitle("Relationships Between Key Happiness Drivers") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8)
  )

# Let's save it cleanly
ggsave(
  "output/figures/ggpairs_clean.png",
  plot_pairs,
  width = 12,
  height = 10,
  dpi = 300
)
