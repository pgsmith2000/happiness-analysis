library(readxl)
library(tidyverse)
library(janitor)

# Load data
happiness <- read_excel("data/world_happiness.xlsx") |>
  clean_names()

# Create reusable happiness segments
happiness_segmented <- happiness |>
  mutate(
    happiness_group = case_when(
      happiness_score >= quantile(happiness_score, 0.75, na.rm = TRUE) ~ "Top 25%",
      happiness_score <= quantile(happiness_score, 0.25, na.rm = TRUE) ~ "Bottom 25%",
      TRUE ~ "Middle 50%"
    ),
    happiness_group = factor(
      happiness_group,
      levels = c("Bottom 25%", "Middle 50%", "Top 25%")
    )
  )

# Segment summary using cleaner labels
segment_summary <- happiness_segmented |>
  group_by(happiness_group) |>
  summarise(
    Countries = n(),
    `Avg Happiness` = mean(happiness_score, na.rm = TRUE),
    GDP = mean(economy_gdp_per_capita, na.rm = TRUE),
    `Social Support` = mean(family, na.rm = TRUE),
    Health = mean(health_life_expectancy, na.rm = TRUE),
    Freedom = mean(freedom, na.rm = TRUE),
    Trust = mean(trust_government_corruption, na.rm = TRUE),
    Generosity = mean(generosity, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(-c(happiness_group, Countries), ~ round(.x, 2)))

print(segment_summary)

write.csv(
  segment_summary,
  "output/segment_summary.csv",
  row.names = FALSE
)

# Correlation table with cleaner labels
cor_tbl <- happiness |>
  transmute(
    Happiness = happiness_score,
    GDP = economy_gdp_per_capita,
    `Social Support` = family,
    Health = health_life_expectancy,
    Freedom = freedom,
    Trust = trust_government_corruption,
    Generosity = generosity
  ) |>
  summarise(across(
    -Happiness,
    ~ cor(.x, Happiness, use = "complete.obs")
  )) |>
  pivot_longer(
    everything(),
    names_to = "Driver",
    values_to = "Correlation"
  ) |>
  arrange(desc(abs(Correlation))) |>
  mutate(Correlation = round(Correlation, 2))

print(cor_tbl)

write.csv(
  cor_tbl,
  "output/driver_correlations.csv",
  row.names = FALSE
)

# Long-format segment comparison table for easier plotting/reporting
segment_long <- happiness_segmented |>
  select(
    happiness_group,
    economy_gdp_per_capita,
    family,
    health_life_expectancy,
    freedom,
    trust_government_corruption,
    generosity
  ) |>
  pivot_longer(
    -happiness_group,
    names_to = "driver",
    values_to = "value"
  ) |>
  mutate(
    driver = recode(
      driver,
      economy_gdp_per_capita = "GDP",
      family = "Social Support",
      health_life_expectancy = "Health",
      freedom = "Freedom",
      trust_government_corruption = "Trust",
      generosity = "Generosity"
    )
  )

segment_driver_summary <- segment_long |>
  group_by(happiness_group, driver) |>
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    mean_value = round(mean_value, 2),
    median_value = round(median_value, 2)
  )

print(segment_driver_summary)

write.csv(
  segment_driver_summary,
  "output/segment_driver_summary.csv",
  row.names = FALSE
)
