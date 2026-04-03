library(readxl)
library(tidyverse)
library(janitor)

happiness <- read_excel("data/world_happiness.xlsx") |>
  clean_names()

# Top and bottom groups
happiness_segmented <- happiness |>
  mutate(
    happiness_group = case_when(
      happiness_score >= quantile(happiness_score, 0.75, na.rm = TRUE) ~ "Top 25%",
      happiness_score <= quantile(happiness_score, 0.25, na.rm = TRUE) ~ "Bottom 25%",
      TRUE ~ "Middle 50%"
    )
  )

segment_summary <- happiness_segmented |>
  group_by(happiness_group) |>
  summarise(
    n = n(),
    avg_happiness = mean(happiness_score, na.rm = TRUE),
    avg_gdp = mean(economy_gdp_per_capita, na.rm = TRUE),
    avg_family = mean(family, na.rm = TRUE),
    avg_health = mean(health_life_expectancy, na.rm = TRUE),
    avg_freedom = mean(freedom, na.rm = TRUE),
    avg_trust = mean(trust_government_corruption, na.rm = TRUE),
    avg_generosity = mean(generosity, na.rm = TRUE)
  )

print(segment_summary)

# Rank strongest linear relationships with happiness
cor_tbl <- happiness |>
  select(happiness_score, economy_gdp_per_capita, family,
         health_life_expectancy, freedom,
         trust_government_corruption, generosity) |>
  summarise(across(-happiness_score,
                   ~ cor(.x, happiness_score, use = "complete.obs"))) |>
  pivot_longer(everything(), names_to = "driver", values_to = "correlation") |>
  arrange(desc(abs(correlation)))

print(cor_tbl)
