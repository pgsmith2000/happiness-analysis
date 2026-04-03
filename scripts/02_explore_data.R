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

# Correlations
num_data <- happiness |> select(where(is.numeric))
cor(num_data, use = "complete.obs")

# Pair plot for main drivers
num_data |>
  select(happiness_score, economy_gdp_per_capita, family,
         health_life_expectancy, freedom,
         trust_government_corruption, generosity) |>
  ggpairs()