library(readxl)
library(tidyverse)
library(janitor)
library(skimr)

# Set working directory to project root if needed
setwd("d:/RProjects/happiness-analysis")
file_path <- "data/world_happiness.xlsx"

# Check sheet names first
excel_sheets(file_path)

# Replace with actual sheet name if needed
happiness <- read_excel(file_path) |>
  clean_names()

glimpse(happiness)
names(happiness)
skim(happiness)

happiness <- happiness |>
  rename(
    country = country,
    happiness = happiness_score,
    gdp = economy_gdp_per_capita,
    family = family,
    health = health_life_expectancy,
    freedom = freedom,
    generosity = generosity,
    trust = trust_government_corruption
  )


glimpse(happiness)
names(happiness)
skim(happiness)
