library(readxl)
library(tidyverse)
library(janitor)
library(scales)

happiness <- read_excel("data/world_happiness.xlsx") |>
  clean_names()

# GDP vs happiness
p1 <- ggplot(happiness, aes(x = economy_gdp_per_capita, y = happiness_score)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "GDP is strongly related to happiness, but not enough on its own",
    x = "Economy (GDP per Capita)",
    y = "Happiness Score"
  ) +
  theme_minimal()

ggsave("output/figures/gdp_vs_happiness.png", p1, width = 8, height = 5, dpi = 300)

# Correlation bar chart
cor_tbl <- happiness |>
  select(happiness_score, economy_gdp_per_capita, family,
         health_life_expectancy, freedom,
         trust_government_corruption, generosity) |>
  summarise(across(-happiness_score,
                   ~ cor(.x, happiness_score, use = "complete.obs"))) |>
  pivot_longer(everything(), names_to = "driver", values_to = "correlation") |>
  arrange(correlation)

p2 <- ggplot(cor_tbl, aes(x = reorder(driver, correlation), y = correlation)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Some drivers matter more than others",
    x = NULL,
    y = "Correlation with Happiness Score"
  ) +
  theme_minimal()

ggsave("output/figures/driver_correlations.png", p2, width = 8, height = 5, dpi = 300)
