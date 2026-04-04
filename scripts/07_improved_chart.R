library(readxl)
library(tidyverse)
library(janitor)

happiness <- read_excel("data/world_happiness.xlsx") |>
  clean_names()

dir.create("output/figures/slide_assets", recursive = TRUE, showWarnings = FALSE)

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
  arrange(desc(Correlation))

# first attempt
# p_driver_correlations_mini <- ggplot(
#   cor_tbl,
#   aes(x = reorder(Driver, Correlation), y = Correlation)
# ) +
#   geom_col(width = 0.7, fill = "#F2F2F2", color = "#333333", linewidth = 0.4) +
#   coord_flip() +
#   labs(x = NULL, y = NULL) +
#   theme_minimal(base_size = 11) +
#   theme(
#     panel.background = element_rect(fill = "transparent", color = NA),
#     plot.background  = element_rect(fill = "transparent", color = NA),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.text.y = element_text(size = 10, face = "bold", color = "#FFFFFF"),
#     axis.ticks.y = element_blank(),
#     plot.margin = margin(5, 5, 5, 5)
#   )

# second attempt adding correlation values
# p_driver_correlations_mini <- ggplot(
#   cor_tbl,
#   aes(x = reorder(Driver, Correlation), y = Correlation)
# ) +
#   geom_col(
#     width = 0.7,
#     fill = "#F2F2F2",
#     color = "#333333",
#     linewidth = 0.4
#   ) +
#   geom_text(
#     aes(label = Correlation),
#     hjust = 0.5,
#     color = "#333333",
#     size = 3.5,
#     fontface = "bold"
#   ) +
#   coord_flip() +
#   labs(x = NULL, y = NULL) +
#   theme_minimal(base_size = 11) +
#   theme(
#     panel.background = element_rect(fill = "transparent", color = NA),
#     plot.background  = element_rect(fill = "transparent", color = NA),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.text.y = element_text(size = 10, face = "bold", color = "#222222"),
#     axis.ticks.y = element_blank(),
#     plot.margin = margin(5, 5, 5, 5)
#   )

# third attempt
p_driver_correlations_mini <- ggplot(
  cor_tbl,
  aes(x = reorder(Driver, Correlation), y = Correlation)
) +
  geom_col(
    width = 0.7,
    fill = "#4A4A4A",
    color = "#4A4A4A",
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      y = Correlation * 0.1,
      label = sprintf("%.2f", Correlation)
    ),
    hjust = 0,
    color = "#FFFFFF",
    size = 3.2,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_text(
      size = 11,
      face = "bold",
      color = "#222222"
    ),
    axis.ticks.y = element_blank(),
    
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave(
  "output/figures/slide_assets/driver_correlations_mini.png",
  p_driver_correlations_mini,
  width = 3.0,
  height = 1.8,
  dpi = 300,
  bg = "white"
)
