library(readxl)
library(tidyverse)
library(janitor)
library(scales)

# Load data
happiness <- read_excel("data/world_happiness.xlsx") |>
  clean_names()

# Create ordered happiness segments
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

# Create figures folder if needed
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. GDP vs Happiness
# -----------------------------
p_gdp_happiness <- ggplot(
  happiness,
  aes(x = economy_gdp_per_capita, y = happiness_score)
) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  labs(
    title = "GDP is strongly related to happiness, but not enough on its own",
    subtitle = "Economic strength matters, but additional factors also shape outcomes",
    x = "GDP per Capita",
    y = "Happiness Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "output/figures/gdp_vs_happiness.png",
  p_gdp_happiness,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------
# 2. Driver correlations
# -----------------------------
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
  arrange(Correlation) |>
  mutate(Correlation = round(Correlation, 2))

p_driver_correlations <- ggplot(
  cor_tbl,
  aes(x = reorder(Driver, Correlation), y = Correlation)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Some drivers matter far more than others",
    subtitle = "GDP, health, and social support show the strongest relationship with happiness",
    x = NULL,
    y = "Correlation with Happiness Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "output/figures/driver_correlations.png",
  p_driver_correlations,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------
# 3. Segment boxplot
# -----------------------------
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

p_segment_box <- ggplot(
  segment_long,
  aes(x = happiness_group, y = value, fill = happiness_group)
) +
  geom_boxplot() +
  facet_wrap(~driver, scales = "free_y") +
  labs(
    title = "How key drivers differ across happiness score segments",
    subtitle = "Countries grouped into bottom 25%, middle 50%, and top 25% by happiness score",
    x = NULL,
    y = "Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold")
  )

ggsave(
  "output/figures/segment_boxplot.png",
  p_segment_box,
  width = 10,
  height = 6,
  dpi = 300
)

# -----------------------------
# 4. Segment mean comparison
# -----------------------------
segment_means <- happiness_segmented |>
  group_by(happiness_group) |>
  summarise(
    GDP = mean(economy_gdp_per_capita, na.rm = TRUE),
    `Social Support` = mean(family, na.rm = TRUE),
    Health = mean(health_life_expectancy, na.rm = TRUE),
    Freedom = mean(freedom, na.rm = TRUE),
    Trust = mean(trust_government_corruption, na.rm = TRUE),
    Generosity = mean(generosity, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    -happiness_group,
    names_to = "driver",
    values_to = "mean_value"
  )

p_segment_means <- ggplot(
  segment_means,
  aes(x = driver, y = mean_value, group = happiness_group, color = happiness_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Top-performing countries lead across nearly every major driver",
    subtitle = "The largest gaps appear in GDP, health, and social support",
    x = NULL,
    y = "Average Value",
    color = "Happiness Segment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(
  "output/figures/segment_means_comparison.png",
  p_segment_means,
  width = 9,
  height = 5.5,
  dpi = 300
)

