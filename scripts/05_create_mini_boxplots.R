library(readxl)
library(tidyverse)
library(janitor)

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

# Output folder
dir.create("output/figures/mini_boxplots", recursive = TRUE, showWarnings = FALSE)

# Variables in presentation order
plot_specs <- tribble(
  ~var_name,                      ~display_name,      ~file_name,
  "economy_gdp_per_capita",       "GDP",              "01_gdp.png",
  "health_life_expectancy",       "Health",           "02_health.png",
  "family",                       "Social Support",   "03_social_support.png",
  "freedom",                      "Freedom",          "04_freedom.png",
  "trust_government_corruption",  "Trust",            "05_trust.png",
  "generosity",                   "Generosity",       "06_generosity.png"
)

# Function to create one mini boxplot
create_mini_boxplot <- function(data, var_name, display_name, out_file) {
  p <- ggplot(
    data,
    aes(
      x = happiness_group,
      y = .data[[var_name]]
    )
  ) +
    geom_boxplot(width = 0.55, outlier.size = 1.2) +
    labs(
      title = display_name,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    # theme(
    #   plot.title = element_text(
    #     face = "bold",
    #     size = 13,
    #     hjust = 0.5
    #   ),
    #   axis.text.x = element_text(size = 9),
    #   axis.text.y = element_text(size = 8),
    #   panel.grid.major.x = element_blank(),
    #   panel.grid.minor = element_blank(),
    #   plot.margin = margin(8, 8, 8, 8)
    # )
    
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 8, 8, 8)
    )
  
  ggsave(
    filename = file.path("output/figures/mini_boxplots", out_file),
    plot = p,
    width = 3.0,
    height = 2.2,
    dpi = 300,
    bg = "white"
  )
}

# Create all six plots
pwalk(
  list(
    var_name = plot_specs$var_name,
    display_name = plot_specs$display_name,
    out_file = plot_specs$file_name
  ),
  ~ create_mini_boxplot(
    data = happiness_segmented,
    var_name = ..1,
    display_name = ..2,
    out_file = ..3
  )
)

message("Mini boxplots saved to: output/figures/mini_boxplots")
