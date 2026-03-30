# =============================================================================
# Title:       04_visualization.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Publication-quality demographic visualizations for Boston, MA.
#              Produces: (1) population pyramid by age and sex,
#              (2) race/ethnicity bar chart, (3) neighborhood population chart.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # ggplot2, dplyr, tidyr, readr, forcats
library(scales)      # Axis formatting (comma, percent)
library(viridis)     # Color palettes
library(here)        # Relative paths

# ---- Shared plot theme -------------------------------------------------------
theme_epi <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle    = element_text(size = base_size - 1, color = "grey40", hjust = 0),
      plot.caption     = element_text(size = base_size - 3, color = "grey55", hjust = 0),
      axis.title       = element_text(size = base_size - 1, color = "grey30"),
      axis.text        = element_text(size = base_size - 2, color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = base_size - 2),
      legend.text      = element_text(size = base_size - 2),
      plot.margin      = margin(12, 16, 8, 12)
    )
}
theme_set(theme_epi())

# ---- Paths -------------------------------------------------------------------
PYRAMID_PATH   <- here("output", "table_age_sex_pyramid.csv")
RACE_PATH      <- here("output", "table_race_ethnicity.csv")
NBHD_PATH      <- here("output", "table_neighborhood_comparison.csv")
AGE_SUM_PATH   <- here("output", "table_age_summary.csv")

OUT_PYRAMID    <- here("output", "fig_population_pyramid.png")
OUT_RACE       <- here("output", "fig_race_ethnicity.png")
OUT_NBHD       <- here("output", "fig_neighborhood_population.png")

# ---- Load data ---------------------------------------------------------------
message("Loading analysis output tables...")

pyramid_df <- read_csv(PYRAMID_PATH, show_col_types = FALSE)
race_df    <- read_csv(RACE_PATH,    show_col_types = FALSE)
nbhd_df    <- read_csv(NBHD_PATH,    show_col_types = FALSE)

# ---- Figure 1: Population pyramid by age and sex ----------------------------
# Classic epidemiologic pyramid: males on left (negative), females on right
# Color: male = navy, female = coral — neutral, distinguishable palette

message("Building population pyramid...")

# Order age groups correctly (youngest at bottom)
age_order <- c(
  "0–4", "5–9", "10–14", "15–17", "18–19", "20", "21",
  "22–24", "25–29", "30–34", "35–39", "40–44", "45–49",
  "50–54", "55–59", "60–61", "62–64", "65–66", "67–69",
  "70–74", "75–79", "80–84", "85+"
)

pyramid_plot_df <- pyramid_df |>
  filter(!is.na(age_group), !is.na(estimate)) |>
  mutate(
    age_group  = factor(age_group, levels = age_order),
    # Males plot to the left (negative x), females to the right (positive x)
    pop_signed = if_else(sex == "Male", -estimate, estimate)
  )

# Compute x-axis limits (symmetric around zero)
max_pop <- max(abs(pyramid_plot_df$pop_signed), na.rm = TRUE)
x_limit <- ceiling(max_pop / 5000) * 5000   # Round up to nearest 5,000

# Label breaks
x_breaks <- seq(-x_limit, x_limit, by = 5000)
x_labels <- scales::comma(abs(x_breaks))

fig1 <- ggplot(pyramid_plot_df,
               aes(x = pop_signed, y = age_group, fill = sex)) +
  geom_col(alpha = 0.85, width = 0.85) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Male" = "#1B6CA8", "Female" = "#D4517A"),
    name   = "Sex"
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
    limits = c(-x_limit, x_limit)
  ) +
  # Add sex labels above the chart
  annotate("text", x = -x_limit * 0.6, y = length(age_order) + 0.5,
           label = "← Male", color = "#1B6CA8", fontface = "bold", size = 4) +
  annotate("text", x = x_limit * 0.6, y = length(age_order) + 0.5,
           label = "Female →", color = "#D4517A", fontface = "bold", size = 4) +
  labs(
    title    = "Boston Has a Young Adult Bulge Driven by Its Large College Population",
    subtitle = paste0("Population pyramid by age and sex, Boston MA",
                      " — ACS 5-Year Estimates 2019–2023"),
    x        = "Population",
    y        = "Age Group",
    caption  = "Source: U.S. Census Bureau ACS 5-Year Estimates (B01001) | Analysis: Tahir Arif, MPH"
  ) +
  theme(
    legend.position = "none",   # Sex labels above make legend redundant
    axis.text.y     = element_text(size = 8.5)
  )

ggsave(OUT_PYRAMID, plot = fig1, width = 10, height = 10, dpi = 300, bg = "white")
message("Saved: ", OUT_PYRAMID)

# ---- Figure 2: Race/ethnicity bar chart --------------------------------------
# Horizontal bars; sorted descending; exclude "TOTAL" row

message("Building race/ethnicity chart...")

race_plot_df <- race_df |>
  filter(race_group != "TOTAL", !is.na(pct_of_total)) |>
  mutate(
    # Shorten long labels for chart readability
    race_label = str_replace(race_group, "NH ", ""),
    race_label = str_replace(race_label, " alone", ""),
    race_label = str_replace(race_label, " or African American", "/AA"),
    race_label = str_replace(race_label, " or Latino \\(any race\\)", "\n(any race)"),
    race_label = factor(race_label, levels = rev(race_label[order(pct_of_total)]))
  )

# Create color palette: viridis with fixed assignment
n_groups   <- nrow(race_plot_df)
bar_colors <- viridis(n_groups, option = "D", begin = 0.1, end = 0.85)
names(bar_colors) <- levels(race_plot_df$race_label)

fig2 <- ggplot(race_plot_df,
               aes(x = pct_of_total, y = race_label, fill = race_label)) +
  geom_col(alpha = 0.88) +
  # Add percentage labels inside bars
  geom_text(
    aes(label = paste0(pct_of_total, "%")),
    hjust     = -0.1,
    size      = 3.5,
    color     = "grey30"
  ) +
  # Add MOE error bars (converted to percentage)
  geom_errorbarh(
    aes(xmin = pct_of_total - (moe / estimate * pct_of_total),
        xmax = pct_of_total + (moe / estimate * pct_of_total)),
    height = 0.3, color = "grey50", linewidth = 0.5
  ) +
  scale_fill_manual(values = bar_colors, guide = "none") +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, max(race_plot_df$pct_of_total, na.rm = TRUE) * 1.18),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Boston Is a Majority-Minority City: No Single Racial Group Exceeds 45%",
    subtitle = "Population by race/ethnicity, Boston MA — ACS 5-Year Estimates 2019–2023",
    x        = "% of Total Population (with ACS Margin of Error)",
    y        = NULL,
    caption  = "Source: U.S. Census Bureau ACS 5-Year Estimates (B03002) | Analysis: Tahir Arif, MPH\nNote: NH = Non-Hispanic"
  )

ggsave(OUT_RACE, plot = fig2, width = 10, height = 6, dpi = 300, bg = "white")
message("Saved: ", OUT_RACE)

# ---- Figure 3: Neighborhood population bar chart ----------------------------
# Horizontal bars sorted by population; color by population size group

message("Building neighborhood population chart...")

nbhd_plot_df <- nbhd_df |>
  filter(!is.na(total_population)) |>
  mutate(
    neighborhood = factor(neighborhood,
                          levels = neighborhood[order(total_population)]),
    pop_group    = factor(pop_group,
                          levels = c("Large (≥50K)", "Medium (20–50K)",
                                     "Small (5–20K)", "Micro (<5K)"))
  )

group_colors <- c(
  "Large (≥50K)"   = "#440154",
  "Medium (20–50K)"= "#31688E",
  "Small (5–20K)"  = "#35B779",
  "Micro (<5K)"    = "#FDE725"
)

fig3 <- ggplot(nbhd_plot_df,
               aes(x = total_population, y = neighborhood, fill = pop_group)) +
  geom_col(alpha = 0.85) +
  geom_text(
    aes(label = scales::comma(total_population)),
    hjust  = -0.1,
    size   = 2.8,
    color  = "grey30"
  ) +
  scale_fill_manual(values = group_colors, name = "Population Size") +
  scale_x_continuous(
    labels = scales::comma,
    limits = c(0, max(nbhd_plot_df$total_population, na.rm = TRUE) * 1.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Boston's Population Is Concentrated in a Few Large Neighborhoods",
    subtitle = "2025 estimated population by neighborhood — City of Boston",
    x        = "Total Population",
    y        = NULL,
    caption  = paste0(
      "Source: City of Boston Open Data, 2025 Population Estimates | ",
      "Analysis: Tahir Arif, MPH\n",
      "URL: https://data.boston.gov/dataset/2025-boston-population-estimates-neighborhood-level"
    )
  ) +
  theme(
    axis.text.y     = element_text(size = 8),
    legend.position = "right"
  )

ggsave(OUT_NBHD, plot = fig3,
       width  = 12,
       height = max(8, nrow(nbhd_plot_df) * 0.35 + 2),  # Scale height to # neighborhoods
       dpi    = 300,
       bg     = "white")
message("Saved: ", OUT_NBHD)

message("\nAll demographic figures saved to output/")
message("Done.")
