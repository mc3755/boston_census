# =============================================================================
# Title:       03_demographic_analysis.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Demographic analysis of Boston population using Census ACS
#              and neighborhood-level data. Produces: (1) race/ethnicity
#              distribution table, (2) population pyramid data, (3) age
#              distribution summary, (4) neighborhood-level comparisons.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # dplyr, tidyr, readr, stringr, purrr
library(janitor)     # tabyl(), adorn_*
library(scales)      # Number formatting
library(here)        # Relative paths

# ---- Paths -------------------------------------------------------------------
RACE_PATH       <- here("data", "census_boston_race.csv")
AGE_SEX_PATH    <- here("data", "census_boston_age_sex.csv")
NBHD_PATH       <- here("data", "neighborhood_demographics.csv")

OUT_RACE        <- here("output", "table_race_ethnicity.csv")
OUT_AGE_SEX     <- here("output", "table_age_sex_pyramid.csv")
OUT_AGE_SUMMARY <- here("output", "table_age_summary.csv")
OUT_NBHD        <- here("output", "table_neighborhood_comparison.csv")

# ---- Load data ---------------------------------------------------------------
message("Loading Census data...")

race_df    <- read_csv(RACE_PATH,    show_col_types = FALSE)
age_sex_df <- read_csv(AGE_SEX_PATH, show_col_types = FALSE)
nbhd_df    <- read_csv(NBHD_PATH,    show_col_types = FALSE)

message("Race data:    ", nrow(race_df), " rows")
message("Age/sex data: ", nrow(age_sex_df), " rows")
message("Neighborhood data: ", nrow(nbhd_df), " rows")

# ---- Analysis 1: Race/ethnicity distribution ---------------------------------
# Create a clean race/ethnicity summary table with counts and percentages.
# Uses ACS Table B03002 data pulled in 01_census_api_pull.R.

message("\n=== Race/Ethnicity Analysis ===")

# Identify total population estimate column
total_pop <- race_df |>
  select(matches("total_pop_e")) |>
  pull(1)

# Create race/ethnicity summary
race_summary <- tibble(
  race_group       = c(
    "NH White alone",
    "NH Black or African American alone",
    "NH Asian alone",
    "NH American Indian/Alaska Native",
    "NH Native Hawaiian/Pacific Islander",
    "NH Some Other Race alone",
    "NH Two or More Races",
    "Hispanic or Latino (any race)"
  ),
  # Extract estimate columns; handle column naming variation
  estimate = c(
    race_df[["nh_white_e"]]       %||% NA_real_,
    race_df[["nh_black_e"]]       %||% NA_real_,
    race_df[["nh_asian_e"]]       %||% NA_real_,
    race_df[["nh_aian_e"]]        %||% NA_real_,
    race_df[["nh_nhpi_e"]]        %||% NA_real_,
    race_df[["nh_other_e"]]       %||% NA_real_,
    race_df[["nh_two_or_more_e"]] %||% NA_real_,
    race_df[["hispanic_latino_e"]] %||% NA_real_
  ),
  moe = c(
    race_df[["nh_white_m"]]       %||% NA_real_,
    race_df[["nh_black_m"]]       %||% NA_real_,
    race_df[["nh_asian_m"]]       %||% NA_real_,
    race_df[["nh_aian_m"]]        %||% NA_real_,
    race_df[["nh_nhpi_m"]]        %||% NA_real_,
    race_df[["nh_other_m"]]       %||% NA_real_,
    race_df[["nh_two_or_more_m"]] %||% NA_real_,
    race_df[["hispanic_latino_m"]] %||% NA_real_
  )
) |>
  mutate(
    pct_of_total   = round(estimate / total_pop * 100, 1),
    # 90% confidence interval for ACS estimates
    ci_lower       = estimate - moe,
    ci_upper       = estimate + moe
  ) |>
  arrange(desc(estimate))

# Add total row
race_with_total <- race_summary |>
  bind_rows(
    tibble(
      race_group   = "TOTAL",
      estimate     = total_pop,
      pct_of_total = 100.0
    )
  )

message("Race/ethnicity distribution:")
print(race_with_total)

write_csv(race_with_total, OUT_RACE)
message("Race/ethnicity table saved to: ", OUT_RACE)

# ---- Analysis 2: Population pyramid data -------------------------------------
# Reshape age × sex data into pyramid-ready format (wide: male/female columns,
# long: one row per age group). Males shown as negative values on pyramid charts.

message("\n=== Population Pyramid Analysis ===")

# Clean age labels into standardized age groups
clean_age_label <- function(var_label) {
  case_when(
    str_detect(var_label, "under5|under_5|0_4")  ~ "0–4",
    str_detect(var_label, "5_9")                  ~ "5–9",
    str_detect(var_label, "10_14")                ~ "10–14",
    str_detect(var_label, "15_17")                ~ "15–17",
    str_detect(var_label, "18_19")                ~ "18–19",
    str_detect(var_label, "^20$|_20$")            ~ "20",
    str_detect(var_label, "^21$|_21$")            ~ "21",
    str_detect(var_label, "22_24")                ~ "22–24",
    str_detect(var_label, "25_29")                ~ "25–29",
    str_detect(var_label, "30_34")                ~ "30–34",
    str_detect(var_label, "35_39")                ~ "35–39",
    str_detect(var_label, "40_44")                ~ "40–44",
    str_detect(var_label, "45_49")                ~ "45–49",
    str_detect(var_label, "50_54")                ~ "50–54",
    str_detect(var_label, "55_59")                ~ "55–59",
    str_detect(var_label, "60_61")                ~ "60–61",
    str_detect(var_label, "62_64")                ~ "62–64",
    str_detect(var_label, "65_66")                ~ "65–66",
    str_detect(var_label, "67_69")                ~ "67–69",
    str_detect(var_label, "70_74")                ~ "70–74",
    str_detect(var_label, "75_79")                ~ "75–79",
    str_detect(var_label, "80_84")                ~ "80–84",
    str_detect(var_label, "85_over|85_")          ~ "85+",
    TRUE                                           ~ var_label
  )
}

# Process age × sex data
pyramid_df <- age_sex_df |>
  mutate(
    age_group = clean_age_label(age_label),
    # Male estimates are positive; female estimates kept positive here,
    # then flipped to negative in visualization
    estimate  = estimate
  ) |>
  select(sex, age_group, estimate, moe) |>
  filter(!is.na(age_group))

# Define ordered age group factor for proper pyramid ordering
age_order <- c(
  "0–4", "5–9", "10–14", "15–17", "18–19", "20", "21",
  "22–24", "25–29", "30–34", "35–39", "40–44", "45–49",
  "50–54", "55–59", "60–61", "62–64", "65–66", "67–69",
  "70–74", "75–79", "80–84", "85+"
)

pyramid_df <- pyramid_df |>
  mutate(age_group = factor(age_group, levels = age_order)) |>
  arrange(age_group, sex)

message("Population pyramid data (first 10 rows):")
print(head(pyramid_df, 10))

write_csv(pyramid_df, OUT_AGE_SEX)
message("Population pyramid data saved to: ", OUT_AGE_SEX)

# ---- Analysis 3: Aggregate age summary statistics ----------------------------
message("\n=== Age Distribution Summary ===")

age_summary <- pyramid_df |>
  # Collapse detailed age groups into 4 epidemiologically meaningful categories
  mutate(
    age_cat = case_when(
      age_group %in% c("0–4", "5–9", "10–14") ~ "Children (<15)",
      age_group %in% c("15–17", "18–19", "20", "21", "22–24") ~ "Youth (15–24)",
      age_group %in% c("25–29", "30–34", "35–39", "40–44",
                       "45–49", "50–54", "55–59", "60–61", "62–64") ~ "Adults (25–64)",
      TRUE ~ "Older Adults (65+)"
    )
  ) |>
  group_by(sex, age_cat) |>
  summarise(
    estimate = sum(estimate, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  group_by(sex) |>
  mutate(pct = round(estimate / sum(estimate) * 100, 1)) |>
  ungroup() |>
  arrange(sex, age_cat)

message("Age category summary:")
print(age_summary)

write_csv(age_summary, OUT_AGE_SUMMARY)
message("Age summary saved to: ", OUT_AGE_SUMMARY)

# ---- Analysis 4: Neighborhood-level population comparison ---------------------
message("\n=== Neighborhood Analysis ===")

nbhd_analysis <- nbhd_df |>
  # Sort by population descending; add percentile ranks
  arrange(desc(total_population)) |>
  mutate(
    pop_rank       = row_number(),
    pct_of_city    = round(total_population / sum(total_population, na.rm = TRUE) * 100, 2),
    cumulative_pct = round(cumsum(pct_of_city), 1),
    pop_group      = case_when(
      total_population >= 50000 ~ "Large (≥50K)",
      total_population >= 20000 ~ "Medium (20–50K)",
      total_population >= 5000  ~ "Small (5–20K)",
      TRUE                      ~ "Micro (<5K)"
    )
  )

message("Neighborhood population distribution:")
print(nbhd_analysis |> select(neighborhood, total_population, pct_of_city, pop_group))

# Concentration: how many neighborhoods hold 50% of the population?
top_50pct <- nbhd_analysis |>
  filter(cumulative_pct <= 50) |>
  nrow()
message("\nNeighborhoods holding top 50% of population: ", top_50pct,
        " out of ", nrow(nbhd_analysis))

write_csv(nbhd_analysis, OUT_NBHD)
message("Neighborhood comparison saved to: ", OUT_NBHD)

# ---- Print summary -----------------------------------------------------------
message("\n=== KEY FINDINGS ===")
message("Boston total population (ACS 2023 5-yr): ", format(total_pop, big.mark = ","))

top_race <- race_summary |> slice_max(pct_of_total, n = 1)
message("Largest racial/ethnic group: ", top_race$race_group,
        " (", top_race$pct_of_total, "%)")

message("Neighborhoods analyzed: ", nrow(nbhd_analysis))
message("Largest neighborhood: ", nbhd_analysis$neighborhood[1],
        " (", format(nbhd_analysis$total_population[1], big.mark = ","), " residents)")
