# =============================================================================
# Title:       01_census_api_pull.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Pull demographic data for Boston, MA from the U.S. Census Bureau
#              American Community Survey (ACS) 5-Year Estimates using the
#              tidycensus package. Retrieves population by race/ethnicity,
#              age group, and sex. Saves to data/census_boston.csv.
#
# PREREQUISITE: A free Census API key is required.
#   1. Register at: https://api.census.gov/data/key_signup.html
#   2. Add to your ~/.Renviron file:
#        CENSUS_API_KEY=your_key_here
#   3. Or set inline for this session:
#        Sys.setenv(CENSUS_API_KEY = "your_key_here")
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)    # dplyr, tidyr, readr, stringr
library(tidycensus)   # Census Bureau API wrapper
library(janitor)      # clean_names()
library(here)         # Relative paths

# ---- Configuration -----------------------------------------------------------

# Load API key from environment variable
# NEVER hard-code your key in a script — use environment variables
CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")

if (nchar(CENSUS_API_KEY) == 0) {
  stop(
    "Census API key not found.\n",
    "  1. Register at: https://api.census.gov/data/key_signup.html\n",
    "  2. Add to ~/.Renviron: CENSUS_API_KEY=your_key_here\n",
    "  3. Or run: Sys.setenv(CENSUS_API_KEY = 'your_key_here')\n",
    "  4. Then restart R (or re-source this script)."
  )
}

# Register key with tidycensus for this session
census_api_key(CENSUS_API_KEY, install = FALSE, overwrite = TRUE)

# Geographic identifiers for Boston, MA
# FIPS: State = 25 (Massachusetts), Place = 07000 (Boston city)
STATE_FIPS  <- "25"
PLACE_FIPS  <- "07000"

# ACS dataset: 5-year estimates (most reliable for sub-city areas)
ACS_YEAR    <- 2023
ACS_SURVEY  <- "acs5"

# ---- Define Census variables to pull -----------------------------------------
#
# We pull three sets of variables:
#   (A) Total population by race/ethnicity (Table B03002)
#   (B) Population by sex and age group (Table B01001)
#   (C) Median age and total population (Table B01002, B01003)
#
# Use load_variables(2023, "acs5") to explore all available variables.

# (A) Race/ethnicity (Hispanic/Latino origin × race, Table B03002)
race_vars <- c(
  total_pop           = "B03002_001",   # Total population
  not_hispanic        = "B03002_002",   # Not Hispanic or Latino
  nh_white            = "B03002_003",   # NH White alone
  nh_black            = "B03002_004",   # NH Black or African American alone
  nh_aian             = "B03002_005",   # NH American Indian/Alaska Native
  nh_asian            = "B03002_006",   # NH Asian alone
  nh_nhpi             = "B03002_007",   # NH Native Hawaiian/Pacific Islander
  nh_other            = "B03002_008",   # NH Some other race alone
  nh_two_or_more      = "B03002_009",   # NH Two or more races
  hispanic_latino     = "B03002_012"    # Hispanic or Latino (any race)
)

# (B) Sex × age (Table B01001) — 5-year age bands, by sex
# Male age groups: B01001_003 to B01001_025
# Female age groups: B01001_027 to B01001_049
# We pull the summary age groups for a population pyramid
age_sex_vars <- c(
  # Males
  male_under5    = "B01001_003",
  male_5_9       = "B01001_004",
  male_10_14     = "B01001_005",
  male_15_17     = "B01001_006",
  male_18_19     = "B01001_007",
  male_20        = "B01001_008",
  male_21        = "B01001_009",
  male_22_24     = "B01001_010",
  male_25_29     = "B01001_011",
  male_30_34     = "B01001_012",
  male_35_39     = "B01001_013",
  male_40_44     = "B01001_014",
  male_45_49     = "B01001_015",
  male_50_54     = "B01001_016",
  male_55_59     = "B01001_017",
  male_60_61     = "B01001_018",
  male_62_64     = "B01001_019",
  male_65_66     = "B01001_020",
  male_67_69     = "B01001_021",
  male_70_74     = "B01001_022",
  male_75_79     = "B01001_023",
  male_80_84     = "B01001_024",
  male_85_over   = "B01001_025",
  # Females
  female_under5  = "B01001_027",
  female_5_9     = "B01001_028",
  female_10_14   = "B01001_029",
  female_15_17   = "B01001_030",
  female_18_19   = "B01001_031",
  female_20      = "B01001_032",
  female_21      = "B01001_033",
  female_22_24   = "B01001_034",
  female_25_29   = "B01001_035",
  female_30_34   = "B01001_036",
  female_35_39   = "B01001_037",
  female_40_44   = "B01001_038",
  female_45_49   = "B01001_039",
  female_50_54   = "B01001_040",
  female_55_59   = "B01001_041",
  female_60_61   = "B01001_042",
  female_62_64   = "B01001_043",
  female_65_66   = "B01001_044",
  female_67_69   = "B01001_045",
  female_70_74   = "B01001_046",
  female_75_79   = "B01001_047",
  female_80_84   = "B01001_048",
  female_85_over = "B01001_049"
)

# (C) Summary statistics
summary_vars <- c(
  total_pop    = "B01003_001",   # Total population
  median_age   = "B01002_001"    # Median age
)

# ---- Pull race/ethnicity data ------------------------------------------------
message("Pulling race/ethnicity data (ACS ", ACS_YEAR, " 5-year)...")

race_df <- get_acs(
  geography = "place",
  state     = STATE_FIPS,
  variables = race_vars,
  year      = ACS_YEAR,
  survey    = ACS_SURVEY,
  output    = "wide"        # One row per geography, one column per variable
) |>
  filter(GEOID == paste0(STATE_FIPS, PLACE_FIPS)) |>
  clean_names()

message("Race/ethnicity pull complete. Rows: ", nrow(race_df))
print(race_df)

# ---- Pull age × sex data -----------------------------------------------------
message("\nPulling age × sex data...")

age_sex_df <- get_acs(
  geography = "place",
  state     = STATE_FIPS,
  variables = age_sex_vars,
  year      = ACS_YEAR,
  survey    = ACS_SURVEY,
  output    = "tidy"        # Long format: one row per variable per geography
) |>
  filter(GEOID == paste0(STATE_FIPS, PLACE_FIPS)) |>
  clean_names()

message("Age × sex pull complete. Rows: ", nrow(age_sex_df))

# Parse variable names into sex and age_group columns
age_sex_df <- age_sex_df |>
  mutate(
    # variable format: "male_under5E", "female_22_24E", etc. (after clean_names adds _e)
    variable_base = str_remove(variable, "_e$|e$"),
    sex           = if_else(str_starts(variable_base, "male"), "Male", "Female"),
    age_label     = str_remove(variable_base, "^male_|^female_")
  )

# ---- Pull summary statistics --------------------------------------------------
message("\nPulling summary statistics...")

summary_df <- get_acs(
  geography = "place",
  state     = STATE_FIPS,
  variables = summary_vars,
  year      = ACS_YEAR,
  survey    = ACS_SURVEY,
  output    = "tidy"
) |>
  filter(GEOID == paste0(STATE_FIPS, PLACE_FIPS)) |>
  clean_names()

message("Summary stats: ", summary_df$variable, " = ", summary_df$estimate)

# ---- Combine and save --------------------------------------------------------
# Save each dataset to separate CSVs for use in subsequent scripts

write_csv(race_df,    here("data", "census_boston_race.csv"))
write_csv(age_sex_df, here("data", "census_boston_age_sex.csv"))
write_csv(summary_df, here("data", "census_boston_summary.csv"))

# Also write a combined long-format file for convenience
census_combined <- bind_rows(
  age_sex_df |> mutate(table = "B01001 — Age × Sex"),
  summary_df |> mutate(table = "B01002/B01003 — Summary Stats")
)
write_csv(census_combined, here("data", "census_boston.csv"))

message("\nAll Census data saved to data/")
message("Files written:")
message("  data/census_boston_race.csv     (race/ethnicity)")
message("  data/census_boston_age_sex.csv  (age × sex)")
message("  data/census_boston_summary.csv  (totals, median age)")
message("  data/census_boston.csv          (combined long format)")
message("\nProceed with 02_boston_neighborhoods.R")
