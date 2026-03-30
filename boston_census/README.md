# Boston Demographic Analysis — Census & Neighborhood Data

## Project Description

This repository analyzes the demographic profile of Boston, Massachusetts using two complementary data sources: the U.S. Census Bureau American Community Survey (ACS) 5-Year Estimates and the City of Boston's 2025 neighborhood-level population estimates. The analysis characterizes Boston's population by race/ethnicity, age, and sex, and examines how demographic composition varies across the city's 26 neighborhoods.

This type of analysis supports epidemiologic rate calculation, health equity assessment, and the identification of populations that may be disproportionately affected by respiratory illnesses or other health conditions tracked in companion repositories (`influenza_boston`, `covid_boston`, `rsv_boston`).

---

## Data Sources

| Source | Description | URL |
|--------|-------------|-----|
| U.S. Census Bureau ACS 5-Year Estimates | Race/ethnicity, age, sex for Boston | https://api.census.gov/data/2023/acs/acs5 |
| Census Population Estimates Program (PEP) | Annual population characteristics | https://api.census.gov/data/2023/pep/charv |
| City of Boston Open Data | 2025 neighborhood-level population estimates | https://data.boston.gov/dataset/2025-boston-population-estimates-neighborhood-level |

---

## Methods Summary

1. **Census data acquisition**: `tidycensus` R package used to pull ACS 5-year estimates for Boston city (Suffolk County, MA place FIPS 07000). Variables include total population, race/ethnicity breakdown, age groups, and sex.
2. **Neighborhood data**: Boston's 2025 population estimates downloaded from Analyze Boston open data portal and processed into a clean neighborhood-level demographic table.
3. **Demographic analysis**: Population pyramids by age and sex, race/ethnicity distributions, and neighborhood-level comparisons.
4. **Visualization**: Population pyramid, race/ethnicity bar chart, and neighborhood comparison chart.

---

## Repository Structure

```
boston_census/
├── README.md
├── .gitignore
├── scripts/
│   ├── 01_census_api_pull.R        # Pull ACS data via tidycensus
│   ├── 02_boston_neighborhoods.R   # Download and process neighborhood data
│   ├── 03_demographic_analysis.R   # Population pyramid data, race/ethnicity, summaries
│   └── 04_visualization.R          # Population pyramid, race chart, neighborhood chart
├── data/                           # Raw and cleaned data (gitignored)
└── output/                         # Tables and figures (gitignored)
```

---

## How to Run

**Prerequisites**: You need a free Census API key.

1. Register at https://api.census.gov/data/key_signup.html (free, instant)
2. Set your key in your R environment:

```r
# Add to ~/.Renviron (run once):
usethis::edit_r_environ()
# Then add the line:
# CENSUS_API_KEY=your_key_here
```

3. Run scripts in order:

```r
source("scripts/01_census_api_pull.R")
source("scripts/02_boston_neighborhoods.R")
source("scripts/03_demographic_analysis.R")
source("scripts/04_visualization.R")
```

---

## Required R Packages

```r
install.packages(c(
  "tidyverse",    # dplyr, ggplot2, tidyr, readr, stringr
  "tidycensus",   # Census API wrapper
  "janitor",      # clean_names()
  "httr",         # HTTP requests (neighborhood data download)
  "readr",        # CSV reading
  "lubridate",    # Date handling
  "viridis",      # Color palettes
  "scales",       # Axis formatting
  "here"          # Relative paths
))
```

---

## Census API Key

A free Census API key is required and can be obtained at:  
**https://api.census.gov/data/key_signup.html**

The key is read from the environment variable `CENSUS_API_KEY`. Set it using:

```r
Sys.setenv(CENSUS_API_KEY = "your_key_here")
```

Or permanently via `usethis::edit_r_environ()`.

---

## Author

**Tahir Arif, MPH**  
Epidemiologist  
Date: March 2026

---

## Notes

- ACS 5-year estimates (2019–2023) reflect the 5-year average, not a single year. They are the most reliable small-area estimates available.
- Boston neighborhood boundaries used are the City of Boston Planning Department's official neighborhood definitions (26 neighborhoods).
- Race/ethnicity categories follow OMB/Census standards. Hispanic/Latino ethnicity is reported separately from racial categories.
