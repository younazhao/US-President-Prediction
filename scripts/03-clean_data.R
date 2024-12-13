#### Preamble ####
# Purpose: Cleans the raw data from 538 by removing missing values
# Author: Yun Chu, Felix Li, and Wen Han Zhao 
# Date: 22 October 2024
# Contact: youna.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: raw data has been downloaded from the website
# Any other information needed? None

#### Workspace setup ####
library(readr)
library(dplyr)
library(lubridate)
library(rstanarm)
library(janitor)
library(tidyr)
library(arrow)

#### Clean data ####
data <- read_csv("data/01-raw_data/poll_raw_data.csv") |>
  clean_names()
# Filter and prepare data for Harris model
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.0,
    transparency_score >= 4,
    pollscore <= 0
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(state == "National") |>  # Remove the filter on end_date here
  drop_na(pct, end_date) |>  # Drop rows with NA in pct or end_date
  mutate(
    # Get the earliest and latest end dates
    earliest_date = as.Date("July/21/2024", format = "%B/%d/%Y"),
    # Create a new column for days after the earliest date
    days_after_earliest = as.numeric(end_date - earliest_date),
  ) |>
  filter(days_after_earliest > 0)


# Filter and prepare data for Trump model
just_trump_high_quality <- data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 2.0,
    transparency_score >= 4,
    pollscore <= 0
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(state == "National") |>  # Remove the filter on end_date here
  drop_na(pct, end_date) |>  # Drop rows with NA in pct or end_date
  mutate(
    # Get the earliest and latest end dates
    earliest_date = as.Date("July/21/2024", format = "%B/%d/%Y"),
    # Create a new column for days after the earliest date
    days_after_earliest = as.numeric(end_date - earliest_date),
  ) |>
  filter(days_after_earliest > 0)

write_parquet(just_harris_high_quality, "data/02-analysis_data/Harris.parquet")
write_parquet(just_trump_high_quality, "data/02-analysis_data/Trump.parquet")