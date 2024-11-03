#### Preamble ####
# Purpose: Explore the dataset 
# Author: Yun Chu, Felix Li, and Wen Han Zhao 
# Date: 4 November 2024
# License: MIT
# Pre-requisites: raw data used from data folder
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(ggplot2)
library(arrow)

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/Harris.parquet")
data <- read.csv("data/01-raw_data/raw_data.csv")

### Histogram of Polls in Raw dataset
hist(data$pct, main = "Histogram of Raw Dataset of Polls", xlab = "poll", ylab = "Frequency")

### Only keeping the rows that we want to investigate
harris <- analysis_data |> select(sample_size, pollster, numeric_grade, pollscore, methodology, transparency_score, state, start_date, 
                                  end_date, population, party, answer, candidate_name, pct)

### Summary of the selected dataset
summary(harris)

### Histogram of Polls of Selected Data
hist(harris$pct, main = "Histogram of Poll", xlab = "The pencentage of the vote for Harris in the poll", ylab = "Frequency")

### Summary statistics for pct
mean_pct <- mean(data$pct)
sd_pct <- sd(data$pct)
min_pct <- min(data$pct)
max_pct <- max(data$pct)

summary_table <- data.frame( Metric = c("Mean", "Standard Deviation", "Minimum", "Maximum"),
                             Value = c(mean_pct, sd_pct, min_pct, max_pct))

kable(summary_table, caption = "Statistics for Poll Percentage")

### Distribution of poll by pollster
pollster_selected <- data |> filter(
  numeric_grade >= 2.0,
  pollscore <= 0,
  transparency_score >= 4
)|>
  mutate(
    state = if_else(is.na(state), "National", state), 
  ) |>
  mutate(
    number_pct = round((pct / 100) * sample_size, 0)
  )|>
  group_by(pollster) |>
  filter(n() > 50)

pollster_selected$harris <- ifelse(pollster_selected$answer == "Harris", 1,
                                   ifelse(pollster_selected$answer == "Trump", 0, NA))

ggplot(pollster_selected, aes(x = reorder(pollster, -table(pollster)[pollster]))) +
  geom_bar() +
  labs(title = "Distribution of Polls by Pollster", x ="Pollster", y = "Number of Polls")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

### Distribution of poll by state
ggplot(pollster_selected, aes(x = reorder(state, -table(state)[state]))) + 
  geom_bar() + 
  labs(title = "Distribution of Polls by State", x = "State", y = "Number of Polls") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

### Plot Harris Variable 
ggplot(pollster_selected, aes(x = factor(harris))) +
  geom_bar() +
  labs(title = "Distribution of Polls by Trump vs Harris", x = "Voter for Kamala Harris", y = "Number of Polls") +
  scale_x_discrete(labels = c("1" = "Harris", "0" = "Trump"), na.translate = TRUE) +
  scale_x_discrete(labels = c("Harris", "Trump", "Others"))

