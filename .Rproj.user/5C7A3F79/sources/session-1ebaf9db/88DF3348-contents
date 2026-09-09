# draft analyses 2.0
## by Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(here)
library(DescTools)
library(patchwork)
library(ggokabeito)
library(lme4)
library(modelr)
library(ggplot2)

## Source functions
source(
  here::here(
    'analyses/functions.R'
  )
)

## Common theme for plots
common_theme <- theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    title = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
  )

## Colors for plots ----
color_positive_high <- palette_okabe_ito(3)
color_positive_medium <- palette_okabe_ito(5)
color_negative_medium <- palette_okabe_ito(1)
color_negative_high <- palette_okabe_ito(7)

## Plot size ----
plot_height <- 4
plot_width <- 7.1

# Load data ----
## Wide data
load(
  here::here('input/cleaned_data/data_wide.rds')
)
# load(
#   here::here('input/cleaned_data/data_long.rds')
# )

# Decompose data ----
data <- data |>
  # --- L0 overall score ---
  mutate(
    practice_duration_logs_L0 = practice_duration_logs_months,
    practice_freq_logs_L0 = practice_freq_logs,
    practice_freq_survey_L0 = practice_freq_survey,
    practice_freq_error_L0 = practice_freq_survey_L0 - practice_freq_logs_L0,
    practice_length_logs_L0 = practice_length_logs,
    practice_length_survey_L0 = practice_length_survey,
    practice_length_error_L0 = practice_length_survey_L0 - practice_length_logs_L0,
  ) |> 
  # --- L3 components (school means) ---
  group_by(school_ID) |>
  mutate(
    # duration
    practice_duration_logs_L3 = mean(practice_duration_logs_months, na.rm = T),
    
    # Frequency
    practice_freq_survey_L3 = mean(practice_freq_survey, na.rm = T),
    practice_freq_logs_L3 = mean(practice_freq_logs, na.rm = T),
    practice_freq_error_L3 = practice_freq_survey_L3 - practice_freq_logs_L3,
    
    # Length
    practice_length_survey_L3 = mean(practice_length_survey, na.rm = T),
    practice_length_logs_L3 = mean(practice_length_logs, na.rm = T),
    practice_length_error_L3 = practice_length_survey_L3 - practice_length_logs_L3,
    
  ) |>
  # --- L2 components (group means school-mean centred) ---
  group_by(school_ID, group_ID) |>
  mutate(
    # Duration
    practice_duration_logs_L2 = mean(practice_duration_logs_months, na.rm = T) - practice_duration_logs_L3,
    
    # Frequency
    practice_freq_survey_L2 = mean(practice_freq_survey, na.rm = T) - practice_freq_survey_L3,
    practice_freq_logs_L2 = mean(practice_freq_logs, na.rm = T) - practice_freq_logs_L3,
    practice_freq_error_L2 = practice_freq_survey_L2 - practice_freq_logs_L2,
    
    # Length
    practice_length_survey_L2 = mean(practice_length_survey, na.rm = T) - practice_length_survey_L3,
    practice_length_logs_L2 = mean(practice_length_logs, na.rm = T) - practice_length_logs_L3,
    practice_length_error_L2 = practice_length_survey_L2 - practice_length_logs_L2,
    
    # --- L1 components (within-group deviations, student scores group mean centered) ---
    # Duration
    practice_duration_logs_L1 = practice_duration_logs_months - mean(practice_duration_logs_months, na.rm = T),
    
    # Frequency
    practice_freq_survey_L1 = practice_freq_survey - mean(practice_freq_survey, na.rm = T),
    practice_freq_logs_L1 = practice_freq_logs - mean(practice_freq_logs, na.rm = T),
    practice_freq_error_L1 = practice_freq_survey_L1 - practice_freq_logs_L1,
    
    # Length
    practice_length_survey_L1 = practice_length_survey - mean(practice_length_survey, na.rm = T),
    practice_length_logs_L1 = practice_length_logs - mean(practice_length_logs, na.rm = T),
    practice_length_error_L1 = practice_length_survey_L1 - practice_length_logs_L1,
  ) |>
  ungroup()


# Equivalence (Blandt altman plots) ----
## Length ----
### L0
blandtaltman_length_L0 <- data |> 
  ggplot(
    aes(
      x = practice_length_logs_L0,
      y = practice_length_error_L0
    )
  ) +
  geom_point() +
  geom_hline(
    yintercept = mean(data$practice_length_error_L0),
    color = 'blue'
  ) +
  geom_hline(
    yintercept = mean(data$practice_length_error_L0) + (1.96*sd(data$practice_length_error_L0)),
    color = 'red'
  ) +
  geom_hline(
    yintercept = mean(data$practice_length_error_L0) - (1.96*sd(data$practice_length_error_L0)),
    color = 'red'
  ) +
  coord_cartesian(xlim = c(-20, 20), ylim = c(-15, 15))

## L1
blandtaltman_length_L1 <- data |> 
  ggplot(
    aes(
      x = practice_length_logs_L1,
      y = practice_length_error_L1
    )
  ) +
  geom_point() +
  geom_hline(
    yintercept = mean(data$practice_length_error_L1),
    color = 'blue'
  ) +
  geom_hline(
    yintercept = mean(data$practice_length_error_L1) + (1.96*sd(data$practice_length_error_L1)),
    color = 'red'
  ) +
  geom_hline(
    yintercept = mean(data$practice_length_error_L1) - (1.96*sd(data$practice_length_error_L1)),
    color = 'red'
  ) +
  coord_cartesian(xlim = c(-20, 20), ylim = c(-15, 15))

## L2
blandtaltman_length_L2 <- data |> 
  ggplot(
    aes(
      x = practice_length_logs_L2,
      y = practice_length_error_L2
    )
  ) +
  geom_point() +
  geom_hline(
    yintercept = mean(data$practice_length_error_L2),
    color = 'blue'
  ) +
  geom_hline(
    yintercept = mean(data$practice_length_error_L2) + (1.96*sd(data$practice_length_error_L2)),
    color = 'red'
  ) +
  geom_hline(
    yintercept = mean(data$practice_length_error_L2) - (1.96*sd(data$practice_length_error_L2)),
    color = 'red'
  ) +
  coord_cartesian(xlim = c(-20, 20), ylim = c(-15, 15))

## L3
blandtaltman_length_L3 <- data |> 
  ggplot(
    aes(
      x = practice_length_logs_L3,
      y = practice_length_error_L3
    )
  ) +
  geom_point() +
  geom_hline(
    yintercept = mean(data$practice_length_error_L3),
    color = 'blue'
  ) +
  geom_hline(
    yintercept = mean(data$practice_length_error_L3) + (1.96*sd(data$practice_length_error_L3)),
    color = 'red'
  ) +
  geom_hline(
    yintercept = mean(data$practice_length_error_L3) - (1.96*sd(data$practice_length_error_L3)),
    color = 'red'
  ) +
  coord_cartesian(xlim = c(-20, 20), ylim = c(-15, 15))

### Combine plots
blandtaltman_length <- blandtaltman_length_L0 +
  blandtaltman_length_L1 +
  blandtaltman_length_L2 +
  blandtaltman_length_L3 +
  plot_layout(ncol = 4)

## Frequency ----
### L0
blandtaltman_freq_L0 <- data |> 
  ggplot(
    aes(
      x = practice_freq_logs_L0,
      y = practice_freq_error_L0
    )
  ) +
  geom_point() +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L0),
    color = 'blue'
  ) +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L0) + (1.96*sd(data$practice_freq_error_L0)),
    color = 'red'
  ) +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L0) - (1.96*sd(data$practice_freq_error_L0)),
    color = 'red'
  ) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))

## L1
blandtaltman_freq_L1 <- data |> 
  ggplot(
    aes(
      x = practice_freq_logs_L1,
      y = practice_freq_error_L1
    )
  ) +
  geom_point() +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L1),
    color = 'blue'
  ) +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L1) + (1.96*sd(data$practice_freq_error_L1)),
    color = 'red'
  ) +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L1) - (1.96*sd(data$practice_freq_error_L1)),
    color = 'red'
  ) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))

## L2
blandtaltman_freq_L2 <- data |> 
  ggplot(
    aes(
      x = practice_freq_logs_L2,
      y = practice_freq_error_L2
    )
  ) +
  geom_point() +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L2),
    color = 'blue'
  ) +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L2) + (1.96*sd(data$practice_freq_error_L2)),
    color = 'red'
  ) +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L2) - (1.96*sd(data$practice_freq_error_L2)),
    color = 'red'
  ) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))

## L3
blandtaltman_freq_L3 <- data |> 
  ggplot(
    aes(
      x = practice_freq_logs_L3,
      y = practice_freq_error_L3
    )
  ) +
  geom_point() +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L3),
    color = 'blue'
  ) +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L3) + (1.96*sd(data$practice_freq_error_L3)),
    color = 'red'
  ) +
  geom_hline(
    yintercept = mean(data$practice_freq_error_L3) - (1.96*sd(data$practice_freq_error_L3)),
    color = 'red'
  ) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))

### Combine plots
blandtaltman_freq <- blandtaltman_freq_L0 +
  blandtaltman_freq_L1 +
  blandtaltman_freq_L2 +
  blandtaltman_freq_L3 +
  plot_layout(ncol = 4)

wrap_elements(blandtaltman_length) + wrap_elements(blandtaltman_freq) +
  plot_layout(nrow = 2)




# convergent validity
