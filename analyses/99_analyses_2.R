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

# Reliability ----
reliability_L3 <- data |> 
  group_by(
    school_ID
  ) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  dplyr::select(
    contains('L3')
  ) |> 
  pivot_longer(
    contains('L3'),
    names_prefix = "practice_",
    names_sep = "_",
    names_to = c('dimension', 'source', 'level'),
    values_to = 'score'
  ) |> 
  group_by(
    dimension, source
  ) |> 
  summarise(
    variance = var(score)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = source,
    values_from = variance,
    names_prefix = "variance_"
  ) |> 
  mutate(
    reliability = variance_logs / (variance_logs + variance_error),
    level = 'school'
  )

reliability_L2 <- data |> 
  group_by(
    group_ID
  ) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  dplyr::select(
    contains('L2')
  ) |> 
  pivot_longer(
    contains('L2'),
    names_prefix = "practice_",
    names_sep = "_",
    names_to = c('dimension', 'source', 'level'),
    values_to = 'score'
  ) |> 
  group_by(
    dimension, source
  ) |> 
  summarise(
    variance = var(score)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = source,
    values_from = variance,
    names_prefix = "variance_"
  ) |> 
  mutate(
    reliability = variance_logs / (variance_logs + variance_error),
    level = 'group'
  )

reliability_L1 <- data |> 
  group_by(
    school_ID
  ) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  dplyr::select(
    contains('L1')
  ) |> 
  pivot_longer(
    contains('L1'),
    names_prefix = "practice_",
    names_sep = "_",
    names_to = c('dimension', 'source', 'level'),
    values_to = 'score'
  ) |> 
  group_by(
    dimension, source
  ) |> 
  summarise(
    variance = var(score)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = source,
    values_from = variance,
    names_prefix = "variance_"
  ) |> 
  mutate(
    reliability = variance_logs / (variance_logs + variance_error),
    level = 'student'
  )

reliability_L0 <- data |> 
  # group_by(
  #   school_ID
  # ) |> 
  # slice_head(n = 1) |> 
  # ungroup() |> 
  dplyr::select(
    contains('L0')
  ) |> 
  pivot_longer(
    contains('L0'),
    names_prefix = "practice_",
    names_sep = "_",
    names_to = c('dimension', 'source', 'level'),
    values_to = 'score'
  ) |> 
  group_by(
    dimension, source
  ) |> 
  summarise(
    variance = var(score)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = source,
    values_from = variance,
    names_prefix = "variance_"
  ) |> 
  mutate(
    reliability = variance_logs / (variance_logs + variance_error),
    level = 'overall'
  )

reliability <- bind_rows(
  reliability_L0,
  reliability_L1,
  reliability_L2,
  reliability_L3,
) |> 
  mutate(
    perc_true = reliability,
    perc_error = 1 - reliability,
    Level = as_factor(level) |> fct_relevel('overall', 'school', 'group', 'student')
  ) |> 
  pivot_longer(
    contains('perc'),
    names_to = 'Variance source',
    values_to = 'Percentage'
  ) |> 
  mutate(
    `Variance source` = case_when(
      `Variance source` == 'perc_true' ~ 'Variance attributed to true score',
      `Variance source` == 'perc_error' ~ 'Variance attributed to measurement error',
    ) |> as_factor() |> fct_relevel(
      'Variance attributed to measurement error',
      'Variance attributed to true score', 
      )
  ) |> 
  arrange(
    dimension,
    Level,
    `Variance source`
  ) 

reliability

## Plot ----
reliability |> 
  ggplot(
    aes(
      x = Level,
      y = Percentage,
      fill = `Variance source`
    )
  ) +
  geom_bar(
    position = 'stack', stat = 'identity'
  ) +
  facet_grid(
    ~dimension
  ) +
  scale_fill_manual(
    values = c(
      'Variance attributed to true score' = 'green',
      'Variance attributed to measurement error' = 'red'
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1), 
    expand = expansion(0),
    labels = scales::percent
  ) +
  ylab('Reliability') +
  xlab('Level') +
  common_theme

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
