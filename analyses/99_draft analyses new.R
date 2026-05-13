## 99 draft analyses new
## by Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(here)
library(DescTools)
library(plyr)
library(patchwork)
library(ggokabeito)

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

## Colors for plots
color_positive_high <- palette_okabe_ito(3)
color_positive_medium <- palette_okabe_ito(5)
color_negative_medium <- palette_okabe_ito(1)
color_negative_high <- palette_okabe_ito(7)

# Load data ----
## Wide data
load(
  here::here('input/cleaned_data/data_wide.rds')
)

# Participant descriptives ----
## Distribution of participants ----
### Number of students
nrow(data)

### Number of students per school
number_of_students_per_school <- data |> 
  group_by(
    school_ID
  ) |> 
  summarise(
    n_students =  n()
  ) |> 
  ungroup() |> 
  summarise(
    Mean = mean(n_students, na.rm = T),
    SD = sd(n_students, na.rm = T),
    Median = median(n_students, na.rm = T),
    Min = min(n_students, na.rm = T),
    Max = max(n_students, na.rm = T) 
  ) |> 
  mutate(
    `Leveled structure` = 'Students per school'
  ) |> 
  relocate(
    `Leveled structure`
  )

### Number of teachers
unique(data$group_ID) |> length()

### Number of teachers per school
number_of_teachers_per_school <- data |>
  dplyr::select( # select only group_ID and school_ID
    group_ID,
    school_ID
  ) |> 
  distinct() |> # removes duplicates, keeps only one row per teacher
  group_by(
    school_ID
  ) |> 
  summarise(
    n_teachers =  n()
  ) |> 
  ungroup() |> 
  summarise(
    Mean = mean(n_teachers, na.rm = T),
    SD = sd(n_teachers, na.rm = T),
    Median = median(n_teachers, na.rm = T),
    Min = min(n_teachers, na.rm = T),
    Max = max(n_teachers, na.rm = T) 
  ) |> 
  mutate(
    `Leveled structure` = 'Teachers per school'
  ) |> 
  relocate(
    `Leveled structure`
  )

### Number of students per teacher 
number_of_students_per_teacher <- data |>
  group_by(
    group_ID
  ) |>
  summarise(
    n_students = n()
  ) |>
  ungroup() |>
  summarise(
    Mean = mean(n_students, na.rm = T),
    SD = sd(n_students, na.rm = T),
    Median = median(n_students, na.rm = T),
    Min = min(n_students, na.rm = T),
    Max = max(n_students, na.rm = T)
  ) |> 
  mutate(
    `Leveled structure` = 'Students per teacher'
  ) |> 
  relocate(
    `Leveled structure`
  )

### Number of schools
unique(data$school_ID) |> length()

## Description of student characteristics
data |> 
  summarise(
    Mean_years = floor(mean(age)),
    Mean_months = round((mean(age)-8)*12),
    SD_monhts = round(sd(age)*12)
  )

# Transform durationvariables ----
data <- data |> 
  mutate(
    practice_duration_survey = practice_duration_survey |> 
      fct_relevel(
        'No practice', 'Less than 1 month', '1-2 months', '3-4 months', 'Complete period'
        ),
    practice_duration_survey_ranked = case_when(
      practice_duration_survey == 'No practice' ~ 1,
      practice_duration_survey == 'Less than 1 month' ~ 2,
      practice_duration_survey == '1-2 months' ~ 3,
      practice_duration_survey == '3-4 months' ~ 4,
      practice_duration_survey == 'Complete period' ~ 5,
    )
  )

# Decompose data ----
data_decomposed <- data |>
  # --- L3 components (school means) ---
  group_by(school_ID) |>
  mutate(
    # duration
    practice_duration_logs_months_L3 = mean(practice_duration_logs_months, na.rm = T),
    
    # Frequency
    practice_freq_survey_L3 = mean(practice_freq_survey, na.rm = T),
    practice_freq_logs_L3 = mean(practice_freq_logs, na.rm = T),
    
    # Length
    practice_length_survey_L3 = mean(practice_length_survey, na.rm = T),
    practice_length_logs_L3 = mean(practice_length_logs, na.rm = T),
    
  ) |>
  # --- L2 components (group means school-mean centred) ---
  group_by(school_ID, group_ID) |>
  mutate(
    # Duration
    practice_duration_logs_months_L2 = mean(practice_duration_logs_months, na.rm = T) - practice_duration_logs_months_L3,
    
    # Frequency
    practice_freq_survey_L2 = mean(practice_freq_survey, na.rm = T) - practice_freq_survey_L3,
    practice_freq_logs_L2 = mean(practice_freq_logs, na.rm = T) - practice_freq_logs_L3,
    
    # Length
    practice_length_survey_L2 = mean(practice_length_survey, na.rm = T) - practice_length_survey_L3,
    practice_length_logs_L2 = mean(practice_length_logs, na.rm = T) - practice_length_logs_L3,
    
    # --- L1 components (within-group deviations, student scores group mean centered) ---
    # Duration
    practice_duration_logs_months_L1 = practice_duration_logs_months - mean(practice_duration_logs_months, na.rm = T),
    
    # Frequency
    practice_freq_survey_L1 = practice_freq_survey - mean(practice_freq_survey, na.rm = T),
    practice_freq_logs_L1 = practice_freq_logs - mean(practice_freq_logs, na.rm = T),
    
    # Length
    practice_length_survey_L1 = practice_length_survey - mean(practice_length_survey, na.rm = T),
    practice_length_logs_L1 = practice_length_logs - mean(practice_length_logs, na.rm = T),
  ) |>
  ungroup()
 
# Correlation analysis ----
# Duration
cor_dur_overall <- KendallTauB(
  x = data_decomposed |> pull(practice_duration_logs_months),
  y = data_decomposed |> pull(practice_duration_survey_ranked),
  conf.level = 0.95
  )

## Frequency
# Overall
cor_freq_overall <- cor.test(
  x = data_decomposed |> pull(practice_freq_logs),
  y = data_decomposed |> pull(practice_freq_survey),
  method = "pearson"
)

# Freq L1
cor_freq_L1 <- cor.test(
  x = data_decomposed |> pull(practice_freq_logs_L1),
  y = data_decomposed |> pull(practice_freq_survey_L1),
  method = "pearson"
)

# freq L2
cor_freq_L2 <- cor.test(
  x = data_decomposed |> 
    distinct(group_ID, .keep_all = T) |> 
    pull(practice_freq_logs_L2),
  y = data_decomposed |> 
    distinct(group_ID, .keep_all = T) |> 
    pull(practice_freq_survey_L2),
  method = "pearson"
)

# freq L3
cor_freq_L3 <- cor.test(
  x = data_decomposed |> 
    distinct(school_ID, .keep_all = T) |> 
    pull(practice_freq_logs_L3),
  y = data_decomposed |> 
    distinct(school_ID, .keep_all = T) |> 
    pull(practice_freq_survey_L3),
  method = "pearson"
)

## Length
# Overall
cor_length_overall <- cor.test(
  x = data_decomposed |> pull(practice_length_logs),
  y = data_decomposed |> pull(practice_length_survey),
  method = "pearson"
)

# Length L1
cor_length_L1 <- cor.test(
  x = data_decomposed |> pull(practice_length_logs_L1),
  y = data_decomposed |> pull(practice_length_survey_L1),
  method = "pearson"
)

# Length L2
cor_length_L2 <- cor.test(
  x = data_decomposed |> 
    distinct(group_ID, .keep_all = T) |> 
    pull(practice_length_logs_L2),
  y = data_decomposed |> 
    distinct(group_ID, .keep_all = T) |> 
    pull(practice_length_survey_L2),
  method = "pearson"
)

# Length L3
cor_length_L3 <- cor.test(
  x = data_decomposed |> 
    distinct(school_ID, .keep_all = T) |> 
    pull(practice_length_logs_L3),
  y = data_decomposed |> 
    distinct(school_ID, .keep_all = T) |> 
    pull(practice_length_survey_L3),
  method = "pearson"
)

## Bundle and inspect data
### Initialize tibble to hold data
cor_all <- tibble(
  variable = '',
  level = '',
  estimate = 0,
  pvalue = 0,
  ci_lower = 0,
  ci_upper = 0,
  .rows = 0
)

### Fill tibble
cor_all <- cor_all |> 
  add_row(
    variable = 'Duration',
    level = 'Overall',
    estimate = cor_dur_overall['tau_b'],
    ci_lower = cor_dur_overall['lwr.ci'],
    ci_upper = cor_dur_overall['upr.ci'],
  ) |> 
  add_row(
    variable = 'Duration',
    level = 'School',
    ) |> 
  add_row(
    variable = 'Duration',
    level = 'Teacher',
  ) |> 
  add_row(
    variable = 'Duration',
    level = 'Student',
  ) |> 
  add_row(
    variable = 'Frequency',
    level = 'Overall',
    estimate = cor_freq_overall$estimate,
    ci_lower = cor_freq_overall$conf.int[1],
    ci_upper = cor_freq_overall$conf.int[2],
  ) |> 
  add_row(
    variable = 'Frequency',
    level = 'School',
    estimate = cor_freq_L3$estimate,
    ci_lower = cor_freq_L3$conf.int[1],
    ci_upper = cor_freq_L3$conf.int[2],
  ) |> 
  add_row(
    variable = 'Frequency',
    level = 'Teacher',
    estimate = cor_freq_L2$estimate,
    ci_lower = cor_freq_L2$conf.int[1],
    ci_upper = cor_freq_L2$conf.int[2],
  ) |> 
  add_row(
    variable = 'Frequency',
    level = 'Student',
    estimate = cor_freq_L1$estimate,
    ci_lower = cor_freq_L1$conf.int[1],
    ci_upper = cor_freq_L1$conf.int[2],
  ) |> 
  add_row(
    variable = 'Length',
    level = 'Overall',
    estimate = cor_length_overall$estimate,
    ci_lower = cor_length_overall$conf.int[1],
    ci_upper = cor_length_overall$conf.int[2],
  ) |> 
  add_row(
    variable = 'Length',
    level = 'School',
    estimate = cor_length_L3$estimate,
    ci_lower = cor_length_L3$conf.int[1],
    ci_upper = cor_length_L3$conf.int[2],
  ) |> 
  add_row(
    variable = 'Length',
    level = 'Teacher',
    estimate = cor_length_L2$estimate,
    ci_lower = cor_length_L2$conf.int[1],
    ci_upper = cor_length_L2$conf.int[2],
  ) |> 
  add_row(
    variable = 'Length',
    level = 'Student',
    estimate = cor_length_L1$estimate,
    ci_lower = cor_length_L1$conf.int[1],
    ci_upper = cor_length_L1$conf.int[2],
  )

## Relevel factor for plotting purposes
cor_all <- cor_all |> 
  mutate(
    level = as_factor(level) |> 
      fct_relevel(
        'Overall', 'School', 'Teacher', 'Student'
      ),
    variable = as_factor(variable) |> 
      fct_relevel(
        'Duration', 'Frequency', 'Length'
      )
  )

## Plot ----
### Initialize correlation regens
cor_regions <- tibble(
  xmin = rep(-Inf, 4),
  xmax = rep(Inf, 4),
  ymin = c(0.90, 0.75, 0.5, 0),
  ymax = c(Inf, 0.90, 0.75, 0.5),
  fill = c(color_positive_high, 
           color_positive_medium, 
           color_negative_medium,
           color_negative_high
           ),
  labels = c('Excelent', 'Good', 'Moderate', 'Poor'),
  alpha = rep(0.3, 4)
)

### Plot correlations
#### Duration
p_cor_dur <- cor_all |> 
  filter(
    variable == 'Duration'
  ) |> 
  # Plotting
  ggplot(
    aes(x = level,
        y = estimate)
  ) +
  geom_rect(
    data = cor_regions,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
      alpha = alpha,
    ),
    inherit.aes = FALSE
  ) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = ci_lower, 
      ymax = ci_upper
    ),
    width = 0.2
  ) + 
  # Visuals
  scale_fill_identity(
    guide  = guide_legend(
      title         = "Reliability",
      override.aes  = list(alpha = 0.3)
    ),
    name   = "Reliability",
    breaks = cor_regions$fill,
    labels = cor_regions$labels,
  ) +
  scale_alpha_identity() +
  scale_y_continuous(
    limits = c(0, 1), 
    expand = expansion(0)
  ) +
  ggtitle('Duration') +
  ylab('Proportion of variance') +
  xlab('Level') +
  common_theme

### Frequency
p_cor_freq <- cor_all |> 
  filter(
    variable == 'Frequency'
  ) |> 
  # Plotting
  ggplot(
    aes(x = level,
        y = estimate)
  ) +
  geom_rect(
    data = cor_regions,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
      alpha = alpha,
    ),
    inherit.aes = FALSE
  ) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = ci_lower, 
      ymax = ci_upper
    ),
    width = 0.2
  ) + 
  # Visuals
  scale_fill_identity(
    guide  = guide_legend(
      title         = "Reliability",
      override.aes  = list(alpha = 0.3)
    ),
    name   = "Reliability",
    breaks = cor_regions$fill,
    labels = cor_regions$labels,
  ) +
  scale_alpha_identity() +
  scale_y_continuous(
    limits = c(0, 1), 
    expand = expansion(0)
  ) +
  ggtitle('Session Frequency') +
  ylab('Proportion of variance') +
  xlab('Level') +
  common_theme

### Length
p_cor_length <- cor_all |> 
  filter(
    variable == 'Length'
  ) |> 
  # Plotting
  ggplot(
    aes(x = level,
        y = estimate)
  ) +
  geom_rect(
    data = cor_regions,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
      alpha = alpha,
    ),
    inherit.aes = FALSE
  ) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = ci_lower, 
      ymax = ci_upper
    ),
    width = 0.2
  ) + 
  # Visuals
  scale_fill_identity(
    guide  = guide_legend(
      title         = "Reliability",
      override.aes  = list(alpha = 0.3)
    ),
    name   = "Reliability",
    breaks = cor_regions$fill,
    labels = cor_regions$labels,
  ) +
  scale_alpha_identity() +
  scale_y_continuous(
    limits = c(0, 1), 
    expand = expansion(0)
  ) +
  ggtitle('Session Length') +
  ylab('Proportion of variance') +
  xlab('Level') +
  common_theme

### Combine plots
p_cor_all <- 
  p_cor_dur +
  p_cor_freq +
  p_cor_length +
  plot_layout(
    axis_titles = "collect",
    guides = "collect")

p_cor_all
ggsave(
  filename = here::here('output/manuscript_tables_figures/plot_reliability.png'),
  dpi = 600,
  width = 7.1,
  height = 6
)

# Variance decomposition ----
## Duration ----
ML_dur_logs <- lmer(
  practice_duration_logs_months ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

vars_dur <- decompose_variance(
  model_logs = ML_dur_logs,
  # model_survey = ML_length_survey,
  measure = 'Duration'
)

## Frequency ----
ML_freq_logs <- lmer(
  practice_freq_logs ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

ML_freq_survey <- lmer(
  practice_freq_survey ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

vars_freq <- decompose_variance(
  model_logs = ML_freq_logs,
  model_survey = ML_freq_survey,
  measure = 'Frequency'
)

## Length ----
ML_length_logs <- lmer(
  practice_length_logs ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

ML_length_survey <- lmer(
  practice_length_survey ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

vars_length <- decompose_variance(
  model_logs = ML_length_logs,
  model_survey = ML_length_survey,
  measure = 'Length'
)

## Plot data ----
### Combine tibbles
vars_all <- vars_dur |> 
  bind_rows(
    vars_freq,
    vars_length
  ) |> 
  mutate(
    Level = case_when(
      Group == 'school_ID' ~ 'School',
      Group == 'school_ID:group_ID' ~  'Teacher',
      Group == 'Residual' ~ 'Student'
    ) |> as_factor() |> relevel('School', 'Teacher', 'Student'),
    Report = case_when(
      Model == 'logs' ~ 'Practice Log',
      Model == 'survey' ~ 'Teacher Report'
    ) |> as_factor() |> relevel('Practice Log', 'Teacher Report')
  ) |> 
  complete(
    Measure, Level, Report
  )

### Plot 
#### Duration
p_var_dur <- vars_all |> 
  # Filter
  filter(
    Measure == 'Duration'
  ) |> 
  # Plotting
  ggplot(
    aes(
      x = Report,
      y = ICC,
      fill = Level
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  # Visuals
  scale_y_continuous(
    limits = c(0, 1), 
    expand = expansion(0)
  ) +
  scale_fill_manual(
    values = c("School" = color_positive_high,
               "Teacher" = color_positive_medium,
               "Student" = color_negative_high
    )
  ) +
  ggtitle('Duration') +
  ylab('Proportion of variance') +
  xlab('') +
  common_theme
  
#### Frequency
p_var_freq <- vars_all |> 
  # Filter
  filter(
    Measure == 'Frequency'
  ) |> 
  # Plotting
  ggplot(
    aes(
      x = Report,
      y = ICC,
      fill = Level
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  # Visuals
  scale_y_continuous(
    limits = c(0, 1), 
    expand = expansion(0)
  ) +
  scale_fill_manual(
    values = c("School" = color_positive_high,
               "Teacher" = color_positive_medium,
               "Student" = color_negative_high
    )
  ) +
  ggtitle('Frequency') +
  ylab('Proportion of variance') +
  xlab('') +
  common_theme
  
#### Length
p_var_length <- vars_all |> 
  # Filter
  filter(
    Measure == 'Length'
  ) |> 
  # Plotting
  ggplot(
    aes(
      x = Report,
      y = ICC,
      fill = Level
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  # Visuals
  scale_y_continuous(
    limits = c(0, 1), 
    expand = expansion(0)
  ) +
  scale_fill_manual(
    values = c("School" = color_positive_high,
               "Teacher" = color_positive_medium,
               "Student" = color_negative_high
    )
  ) +
  ggtitle('Length') +
  ylab('Proportion of variance') +
  xlab('') +
  common_theme
  
#### Combine plots
p_var_all <- 
  p_var_dur +
  p_var_freq +
  p_var_length +
  plot_layout(
    axis_titles = "collect",
    guides = "collect")

p_var_all
ggsave(
  filename = here::here('output/manuscript_tables_figures/plot_vars.png'),
  dpi = 600,
  width = 7.1,
  height = 6
)

# Accuracy analysis ----
## Duration
summary(data$practice_duration_survey)
mean(data$practice_duration_logs_months)

## Frequency ----
t.test(
  data$practice_freq_logs,
  data$practice_freq_survey
)

## Length ----
t.test(
  data$practice_length_logs,
  data$practice_length_survey
)

## Calculate estimation error ----
error_margin <- 1
data <- data |> 
  mutate(
    # Duration
    practice_duration_logs_ranked = case_when(
      practice_duration_logs_months == 0 ~ 1,
      (practice_duration_logs_months > 0) & (practice_duration_logs_months < 0.5) ~ 2,
      (practice_duration_logs_months >= 0.5) & (practice_duration_logs_months < 2.5) ~ 3,
      (practice_duration_logs_months >= 2.5) & (practice_duration_logs_months < 4.5) ~ 4,
      (practice_duration_logs_months >= 4.5) ~ 5
      ),
    duration_error = practice_duration_survey_ranked - practice_duration_logs_ranked,
    duration_estimate = case_when(
      duration_error > 0 ~ 'Overestimate',
      duration_error < 0 ~ 'Underestimate',
      duration_error == 0 ~ 'Correct Estimate'
      ),
    
    # Frequency
    freq_error = practice_freq_survey - practice_freq_logs,
    freq_estimate = case_when(
      freq_error > error_margin*sd(practice_freq_logs) ~ 'Overestimate',
      freq_error < -error_margin*sd(practice_freq_logs) ~ 'Underestimate',
      abs(freq_error) < error_margin*sd(practice_freq_logs) ~ 'Correct Estimate',
      ),
    
    # Length
    length_error = practice_length_survey - practice_length_logs,
    length_estimate = case_when(
      length_error > error_margin*sd(practice_length_logs) ~ 'Overestimate',
      length_error < -error_margin*sd(practice_length_logs) ~ 'Underestimate',
      abs(length_error) < error_margin*sd(practice_length_logs) ~ 'Correct Estimate',
    )
  )


data |> 
  filter(
    practice_duration_survey == 'No practice',
    duration_estimate == 'Underestimate'
  ) |> 
  dplyr::select(contains('duration')) |> glimpse()




## Make plotting polygrams for confidence intervals ----
#### General parameters
coord_start = -5
coord_end = 30

#### Duration
poly_dur <- tibble(
  x = c(
    coord_start,
    coord_end,
    coord_end,
    coord_start
  ),
  y = c(
    coord_start + error_margin*sd(data$practice_duration_logs_months),
    coord_end + error_margin*sd(data$practice_duration_logs_months),
    coord_end - error_margin*sd(data$practice_duration_logs_months),
    coord_start - error_margin*sd(data$practice_duration_logs_months)
  )
)

#### Frequency
poly_freq <- tibble(
  x = c(
    coord_start,
    coord_end,
    coord_end,
    coord_start
  ),
  y = c(
    coord_start + error_margin*sd(data$practice_freq_logs),
    coord_end + error_margin*sd(data$practice_freq_logs),
    coord_end - error_margin*sd(data$practice_freq_logs),
    coord_start - error_margin*sd(data$practice_freq_logs)
  )
)

#### Length
poly_length <- tibble(
  x = c(
    coord_start,
    coord_end,
    coord_end,
    coord_start
  ),
  y = c(
    coord_start + error_margin*sd(data$practice_length_logs),
    coord_end + error_margin*sd(data$practice_length_logs),
    coord_end - error_margin*sd(data$practice_length_logs),
    coord_start - error_margin*sd(data$practice_length_logs)
  )
)

## Plot dotplots ----
### Visual parameters
area_max <- 10
area_breaks <- c(1, 10, 20, 50, 100)
area_limits <- c(1, 150)
legend_title <- 'N Students'

## Plot duration ----
p_scatter_dur <- data |> 
  # Rounding
  mutate(
    practice_duration_logs_months = round_any(
      practice_duration_logs_months, 
      1
      ),
    Accuracy = duration_estimate
  ) |> 
  # Plotting
  ggplot(
    aes(
      x = practice_duration_logs_months,
      y = practice_duration_survey,
      color = Accuracy
    )
  ) +
  geom_count() +
  # Visuals
  scale_size_area(
    max_size = area_max,
    breaks = area_breaks,
    limits = area_limits,
    name = legend_title
  ) +
  coord_cartesian(
    xlim = c(0, 5), 
    ylim = c(0, 5)
    ) +
  scale_color_manual(
    values = c(
      "Overestimate" = color_negative_high,
      "Underestimate" = color_negative_medium,
      "Correct Estimate" = color_positive_high
    )
  ) +
  ggtitle('Duration (months)') +
  ylab('Teacher Report') +
  xlab('Practice Log') +
  common_theme

## Plot frequency ----
p_scatter_freq <- data |> 
  # Rounding
  mutate(
    practice_freq_logs = round_any(
      practice_freq_logs,
      0.25
    ),
    Accuracy = freq_estimate
  ) |> 
  # Plotting
  ggplot(
    aes(
      x = practice_freq_logs,
      y = practice_freq_survey,
      color = Accuracy
    )
  ) +
  geom_polygon(
    data = poly_freq, 
    aes(x = x, y = y),
    fill = "lightgrey", 
    alpha = 0.4,
    inherit.aes = FALSE
  ) +
  geom_count() +
  # Visuals
  scale_size_area(
    max_size = area_max,
    breaks = area_breaks,
    limits = area_limits,
    name = legend_title
  ) +
  coord_cartesian(
    xlim = c(0, 4), 
    ylim = c(0, 4)
    ) +
  scale_color_manual(
    values = c(
      "Overestimate" = color_negative_high,
      "Underestimate" = color_negative_medium,
      "Correct Estimate" = color_positive_high
    )
  ) +
  ggtitle('Frequency (sessions per week)') +
  ylab('Teacher Report') +
  xlab('Practice Log') +
  common_theme

## Plot length ----
p_scatter_length <- data |> 
  # Rounding
  mutate(
    practice_length_logs = round_any(
      practice_length_logs,
      1
    ),
    Accuracy = length_estimate
  ) |> 
  # Plotting
  ggplot(
    aes(
      x = practice_length_logs,
      y = practice_length_survey,
      color = Accuracy
    )
  ) +
  geom_polygon(
    data = poly_length, 
    aes(x = x, y = y),
    fill = "lightgrey", 
    alpha = 0.4,
    inherit.aes = FALSE
    ) +
  geom_count() +
  # Visuals
  scale_size_area(
    max_size = area_max,
    breaks = area_breaks,
    limits = area_limits,
    name = legend_title
    ) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 20)) +
  scale_color_manual(
    values = c(
      "Overestimate" = color_negative_high,
      "Underestimate" = color_negative_medium,
      "Correct Estimate" = color_positive_high
    )
  ) +
  ggtitle('Session length (minutes)') +
  ylab('Teacher Report') +
  xlab('Practice Log') +
  common_theme

### Combine plots
p_scatter_all <- 
  p_scatter_dur +
  p_scatter_freq +
  p_scatter_length +
  plot_layout(
    axis_titles = "collect",
    guides = "collect")

p_scatter_all
ggsave(
  filename = here::here('output/manuscript_tables_figures/plot_accuracy.png'),
  dpi = 600,
  width = 7.1,
  height = 6
)
