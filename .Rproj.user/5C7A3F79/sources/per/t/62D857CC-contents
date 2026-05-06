# Final analyses
## by Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(here)
library(lavaan)
library(patchwork)

## Source functions
source(
  here::here(
    'analyses/functions.R'
    )
)

# Load data ----
## Wide data
load(
  here::here('input/cleaned_data/data_wide.rds')
)

# Fidelity analysis easy fix by filtering ----
# data <- data |> 
#   filter(
#     date_survey >= as.Date("2024-07-01") &
#       date_survey <= as.Date("2024-07-31")
#   )

# Descriptive analysis ----
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

# Main analysis - fit covariance SEM models ----
## Length ----
### Specify model 
model_length <- '
practice_length_logs ~~ covar_length*practice_length_survey

practice_length_logs ~ int_logs_length*1
practice_length_survey ~ int_survey_length*1
'

### Fit and inspect unconstrained model
fit_length <- sem(
  model = model_length,
  data = data,
  cluster = "group_ID",
)

summary(fit_length,
        fit.measures = T,
        std = T,
)

### Fit and inspect model with constrained intercepts
fit_length_restricted <- sem(
  model = c(model_length, 
            'int_logs_length == int_survey_length'),
  data = data,
  cluster = "group_ID"
)

summary(fit_length_restricted,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_length,
  fit_length_restricted
)

### Inspect final model
summary(fit_length,
        std = T)

## freq ----
### Specify model
model_freq <- '
practice_freq_logs ~~ covar_freq*practice_freq_survey

practice_freq_logs ~ int_logs_freq*1
practice_freq_survey ~ int_survey_freq*1
'

### Fit and inspect unconstrained model
fit_freq <- sem(
  model = model_freq,
  data = data,
  cluster = "group_ID",
)

summary(fit_freq,
        fit.measures = T,
        std = T
)

### Fit and inspect model with constrained intercepts
fit_freq_restricted <- sem(
  model = c(model_freq, 
            'int_logs_freq == int_survey_freq'),
  data = data,
  cluster = "group_ID"
)

summary(fit_freq_restricted,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_freq,
  fit_freq_restricted
)

### Inspect final model
summary(fit_freq,
        std = T)

## weeks ----
### Specify model
model_weeks <- '
practice_weeks_logs ~~ covar_weeks*practice_weeks_survey

practice_weeks_logs ~ int_logs_weeks*1
practice_weeks_survey ~ int_survey_weeks*1
'

### Fit and inspect unconstrained model
fit_weeks <- sem(
  model = model_weeks,
  data = data,
  cluster = "group_ID",
)

summary(fit_weeks,
        fit.measures = T,
        std = T,
)

### Fit and inspect model with constrained intercepts
fit_weeks_restricted <- sem(
  model = c(model_weeks, 
            'int_logs_weeks == int_survey_weeks'),
  data = data,
  cluster = "group_ID"
)

summary(fit_weeks_restricted,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_weeks,
  fit_weeks_restricted
)

### Inspect final model
summary(fit_weeks,
        std = T)

## ciitime ----
### Specify model
model_ciitime <- '
practice_ciitime_logs ~~ covar_ciitime*practice_ciitime_survey

practice_ciitime_logs ~ int_logs_ciitime*1
practice_ciitime_survey ~ int_survey_ciitime*1
'

### Fit and inspect unconstrained model
fit_ciitime <- sem(
  model = model_ciitime,
  data = data,
  cluster = "group_ID",
)

summary(fit_ciitime,
        fit.measures = T,
        std = T,
)

### Fit and inspect model with constrained intercepts
fit_ciitime_restricted <- sem(
  model = c(model_ciitime, 
            'int_logs_ciitime == int_survey_ciitime'),
  data = data,
  cluster = "group_ID"
)

summary(fit_ciitime_restricted,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_ciitime,
  fit_ciitime_restricted
)

### Inspect final model
summary(fit_ciitime,
        std = T)

# Combine final results in new dataframe ----
## Correlations ----
### Combine correlations of all variables
results_correlations <- rbind(
  standardizedsolution(fit_length),
  standardizedsolution(fit_freq),
  standardizedsolution(fit_weeks),
  standardizedsolution(fit_ciitime)
) |> 
  as_tibble() |> 
  dplyr::select(
    label,
    `est.std`,
    se,
    `ci.lower`,
    `ci.upper`
  ) |> 
  filter(
    str_detect(label, 'covar')
  ) |> 
  separate_wider_delim(cols = label,
                       delim = '_',
                       names = c('covar', 'measure')
  ) |> 
  dplyr::select(
    !covar
  ) |> 
  rename(
    estimate = `est.std`,
    lower_bound = `ci.lower`,
    upper_bound = `ci.upper`
  ) |> 
  mutate(
    estimate = format_number_for_table(estimate, 2),
    se = format_number_for_table(se, 2),
    lower_bound = format_number_for_table(lower_bound, 2),
    upper_bound = format_number_for_table(upper_bound, 2),
  ) |> 
  relocate(
    measure,
    .before = 1
  )

### Inspect correlations
results_correlations

## Intercepts ----
### Combine intercepts
results_intercepts <- rbind(
  parameterestimates(fit_length),
  parameterestimates(fit_freq),
  parameterestimates(fit_weeks),
  parameterestimates(fit_ciitime)
) |> 
  as_tibble() |> 
  dplyr::select(
    label,
    est,
    se,
    `ci.lower`,
    `ci.upper`
  ) |> 
  filter(
    str_detect(label, 'int')
  ) |> 
  separate_wider_delim(cols = label,
                       delim = '_',
                       names = c('int', 'rater', 'measure')
  ) |> 
  dplyr::select(
    !int
  ) |> 
  rename(
    estimate = est,
    lower_bound = `ci.lower`,
    upper_bound = `ci.upper`
  ) |> 
  mutate(
    estimate = format_number_for_table(estimate, 2),
    se = format_number_for_table(se, 2),
    lower_bound = format_number_for_table(lower_bound, 2),
    upper_bound = format_number_for_table(upper_bound, 2),
  ) |> 
  relocate(
    measure,
    .before = 1
  )

# Inspect intercepts
results_intercepts  

# Inspect final results ----
results_correlations
results_intercepts

# Construct tables for paper ----
## Estimate table ----
### Combine intercepts and correlations, and rename variables
table_estimates <- rbind(
  results_intercepts,
  cbind('rater' = NA, results_correlations)
  )

### Add 95% CI
table_estimates <- table_estimates |> 
  mutate(
    `95% CI` = paste0(
      lower_bound, ' - ', upper_bound
    )
  ) |> 
  dplyr::select(
    !contains('bound')
  )

### Recode and rename variable
table_estimates <- table_estimates |> 
  mutate(
    rater = recode(
      rater,
      survey = 'Teacher Report',
      logs = 'Practice Log'
    ),
    measure = recode(
      measure,
      length = 'Length (minutes)',
      freq = 'Frequency (sessions per week)',
      weeks = 'Duration (weeks)',
      ciitime = 'Total Practice Time (hours)'
    )
  ) |> 
  rename(
    Rater = rater,
    Measure = measure,
    Estimate = estimate,
    SE = se
  )

### Add header rows
table_estimates <- table_estimates |> 
  add_row(
    Rater = 'Correlation',
    .after = 8
  ) |> 
  add_row(
    Rater = 'Intercept',
    .before = 1
  )

### Write table
# table_estimates |> 
#   write.table(
#     here::here('output/manuscript_tables_figures/table_estimates.csv'),
#     sep = ';',
#     row.names = F
#   )
# 
# View(table_estimates)

# Multilevel try ----
library(lme4)
library(psych)
library(performance)

# Length ----


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
  measure = 'length'
)
# performance::check_model(ML_length_survey)


# correlations
data |> 
  group_by(
    school_ID
  ) |> 
  summarise(
    survey_mean = mean(practice_length_survey, na.rm = T),
    logs_mean = mean(practice_length_logs, na.rm = T),
  ) |> 
  dplyr::select(
    contains('mean')
  ) |> cor()
cor(
  coef(ML_length_logs)$`school_ID`,
  coef(ML_length_survey)$`school_ID`
)
data |> 
  group_by(
    group_ID
  ) |> 
  summarise(
    survey_mean = mean(practice_length_survey, na.rm = T),
    logs_mean = mean(practice_length_logs, na.rm = T),
  ) |> 
  dplyr::select(
    contains('mean')
  ) |> cor()
cor(
  coef(ML_length_logs)$`school_ID:group_ID`,
  coef(ML_length_survey)$`school_ID:group_ID`
  )

data |> 
  dplyr::select(
    contains('length') 
  ) |> cor()
cor(
  resid(ML_length_logs),
  resid(ML_length_survey)
)

ML_length_survey_logs <- lmer(
  practice_length_survey ~ practice_length_logs + 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)
ML_length_logs_survey <- lmer(
  practice_length_logs ~ 1 + practice_length_survey + (1  +practice_length_survey | school_ID) + (1 + practice_length_survey | school_ID:group_ID),
  data = data
)
vars_length
summary(ML_length_logs, correlation = T)
mod_sem_length <- '
level: 1
    practice_length_logs ~~ practice_length_survey

  level: 2
    practice_length_logs ~~ practice_length_survey

  level: 3
    practice_length_logs ~~ practice_length_survey
'
fit_sem_length <- sem(
  mod_sem_length,
  data = data,
  cluster = c("school_ID", "group_ID"),
  estimator = "MLR"
)
summary(fit_sem_length)





# Frequency ----
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
  measure = 'frequency'
)

# Duration
ML_weeks_logs <- lmer(
  practice_weeks_logs ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

ML_weeks_survey <- lmer(
  practice_weeks_survey ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

vars_weeks <- decompose_variance(
  model_logs = ML_weeks_logs,
  model_survey = ML_weeks_survey,
  measure = 'weeks'
)

# ciitime
ML_ciitime_logs <- lmer(
  practice_ciitime_logs ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

ML_ciitime_survey <- lmer(
  practice_ciitime_survey ~ 1 + (1 | school_ID) + (1 | school_ID:group_ID),
  data = data
)

vars_ciitime <- decompose_variance(
  model_logs = ML_ciitime_logs,
  model_survey = ML_ciitime_survey,
  measure = 'ciitime'
)

vars_length |> 
  bind_rows(
    vars_freq,
    vars_weeks,
    vars_ciitime
  ) |>
  # Stacked barplot
  ggplot(
    aes(
      x = Model,
      y = ICC,
      fill = Group
    )
  ) +
  geom_bar(
    position = "stack",
    stat = 'identity'
  ) +
  
  # Dodges barplot
  # ggplot(
  #   aes(
  #     fill = Model,
  #     y = ICC,
  #     x = Group
  #   )
  # ) +
  # geom_bar(
  #   position = 'dodge',
  #   stat = 'identity'
  # ) +
  
  
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.20)
    ) +
  facet_wrap(
    ~Measure
  )


### -----
library(nlme)
data_long <- data |>
  pivot_longer(
    cols = c(practice_length_logs, practice_length_survey),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    # Dummy indicators for each variable (for the mean structure)
    is_logs   = as.numeric(variable == "practice_length_logs"),
    is_survey = as.numeric(variable == "practice_length_survey")
  )

# --- Fit bivariate 3-level model ---
model_length_lme <- lme(
  fixed = value ~ 0 + is_logs + is_survey,   # separate intercepts, no shared intercept
  random = list(
    school_ID  = pdSymm(~ 0 + is_logs + is_survey),  # 2x2 cov matrix at school level
    group_ID = pdSymm(~ 0 + is_logs + is_survey)   # 2x2 cov matrix at teacher level
  ),
  weights = varIdent(form = ~ 1 | variable),       # allow different residual variances
  data = data_long,
  control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)
)

summary(model_length_lme)

# --------------
library(brms)

# --- Fit bivariate 3-level model ---
# bf() defines each outcome separately; share random effects across levels
formula_logs <- bf(practice_length_logs ~ 1 + 
                     (1 | p | school_ID) +          # school level
                     (1 | q | group_ID))   # teacher level

formula_survey <- bf(practice_length_survey ~ 1 + 
                       (1 | p | school_ID) +        # school level
                       (1 | q | group_ID)) # teacher level

prior_brms <- c(
  prior(lkj(4), class = cor, group = school_ID),
  prior(lkj(1), class = cor, group = group_ID)
)
# The "p" label in | p | tells brms to estimate the *covariance*
# between random effects sharing the same label across the two formulas

# model_test <- brm(
#   formula = formula_logs + formula_survey + set_rescor(TRUE),
#   data = data,
#   chains = 1,
#   iter = 500,
#   warmup = 100,
#   seed = 42
# )

time_begin <- Sys.time()
# model_brms <- brm(
#   formula = formula_logs + formula_survey + set_rescor(TRUE),
#   data = data,
#   prior = prior_brms,
#   chains = 4,
#   cores = 4,
#   iter = 4000,
#   warmup = 1000,
#   seed = 42,
#   control = list(adapt_delta = 0.99, max_treedepth = 15)  # default is 0.80, try 0.95 or 0.99
# )
time_end <- Sys.time()
time_end - time_begin
here::here()
# save(
#   model_brms,
#   file = here::here('output/model_brms.RData')
# )
load(here::here('output/model_brms.RData'))
summary(model_brms)
# pairs(model_brms,
#       variable = c(
#         "cor_group_ID__practicelengthlogs_Intercept__practicelengthsurvey_Intercept" ,
#         "cor_school_ID__practicelengthlogs_Intercept__practicelengthsurvey_Intercept",
#         "rescor__practicelengthlogs__practicelengthsurvey" 
#       ))

# --- Extract posterior draws for all SD parameters ---
draws <- as_draws_df(model_brms)

# --- For practice_length_logs ---
draws <- draws |>
  mutate(
    # Variance components for logs
    var_school_logs   = sd_school_ID__practicelengthlogs_Intercept^2,
    var_teacher_logs  = sd_group_ID__practicelengthlogs_Intercept^2,
    var_student_logs  = sigma_practicelengthlogs^2,
    var_total_logs    = var_school_logs + var_teacher_logs + var_student_logs,
    
    # ICCs for logs
    icc_school_logs   = var_school_logs  / var_total_logs,
    icc_teacher_logs  = var_teacher_logs / var_total_logs,
    icc_student_logs  = var_student_logs / var_total_logs,
    
    # Variance components for survey
    var_school_survey  = sd_school_ID__practicelengthsurvey_Intercept^2,
    var_teacher_survey = sd_group_ID__practicelengthsurvey_Intercept^2,
    var_student_survey = sigma_practicelengthsurvey^2,
    var_total_survey   = var_school_survey + var_teacher_survey + var_student_survey,
    
    # ICCs for survey
    icc_school_survey  = var_school_survey  / var_total_survey,
    icc_teacher_survey = var_teacher_survey / var_total_survey,
    icc_student_survey = var_student_survey / var_total_survey
  )

# --- Summarise posterior distributions of ICCs ---
icc_summary <- draws |>
  as_tibble() |> 
  select(starts_with("icc_")) |>
  pivot_longer(everything(), names_to = "parameter", values_to = "value") |>
  group_by(parameter) |>
  summarise(
    mean     = mean(value),
    median   = median(value),
    sd       = sd(value),
    ci_lower = quantile(value, 0.025),
    ci_upper = quantile(value, 0.975)
  ) |>
  # Clean up parameter names for readability
  mutate(
    variable = ifelse(str_detect(parameter, "logs"), "practice_length_logs", "practice_length_survey"),
    level    = case_when(
      str_detect(parameter, "school")   ~ "School",
      str_detect(parameter, "teacher")  ~ "Teacher",
      str_detect(parameter, "student")  ~ "Student"
    )
  ) |>
  select(variable, level, mean, median, sd, ci_lower, ci_upper)

icc_summary |> 
  arrange(variable)
