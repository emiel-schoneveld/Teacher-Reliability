# Exploratory analyses
## by Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(here)
library(writexl)
library(irr)
library(MASS)
library(gridExtra)  
library(mnormt) 
library(lme4) 
library(knitr) 
library(kableExtra)
library(lavaan)

# Load data ----
## Wide data
load(
  here('input/cleaned_data/data_wide.rds')
  )

## Long data
load(
  here('input/cleaned_data/data_long.rds')
)

# Add level averages ----
data_lvl2 <- data |> 
  group_by(
    group_ID
  ) |> 
  summarise(
    across(
      contains('practice') & !(contains('duration') | contains('type') | contains('explanation')),
      \(x) mean(x, na.rm = TRUE),
      .names = 'mean_{.col}'),
    n_students = n()
  )

data_lv3 <- data |> 
  group_by(
    school_ID
  ) |> 
  summarise(
    across(
      contains('practice') & !(contains('duration') | contains('type') | contains('explanation')),
      \(x) mean(x, na.rm = TRUE),
      .names = 'mean_{.col}'),
    n_students = n()
  )

hist_lv1 <- data |> 
  ggplot(
    aes(
      x = practice_freq_survey
    )
  ) +
  geom_histogram(
    bins = 40, color = 'white'
  ) +
  xlim(0,5)
hist_lv2 <- data_lvl2 |> 
  ggplot(
    aes(
      x = mean_practice_freq_survey
    )
  ) +
  geom_histogram(
    bins = 20, color = 'white'
  ) +
  xlim(0,5)
mli_hist1 <- grid.arrange(
  hist_lv1,
  hist_lv2,
  ncol = 1
)


data |> 
  ggplot(
    aes(
      x = practice_freq_logs,
      y = practice_freq_survey
    )
  ) +
  geom_point(
    alpha = 1/3
  ) +
  geom_smooth(
    method = 'lm',
    se = FALSE
    ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = 'grey',
    alpha = 1/2
  ) +
  facet_wrap(
    ~group_ID,
    nrow = 5
  )


# Inspect correlation between survey and logs
cor_table <- data |> 
  dplyr::select(
      contains('practice')
  ) |> 
  dplyr::select(
      contains('weeks') | 
      contains('freq') |
      contains('length') |
      contains('ciitime')
  ) |> 
  relocate(
    contains('logs')
  ) |> 
  cor(
    use = 'pairwise.complete'
  ) |> 
  round(2) |> 
  as_tibble()

cor_table_with_names <- cor_table |> 
  mutate(
    variable_names = colnames(cor_table),
    .before = 1
  )

cor_table_with_names |> 
  dplyr::select(
    contains('variable') | contains('logs')
  ) |> 
  filter(
    str_detect(variable_names, 'survey')
  ) |> 
  pivot_longer(
    contains('practice'),
    names_to = 'digi_logs',
    values_to = 'correlation') |> 
  ggplot(
    aes(
      x = variable_names,
      y = digi_logs,
      fill = correlation,
      label = correlation
      )
  ) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient(
    low = "red",
    high = "green",
    limits = c(0,1)
    ) +
  labs(
    x = 'Teacher reports',
    y = 'Practice logs'
  )

data_long_comparison <- data_long |> 
  filter(
    measure %in% c('weeks', 'freq', 'length', 'ciitime')
  ) |>
  pivot_wider(
    # id_cols = student_ID,
    names_from = report,
    values_from = score
  )


data_long_comparison <- data_long_comparison |> 
  group_by(
    measure
  ) |> 
  mutate(
    error_margin = sd(logs, na.rm = T) * 1
  ) |> 
  ungroup() |> 
  mutate(
    survey_error = survey - logs,
    correct = abs(survey_error) <= error_margin,
    over_est = !correct & (survey_error > 0)
  )


data_long |> 
  group_by(
    measure,
    report
  ) |> 
  summarise(
    N = n(),
    M = mean(score, na.rm = T),
    Md = median(score, na.rm = T),
    Min = min(score, na.rm = T),
    Max = max(score, na.rm = T),
    SD = sd(score, na.rm = T)
  )


data_long_comparison |> 
  group_by(
    measure
  ) |> 
  summarise(
    N = n(),
    prop_correct = sum(correct, na.rm = T) / n(),
    prop_incorrect = 1 - prop_correct,
    prop_overest = sum(over_est, na.rm = T) / n(),
    prop_underest = prop_incorrect - prop_overest,
    M = mean(logs, na.rm = T),
    MAX = max(logs, na.rm = T),
    MIN = min(logs, na.rm = T),
    ER = mean(error_margin, na.rm = T),
  )

data |> 
  mutate(
    duration_correct = practice_duration_logs == practice_duration_survey
  ) |> 
  summarise(
    N = n(),
    prop_correct = sum(duration_correct, na.rm = T) / N
  )

data_long_comparison |> 
  ggplot(
    aes(
      x = logs,
      y = survey,
      color = correct
    )
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    alpha = 0.5
  ) +
  geom_jitter(
    alpha = 1/4
  ) +
  facet_wrap(
    ~measure,
    scales = 'free',
    ncol = 5
  )

data_long_comparison |> 
  ggplot(
    aes(
      x = abs(survey_error),
    )
  ) +
  geom_histogram(
    width = 1
  ) +
  facet_wrap(
    ~measure,
    scales = 'free',
    ncol = 5
  )


data |> 
  dplyr::select(
    contains('duration')
  ) |> 
  kappa2(
    weight = "unweighted"
  )

data |> 
  dplyr::select(
    contains('freq')
  ) |> 
  icc()

data |> 
  mutate(
    duration_correct = 
      practice_duration_logs == practice_duration_survey
  ) |> 
  group_by(
    school_ID
  ) |> 
  summarise(
    N = n(),
    N_correct = sum(duration_correct, na.rm = T),
    perc_correct = N_correct / N
  ) |> 
  arrange(
    desc(perc_correct), desc(N)
  ) |> View()



# Terrence analysis ----
## Add dummy coded variables for human and software ----
data_long <- data_long |>
  mutate(
    teacher = ifelse(
      report == 'survey',
      1,
      0
    ),
    software = ifelse(
      report == 'logs',
      1,
      0
    )
  )

## Filter data for analyses ----
### Filter session length data
data_long_length <- data_long |> 
  filter(
    measure == 'length'
  )


## Fit and inspect model with homoskedacity assumption ----
### Length ----
#### Fit model
mod_homoskedacity_length <- lmer(
  score ~ 0 + teacher + software +
    (0 + teacher | group_ID) +
    (1 | student_ID),
  data = data_long_length
    )
summary(mod_homoskedacity_length)

#### Save residuals
data_long_length <- data_long_length |> 
  mutate(
    residuals_homo = residuals(mod_homoskedacity_length)
  ) 

#### Inspect homoskedacity assumption
data_long_length |> 
  ggplot(
    aes(
      as.factor(software), residuals_homo
    )
  ) +
  geom_boxplot()

data_long_length |> 
  ggplot(
    aes(
      residuals_homo
    )
  ) +
  geom_histogram() +
  facet_grid(~as.factor(software))

data_long_length |> 
  ggplot(
    aes(
      score, residuals_homo
    )
  ) +
  geom_point() +
  # geom_smooth(method = 'lm', color = 'red', alpha = 0.5) +
  geom_smooth()+
  facet_grid(~as.factor(software))

# - homoskedasticity holds across the independent variable but not the dependent variable

## Fit model that allows for residual variance differences across the independent variable ----
library(nlme)
mod_heteroscedasticity_length <- lme(
  fixed = score ~ 0 + teacher + software,
  random =  list(
    group_ID = ~ teacher,
    student_ID = ~ 1
    ),
  weights = varIdent(
    form = ~ 1 | software
    ),
  data = data_long_length)

summary(mod_heteroscedasticity_length)
summary(mod_heteroscedasticity_length)$coefficients |> names()
summary(mod_heteroscedasticity_length)$coefficients$fixed['teacher']
summary(mod_heteroscedasticity_length)$coefficients$fixed['software']

#### Save residuals
data_long_length <- data_long_length |> 
  mutate(
    residuals_hetero = residuals(mod_heteroscedasticity_length)
  )

#### Inspect homoskedacity assumption
data_long_length |> 
  ggplot(
    aes(
      as.factor(software), residuals_hetero
    )
  ) +
  geom_boxplot()

data_long_length |> 
  ggplot(
    aes(
      residuals_hetero
    )
  ) +
  geom_histogram() +
  facet_grid(~as.factor(software))

data_long_length |> 
  ggplot(
    aes(
      score, residuals_hetero
    )
  ) +
  geom_point() +
  geom_smooth()+
  facet_grid(~as.factor(software))

data_long_length |> 
  pivot_longer(
    contains('residuals'),
    names_to = 'scedasticity',
    values_to = 'residuals',
    names_prefix = 'residuals_'
  ) |> 
  ggplot(
    aes(
      score, residuals
    )
  ) +
  geom_point() +
  geom_smooth()+
  geom_hline(yintercept = 0) +
  facet_grid(scedasticity~as.factor(software))



# Lavaan multilevel try ----
## Intercept equality addition
addition_intercept_equality <- 'int_logs_between == int_survey_between'

## Length ----
### Specify model
model_length <- '
level: 1
practice_length_logs ~~ covar_within*practice_length_survey

level: 2
practice_length_logs ~~ covar_between*practice_length_survey

practice_length_logs ~ int_logs_between*1
practice_length_survey ~ int_survey_between*1
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
fit_length_equalintercepts <- sem(
  model = c(model_length, 
            addition_intercept_equality),
  data = data,
  cluster = "group_ID"
)

summary(fit_length_equalintercepts,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_length,
  fit_length_equalintercepts
)

### Inspect final model
summary(fit_length,
        fit.measures = T,
        std = T)


# Lavaan single level try ----
## Length ----
### Specify model 
model_length_singlelevel <- '
practice_length_logs ~~ covar_length*practice_length_survey

practice_length_logs ~ int_logs_length*1
practice_length_survey ~ int_survey_length*1
'

### Fit and inspect unconstrained model
fit_length_singlelevel <- sem(
  model = model_length_singlelevel,
  data = data,
  cluster = "group_ID",
)

summary(fit_length_singlelevel,
        fit.measures = T,
        std = T,
)

### Fit and inspect model with constrained intercepts
fit_length_singlelevel_restricted <- sem(
  model = c(model_length_singlelevel, 
            'int_logs_length == int_survey_length'),
  data = data,
  cluster = "group_ID"
)

summary(fit_length_singlelevel_restricted,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_length_singlelevel,
  fit_length_singlelevel_restricted
)

### Inspect final model
summary(fit_length_singlelevel,
        std = T)
standardizedsolution(fit_length_singlelevel, 
                     level = .95)
parameterestimates(fit_length_singlelevel,
                   level = .95)

## freq ----
### Specify model
model_freq_singlelevel <- '
practice_freq_logs ~~ covar_freq*practice_freq_survey

practice_freq_logs ~ int_logs_freq*1
practice_freq_survey ~ int_survey_freq*1
'

### Fit and inspect unconstrained model
fit_freq_singlelevel <- sem(
  model = model_freq_singlelevel,
  data = data,
  cluster = "group_ID",
)

summary(fit_freq_singlelevel,
        fit.measures = T,
        std = T,
)

### Fit and inspect model with constrained intercepts
fit_freq_singlelevel_restricted <- sem(
  model = c(model_freq_singlelevel, 
            'int_logs_freq == int_survey_freq'),
  data = data,
  cluster = "group_ID"
)

summary(fit_freq_singlelevel_restricted,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_freq_singlelevel,
  fit_freq_singlelevel_restricted
)

### Inspect final model
summary(fit_freq_singlelevel,
        std = T)
standardizedsolution(fit_freq_singlelevel, 
                     level = .95)
parameterestimates(fit_freq_singlelevel,
                   level = .95)

## weeks ----
### Specify model
model_weeks_singlelevel <- '
practice_weeks_logs ~~ covar_weeks*practice_weeks_survey

practice_weeks_logs ~ int_logs_weeks*1
practice_weeks_survey ~ int_survey_weeks*1
'

### Fit and inspect unconstrained model
fit_weeks_singlelevel <- sem(
  model = model_weeks_singlelevel,
  data = data,
  cluster = "group_ID",
)

summary(fit_weeks_singlelevel,
        fit.measures = T,
        std = T,
)

### Fit and inspect model with constrained intercepts
fit_weeks_singlelevel_restricted <- sem(
  model = c(model_weeks_singlelevel, 
            'int_logs_weeks == int_survey_weeks'),
  data = data,
  cluster = "group_ID"
)

summary(fit_weeks_singlelevel_restricted,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_weeks_singlelevel,
  fit_weeks_singlelevel_restricted
)

### Inspect final model
summary(fit_weeks_singlelevel,
        std = T)
standardizedsolution(fit_weeks_singlelevel, 
                     level = .95)
parameterestimates(fit_weeks_singlelevel,
                   level = .95)

## ciitime ----
### Specify model
model_ciitime_singlelevel <- '
practice_ciitime_logs ~~ covar_ciitime*practice_ciitime_survey

practice_ciitime_logs ~ int_logs_ciitime*1
practice_ciitime_survey ~ int_survey_ciitime*1
'

### Fit and inspect unconstrained model
fit_ciitime_singlelevel <- sem(
  model = model_ciitime_singlelevel,
  data = data,
  cluster = "group_ID",
)

summary(fit_ciitime_singlelevel,
        fit.measures = T,
        std = T,
)

### Fit and inspect model with constrained intercepts
fit_ciitime_singlelevel_restricted <- sem(
  model = c(model_ciitime_singlelevel, 
            'int_logs_ciitime == int_survey_ciitime'),
  data = data,
  cluster = "group_ID"
)

summary(fit_ciitime_singlelevel_restricted,
        fit.measures = T,
        std = T)

### Compare unconstrained and constrained models
anova(
  fit_ciitime_singlelevel,
  fit_ciitime_singlelevel_restricted
)

### Inspect final model
summary(fit_ciitime_singlelevel,
        std = T)
standardizedsolution(fit_ciitime_singlelevel, 
                     level = .95)
parameterestimates(fit_ciitime_singlelevel,
                   level = .95)



