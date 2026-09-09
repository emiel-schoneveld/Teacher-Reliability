# Concurrent validity
## By Emiel Schoneveld

# General syntax ----
## Source data
source(here::here('analyses/03_descriptive_analyses.R'))

## Load packages

# Concurrent validity ----
## Duration ----
### Overall
cor_dur_overall <- KendallTauB(
  x = data |> pull(practice_duration_logs_months),
  y = data |> pull(practice_duration_survey_ranked),
  conf.level = 0.95
)

## Frequency ----
### Overall
cor_freq_overall <- cor.test(
  x = data |> pull(practice_freq_logs),
  y = data |> pull(practice_freq_survey),
  method = "pearson"
)

### Freq L1
cor_freq_L1 <- cor.test(
  x = data |> pull(practice_freq_logs_L1),
  y = data |> pull(practice_freq_survey_L1),
  method = "pearson"
)

### freq L2
cor_freq_L2 <- cor.test(
  x = data |> 
    distinct(group_ID, .keep_all = T) |> 
    pull(practice_freq_logs_L2),
  y = data |> 
    distinct(group_ID, .keep_all = T) |> 
    pull(practice_freq_survey_L2),
  method = "pearson"
)

### freq L3
cor_freq_L3 <- cor.test(
  x = data |> 
    distinct(school_ID, .keep_all = T) |> 
    pull(practice_freq_logs_L3),
  y = data |> 
    distinct(school_ID, .keep_all = T) |> 
    pull(practice_freq_survey_L3),
  method = "pearson"
)

## Length ----
### Overall
cor_length_overall <- cor.test(
  x = data |> pull(practice_length_logs),
  y = data |> pull(practice_length_survey),
  method = "pearson"
)

### Length L1
cor_length_L1 <- cor.test(
  x = data |> pull(practice_length_logs_L1),
  y = data |> pull(practice_length_survey_L1),
  method = "pearson"
)

### Length L2
cor_length_L2 <- cor.test(
  x = data |> 
    distinct(group_ID, .keep_all = T) |> 
    pull(practice_length_logs_L2),
  y = data |> 
    distinct(group_ID, .keep_all = T) |> 
    pull(practice_length_survey_L2),
  method = "pearson"
)

### Length L3
cor_length_L3 <- cor.test(
  x = data |> 
    distinct(school_ID, .keep_all = T) |> 
    pull(practice_length_logs_L3),
  y = data |> 
    distinct(school_ID, .keep_all = T) |> 
    pull(practice_length_survey_L3),
  method = "pearson"
)

## Bundle and inspect data ----
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

### Duration ----
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
  ylab('Correlation') +
  xlab('Level') +
  common_theme

### Frequency ----
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
  ylab('Correlation') +
  xlab('Level') +
  common_theme

### Length ----
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
  ylab('Correlation') +
  xlab('Level') +
  common_theme

### Combine plots ----
p_cor_all <- 
  p_cor_dur +
  p_cor_freq +
  p_cor_length +
  plot_layout(
    axis_titles = "collect",
    guides = "collect")

# p_cor_all
# ggsave(
#   filename = here::here('output/manuscript_tables_figures/plot_reliability.png'),
#   dpi = 600,
#   width = plot_width,
#   height = plot_height
# )
