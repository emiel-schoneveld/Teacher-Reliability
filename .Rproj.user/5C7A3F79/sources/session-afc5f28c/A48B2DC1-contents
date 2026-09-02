# Reliability
## By Emiel Schoneveld

# General syntax ----
## Source data
source(here::here('analyses/03_descriptive_analyses.R'))

## Load packages

# Reliability ----
## School level
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

## Group level
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

## Student level
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

## Overall level
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

## Combine data
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

## Inspect data ----

## Plot reliability ----
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

