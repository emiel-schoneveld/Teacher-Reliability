# Equivalence
## By Emiel Schoneveld

# General syntax ----
## Source data
source(here::here('analyses/03_descriptive_analyses.R'))

## Load packages

# Blandt Altman plots ----
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








# Old ----
## Duration
summary(data$practice_duration_survey)
typical(data$practice_duration_logs_months)

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

## Inspect estimation error ----
data |> 
  pivot_longer(
    contains('_estimate'),
    names_to = 'measure',
    values_to = 'estimate'
  ) |> 
  group_by(
    measure
  ) |> 
  summarise(
    n_total = n(),
    perc_correct = ((sum(estimate == "Correct Estimate")/n_total)*100) |> round(2),
    perc_overestimate = ((sum(estimate == "Overestimate")/n_total)*100) |> round(2),
    perc_underestimate = ((sum(estimate == "Underestimate")/n_total)*100) |> round(2)
  )

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

### Duration ----
p_scatter_dur <- data |> 
  # Rounding
  mutate(
    practice_duration_logs_months = round_to(
      practice_duration_logs_months, 
      0.5
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
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  ggtitle('Duration (months)') +
  ylab('Teacher-report') +
  xlab('Practice Log') +
  common_theme

### Frequency ----
p_scatter_freq <- data |> 
  # Rounding
  mutate(
    practice_freq_logs = round_to(
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
  ylab('Teacher-report') +
  xlab('Practice Log') +
  common_theme

### Length ----
p_scatter_length <- data |> 
  # Rounding
  mutate(
    practice_length_logs = round_to(
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
  ggtitle('Length (minutes)') +
  ylab('Teacher-report') +
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

# p_scatter_all
# ggsave(
#   filename = here::here('output/manuscript_tables_figures/plot_accuracy.png'),
#   dpi = 600,
#   width = plot_width,
#   height = plot_height
# )
