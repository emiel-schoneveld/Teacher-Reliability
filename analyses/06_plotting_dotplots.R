# Plotting dotplot for manuscript ----

# General syntax ----
## Load packages
library(here)
library(tidyverse)
library(patchwork)
library(plyr)

# Load data ----
## Wide data
load(
  here::here('input/cleaned_data/data_wide.rds')
)

# Make plot ----
## Define common theme ----
common_theme <- theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    title = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
    )

## Define common visual parameters ----
limits_size <- range(1, 45)
range_size <- range(1, 4.5)
breaks_size <- c(1, 5, 20, 45)
legend_title <- 'N Students'
line_alpha = 0.4
annotate_point_color = 'orange'
annotate_text_size = 3
color_overestimate <- "#D55E00"
color_underestimate <- "#E69F00"
color_correctestimate <- "#009E73"

## Plot length ----
p1_length <- data |> 
  mutate(
    practice_length_logs = round(practice_length_logs)
  ) |> 
  mutate(
    Accuracy = case_when(
      practice_length_survey > practice_length_logs ~ 'Overestimate',
      practice_length_survey < practice_length_logs ~ 'Underestimate',
      practice_length_survey == practice_length_logs ~ 'Correct Estimate'
    )
  ) |> 
  ggplot(
    aes(
      y = practice_length_survey,
      x = practice_length_logs,
      color = Accuracy
    )
  ) +
  geom_abline(
    intercept = 0, 
    slope = 1, 
    alpha = line_alpha
    ) +
  geom_count() +
  scale_size_continuous(
    limits = limits_size,
    range = range_size,
    breaks = breaks_size,
    name = legend_title
  ) +
  scale_color_manual(
    values = c("Overestimate" = color_overestimate,
               "Underestimate" = color_underestimate,
               "Correct Estimate" = color_correctestimate
    )
  ) +
  ggtitle('Session Length (min)') +
  ylab('Teacher Report') +
  xlab('Practice Log') +
  coord_cartesian(
    xlim = c(0, 20),
    ylim = c(0, 20)
    ) +
  common_theme
# p1_length

## Plot frequency ----
p2_freq <- data |> 
  mutate(
    practice_freq_logs = round_any(practice_freq_logs, 1/4)
  ) |> 
  mutate(
    Accuracy = case_when(
      practice_freq_survey > practice_freq_logs ~ 'Overestimate',
      practice_freq_survey < practice_freq_logs ~ 'Underestimate',
      practice_freq_survey == practice_freq_logs ~ 'Correct Estimate'
    )
  ) |> 
  ggplot(
    aes(
      y = practice_freq_survey,
      x = practice_freq_logs,
      color = Accuracy
    )
  ) +
  geom_abline(
    intercept = 0, 
    slope = 1,
    alpha = line_alpha
    ) +
  # Point for labeling
  # geom_point(
  #   x = 2,
  #   y = 2,
  #   color = annotate_point_color,
  #   size = 4
  # ) +
  geom_count() +
  scale_size_continuous(
    limits = limits_size,
    range = range_size,
    breaks = breaks_size,
    name = legend_title
  ) +
  scale_color_manual(
    values = c("Overestimate" = color_overestimate,
               "Underestimate" = color_underestimate,
               "Correct Estimate" = color_correctestimate
    )
  ) +
  ggtitle('Frequency (days per week)') +
  ylab('Teacher Report') +
  xlab('Practice Log') +
  coord_cartesian(
    xlim = c(0, 5),
    ylim = c(0, 5)
    ) +
  common_theme
# p2_freq

## Plot duration ----
p3_weeks <- data |> 
  mutate(
    Accuracy = case_when(
      practice_weeks_survey > practice_weeks_logs ~ 'Overestimate',
      practice_weeks_survey < practice_weeks_logs ~ 'Underestimate',
      practice_weeks_survey == practice_weeks_logs ~ 'Correct Estimate'
    )
  ) |> 
  ggplot(
    aes(
      y = practice_weeks_survey,
      x = practice_weeks_logs,
      color = Accuracy
    )
  ) +
  geom_abline(
    intercept = 0, 
    slope = 1, 
    alpha = line_alpha
    ) +
  # Point for labeling
  # geom_point(
  #   x = 7,
  #   y = 1,
  #   color = annotate_point_color,
  #   size = 2
  # ) +
  geom_count() +
  scale_size_continuous(
    limits = limits_size,
    range = range_size,
    breaks = breaks_size,
    name = legend_title
  ) +
  scale_color_manual(
    values = c("Overestimate" = color_overestimate,
               "Underestimate" = color_underestimate,
               "Correct Estimate" = color_correctestimate
    )
  ) +
  ggtitle('Duration (weeks)') +
  ylab('Teacher Report') +
  xlab('Practice Log') +
  coord_cartesian(
    xlim = c(0, 20),
    ylim = c(0, 20)) +
  common_theme
# p3_weeks

## Plot total practice time ----
p4_ciitime <- data |> 
  mutate(
    practice_ciitime_logs = round(practice_ciitime_logs),
    practice_ciitime_survey = round(practice_ciitime_survey)
  ) |> 
  mutate(
    Accuracy = case_when(
      practice_ciitime_survey > practice_ciitime_logs ~ 'Overestimate',
      practice_ciitime_survey < practice_ciitime_logs ~ 'Underestimate',
      practice_ciitime_survey == practice_ciitime_logs ~ 'Correct Estimate'
    )
  ) |> 
  ggplot(
    aes(
      y = practice_ciitime_survey,
      x = practice_ciitime_logs,
      color = Accuracy
    )
  ) +
  geom_abline(
    intercept = 0, 
    slope = 1, 
    alpha = line_alpha
    ) +
  # Point for labeling
  # geom_point(
  #   x = 7,
  #   y = 25,
  #   color = annotate_point_color,
  #   size = 2
  # ) +
  geom_count() +
  scale_size_continuous(
    limits = limits_size,
    range = range_size,
    breaks = breaks_size,
    name = legend_title
  ) +
  scale_color_manual(
    values = c("Overestimate" = color_overestimate,
               "Underestimate" = color_underestimate,
               "Correct Estimate" = color_correctestimate
    )
    ) +
  ggtitle('Total Practice Time (hours)') +
  ylab('Teacher Report') +
  xlab('Practice Log') +
  coord_cartesian(
    xlim = c(0, 25),
    ylim = c(0, 25)
  ) +
  scale_x_continuous(
    breaks = c(0, 5, 10, 15, 20, 25),
    minor_breaks = c(2.5, 7.5, 12.5, 17.5, 22.5)
  ) +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15, 20, 25),
    minor_breaks = c(2.5, 7.5, 12.5, 17.5, 22.5)
  ) +
  common_theme
# p4_ciitime

## Combine and save plots ----
scatterplot_teacher_vs_logs <- p1_length +
  p2_freq +
  p3_weeks +
  p4_ciitime +
  plot_layout(
    axis_titles = "collect",
    guides = "collect")

scatterplot_teacher_vs_logs
ggsave(
    filename = here::here('output/manuscript_tables_figures/dotplot.png'),
    dpi = 600,
    width = 7.1,
    height = 6
)