# source data ----
# source(here::here('analyses/04_final_analyses.R'))
# Set seed ----
set.seed(1234)

# Length ----
cors_sampled_length <- sample_correlation(
  sample_size = nrow(data),
  population_mean = mean(data$practice_length_logs),
  population_sd = sd(data$practice_length_logs),
  simulations = 10000
)
summary(cors_sampled_length)
CI_length <- confidence_interval(
  estimate = mean(cors_sampled_length),
  SD = sd(cors_sampled_length),
  sample_size = length(cors_sampled_length)
)

# Frequency ----
cors_sampled_freq <- sample_correlation(
  sample_size = nrow(data),
  population_mean = mean(data$practice_freq_logs),
  population_sd = sd(data$practice_freq_logs),
  simulations = 10000
)
summary(cors_sampled_freq)
CI_freq <- confidence_interval(
  estimate = mean(cors_sampled_freq),
  SD = sd(cors_sampled_freq),
  sample_size = length(cors_sampled_freq)
)

# Weeks ----
cors_sampled_weeks <- sample_correlation_weeks(
  sample_size = nrow(data),
  population_mean = mean(data$practice_weeks_logs),
  population_sd = sd(data$practice_weeks_logs),
  simulations = 10000
)
summary(cors_sampled_weeks)
CI_weeks <- confidence_interval(
  estimate = mean(cors_sampled_weeks),
  SD = sd(cors_sampled_weeks),
  sample_size = length(cors_sampled_weeks)
)

# Ciitime ----
# cors_sampled_ciitime <- sample_correlation(
#   sample_size = 220,
#   population_mean = 5.521,
#   population_sd = sqrt(12.216),
#   simulations = 10000
# )
# summary(cors_sampled_ciitime)
# confidence_interval(
#   Mean = mean(cors_sampled_ciitime),
#   SD = sd(cors_sampled_ciitime),
#   sample_size = length(cors_sampled_ciitime)
# ) # 0.9966 - 0.9966

# Inspect results ----
## Compile table of results
simulation_results <- rbind(
  CI_length,
  CI_freq,
  CI_weeks
) |> as_tibble()
simulation_results$Variable <- c(
  'Length',
  'Frequency',
  'Duration'
)
simulation_results_formatted <- simulation_results |> 
  mutate(
    `Correlation Estimate` = format_number_for_table(estimate),
    `Standard Error` = format_number_for_table(SE),
    `Standard Error` = if_else(
      `Standard Error` == '0.00',
      '< .01',
      `Standard Error`
    ),
    `95% CI` = paste0(
      '[',
      format_number_for_table(lower_limit),
      ', ',
      format_number_for_table(upper_limit),
      ']'
    )
  ) |> 
    dplyr::select(
      Variable,
      `Correlation Estimate`,
      `Standard Error`,
      `95% CI`
    )

## Write results to file
simulation_results_formatted |> 
  write.table(
    file = here::here('output/manuscript_tables_figures/simulation_results.csv'),
    sep = ';',
    row.names = F
    )
