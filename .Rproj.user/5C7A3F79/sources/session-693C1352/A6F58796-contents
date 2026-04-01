# Function to add trailing zeros for tables
format_number_for_table <- function(number_input = '', 
                                    n_decimals = 2,
                                    trailing_character = '') {
  # Convert to character rounded to two decimals
  string_output <- number_input %>% round(n_decimals) %>% as.character()
  
  # Count characters
  string_length_current <- nchar(string_output)
  
  # Find index of .
  point_index <- str_locate(string_output, '\\.')[,1]
  
  # Calculate total string length
  string_length_final <- ifelse(is.na(point_index),
                                string_length_current + 1 + n_decimals,
                                point_index + n_decimals
  )
  
  # Add trailing zeros
  if (n_decimals > 0) {
    # Add point if necessary
    string_output <- ifelse(is.na(point_index),
                            paste0(string_output, '.'),
                            string_output
    )
    
    # Add trailing zeros
    string_output <- str_pad(string_output, width = string_length_final, pad = '0', side = 'right')
    
  }
  
  # Add trailing character
  string_output <- paste0(string_output, trailing_character)
  
  # Return result
  return(string_output)
}

# sample correlation for length, freq ----
sample_correlation <- function(
    sample_size = 220,
    population_mean = numeric(),
    population_sd = numeric(),
    simulations = 500
) {
  # Make empty vector to store correlations
  cor_sampled <- c()
  
  # Fill vector by sampling correlations
  for (x in 1:simulations) {
    data_simulation <- tibble(.rows = sample_size) |> 
      mutate(
        values_exact = rnorm(
          n = sample_size,
          mean = population_mean,
          sd = population_sd
        ),
        values_rounded = round(values_exact)
      )
    
    # Add sampled correlation
    cor_sampled <- c(
      cor_sampled,
      cor(data_simulation)[1,2]
    )
  }
  
  # Return vector
  return(cor_sampled)
}

# sample correlation for weeks ----
sample_correlation_weeks <- function(
    sample_size = 220,
    population_mean = numeric(),
    population_sd = numeric(),
    simulations = 500
) {
  # Define lookup table for conversion ----
  lookup_table <- tibble(
    category = c(
      'no practice',
      'less than 1 month',
      '1 - 2 months',
      '3 - 4 months',
      'the whole semester'
    ),
    max_months = c(
      0,
      0.5,
      2.5,
      4.5,
      5
    ),
    coded_weeks = c(
      0,
      1,
      6,
      14,
      19
    )
  ) |> 
    mutate(
      proportion = max_months / 5,
      max_weeks = proportion * 20
    )
  
  # Simulate ----
  ## Initialize empty vector
  cor_sampled <- c()
  
  ## Loop through simulation
  for (x in 1:simulations) {
    # Simulate data
    data_simulation <- tibble(.rows = sample_size) |> 
      mutate(
        values_exact = rnorm(
          n = sample_size,
          mean = population_mean,
          sd = population_sd
        ) |> 
          round(),
        values_exact = if_else(
          values_exact > 20,
          20,
          values_exact
        ),
        values_converted = NA
      )
    
    ## Convert weeks to scale of questionaire
    for (row_n in 1:nrow(data_simulation)) {
      index <- (data_simulation$values_exact[row_n] <= lookup_table$max_weeks) |> 
        which() |> 
        min()
      
      data_simulation$values_converted[row_n] <- lookup_table$coded_weeks[index]
    }
    
    ## Add correlation
    cor_sampled <- c(
      cor_sampled,
      cor(data_simulation)[1,2]
    )
  }
  
  # Return correlation ----
  return(cor_sampled)
}

# calculate confidence interval ----
confidence_interval <- function(
    estimate = numeric(),
    SD = numeric(),
    sample_size = numeric()
) {
  SE <- SD / sqrt(sample_size)
  limits <- c(
    estimate - (1.96*SE),
    estimate + (1.96*SE)
  )
  results <- c(
    'estimate' = estimate,
    'SE' = SE,
    lower_limit = limits[1],
    upper_limit = limits[2]
  ) |> round(4)
  return(results)
}


# Decomposing variance from intercept only lmer model
decompose_variance <- function(
    model_logs = NA,
    model_survey = NA,
    measure = NA
) {
  # Calculate variance and stt.dev per level
  var_logs <- summary(model_logs)$varcor |> 
    as_tibble() |> 
    rename(
      Group = 'grp',
      Variance = vcov,
      `Standard Deviation` = sdcor
    ) |> 
    dplyr::select(
      Group, Variance, `Standard Deviation`
    ) |> 
    mutate(
      Model = "logs"
    )
  
  var_survey <- summary(model_survey)$varcor |> 
    as_tibble() |> 
    rename(
      Group = 'grp',
      Variance = vcov,
      `Standard Deviation` = sdcor
    ) |> 
    dplyr::select(
      Group, Variance, `Standard Deviation`
    ) |> 
    mutate(
      Model = "survey"
    )
  
  # Calculate ICC per level
  ICC_logs <- performance::icc(
    model_logs,
    by_group = T
  ) |> 
    as_tibble() |> 
    mutate(
      Model = "logs"
    )
  
  ICC_survey <- performance::icc(
    model_survey,
    by_group = T
  ) |> 
    as_tibble() |> 
    mutate(
      Model = "survey"
    )
  
  # Add residual ICC
  ICC_logs <- ICC_logs |> 
    add_row(
      Group = "Residual",
      ICC = 1 - sum(ICC_logs$ICC, na.rm = T),
      Model = "logs"
    ) 
  
  ICC_survey <- ICC_survey |> 
    add_row(
      Group = "Residual",
      ICC = 1 - sum(ICC_survey$ICC, na.rm = T),
      Model = "survey"
    )
  
  # Calculate Cumulative percentage of variance explained
  ICC_logs <- ICC_logs |> 
    mutate(
      Group = factor(
        Group, 
        levels = c('school_ID', 'school_ID:group_ID', 'Residual')
      )
    ) |> 
    arrange(
      Group
    ) |> 
    mutate(
      `Percentage Var Explained` = cumsum(ICC)
    )
  
  ICC_survey <- ICC_survey |> 
    mutate(
      Group = factor(
        Group, 
        levels = c('school_ID', 'school_ID:group_ID', 'Residual')
      )
    ) |> 
    arrange(
      Group
    ) |> 
    mutate(
      `Percentage Var Explained` = cumsum(ICC)
    )
  
  # Bind tibbles
  results_logs <- var_logs |> 
    full_join(
      ICC_logs,
      by = c("Group", "Model")
    )
  
  results_survey <- var_survey |> 
    full_join(
      ICC_survey,
      by = c("Group", "Model")
    )
  
  results_combined <- results_logs |> 
    bind_rows(
      results_survey
    ) |> 
    mutate(
      Group = factor(
        Group, 
        levels = c('school_ID', 'school_ID:group_ID', 'Residual')
      ),
      Measure = measure
    ) |> 
    relocate(
      Measure, Model
    ) |> 
    arrange(
      Model, Group
    )
  
  # Return results
  return(results_combined)
}
