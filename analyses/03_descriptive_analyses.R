# Descriptive analyses
## by Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(here)

## Load functions
source(here('analyses/functions.R'))

# Load data ----
## Wide data
load(
  here('input/cleaned_data/data_wide.rds')
)

## Long data
load(
  here('input/cleaned_data/data_long.rds')
)

# Descriptive table practice measures for entire sample ----
## Descriptives for length
descriptives_length <- data_long |> 
  filter(
    measure == "length" 
  ) |> 
  group_by(
    report
  ) |> 
  summarise(
    N = n(),
    N_missing = sum(is.na(score), na.rm = T),
    Mean = mean(score, na.rm = T),
    Median = median(score, na.rm = T),
    SD = sd(score, na.rm = T),
    Min = min(score, na.rm = T),
    Max = max(score, na.rm = T),
  ) |>
  mutate(
    Measure = 'length',
    .before = report
  )

## Descriptives for frequency
descriptives_frequency <- data_long |> 
  filter(
    measure == "freq" 
  ) |> 
  group_by(
    report
  ) |> 
  summarise(
    N = n(),
    N_missing = sum(is.na(score), na.rm = T),
    Mean = mean(score, na.rm = T),
    Median = median(score, na.rm = T),
    SD = sd(score, na.rm = T),
    Min = min(score, na.rm = T),
    Max = max(score, na.rm = T),
  ) |>
  mutate(
    Measure = 'freq',
    .before = report
  )

## Descriptives for weeks
descriptives_weeks <- data_long |> 
  filter(
    measure == "weeks" 
  ) |> 
  group_by(
    report
  ) |> 
  summarise(
    N = n(),
    N_missing = sum(is.na(score), na.rm = T),
    Mean = mean(score, na.rm = T),
    Median = median(score, na.rm = T),
    SD = sd(score, na.rm = T),
    Min = min(score, na.rm = T),
    Max = max(score, na.rm = T),
  ) |>
  mutate(
    Measure = 'Weeks',
    .before = report
  )

## Combine table for descriptives of length, freq and weeks
descriptives_length_freq_weeks <- descriptives_length |> 
  bind_rows(
    descriptives_frequency
  ) |> 
  bind_rows(
    descriptives_weeks
  ) |> mutate(
    across(
      Mean:Max,
      ~format_number_for_table(number_input = .)
    )
  )

## Write table
descriptives_length_freq_weeks |> 
  write_csv(
    here('output/descriptives_length_freq_weeks.csv')
    )

## Descriptives for duration
descriptives_duration_logs <- c(
  table(data$practice_duration_logs),
  'Missing' = sum(is.na(data$practice_duration_logs))
) / nrow(data) * 100
descriptives_duration_survey <- c(
  table(data$practice_duration_survey),
  'Missing' = sum(is.na(data$practice_duration_survey))
) / nrow(data) * 100

### Combine and format log and survey descriptives
descriptives_duration <- bind_rows(
  descriptives_duration_logs,
  descriptives_duration_survey
) |> 
  mutate(
    across(
      where(is.numeric), 
      ~replace_na(., 0))
    ) |> 
  relocate(
    `No practice`,
    `Less than 1 month`,
    `1-2 months`,
    `3-4 months`,
    `Complete period`,
    Missing
  )|> mutate(
    across(
      where(is.numeric),
      ~format_number_for_table(number_input = ., trailing_character = '%')
    )
  )

### Write table
descriptives_duration |> 
  write_csv(
    here('output/descriptives_duration.csv')
  )



# Descriptives for multilevel structure ----
## Number of schools, teachers and students: 24, 54, 220
unique(data$school_ID) |>  length()
unique(data$group_ID) |>  length()
nrow(data)

## Number of students per school
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

## Number of teachers per school
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

## Number of students per teacher 
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

## Combine and format table table
frequencies_teachers_students_multilevel <- number_of_teachers_per_school |> 
  bind_rows(
    number_of_students_per_school,
    number_of_students_per_teacher
  ) |>
  mutate(
    across(
      Mean:SD,
      ~format_number_for_table(number_input = .)
    )
  )
  
## Write table
frequencies_teachers_students_multilevel |> 
  write_csv(
    here('output/frequencies_teachers_students_multilevel.csv')
  )


# Descriptive table for participant measures ----
# Number of students per grade
data |>
  group_by(
    as.character(grade), # soms checken of het niet numeriek is 
    gender
  ) |> 
  summarise(
    n_students =  n(),
    perc_students = (n_students/nrow(data))*100
  )

# Age distribution
data |>
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = T),
    mean_age_years = floor(mean_age),
    mean_age_month = ((mean_age - mean_age_years) * 12) |> round(0),
    SD = (sd(age, na.rm = T) * 12) |> round(0)   
    )

# percentages per grade  
# ------------------ hier gebleven -----------------

data |> 
  mutate(
    boy = (gender == 'boy'),
    girl = (gender == 'girl'),
    X = (gender == 'X')
  ) |> 
  group_by(
    grade
  ) |> 
  summarise(
    n_boy = sum(boy, na.rm = T),
    perc_boy = (100 * n_boy / n()) |> round(2),
    n_girl = sum(girl, na.rm = T),
    perc_girl = (100 * n_girl / n()) |> round(2), 
    n_X = sum(X, na.rm = T),
    perc_X = (100 * n_X / n()) |> round(2)
  )
# grade n_boy perc_boy n_girl perc_girl   n_X perc_X
# <dbl> <int>    <dbl>  <int>     <dbl> <int>  <dbl>
# 1     2    39     41.5     55      58.5     0   0   
# 2     3    31     38.8     49      61.2     0   0   
# 3     4    35     57.4     25      41.0     1   1.64

# percentages total sample
data |> 
  mutate(
    boy = (gender == 'boy'),
    girl = (gender == 'girl'),
    X = (gender == 'X')
  ) |> 
  summarise(
    n_boy = sum(boy, na.rm = T),
    perc_boy = (100 * n_boy / n()) |> round(2),
    n_X = sum(X, na.rm = T),
    perc_X = (100 * n_X / n()) |> round(2), 
    n_girl = sum(girl, na.rm = T),
    perc_girl = (100 * n_girl / n()) |> round(2)
  )

