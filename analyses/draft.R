
data$practice_duration_survey
data$practice_duration_logs
data$practice_duration_logs_months

data_duration <- data |> 
  dplyr::select(
    contains('ID'),
    contains('duration')
  ) |> 
  mutate(
    practice_duration_survey = as_factor(
      practice_duration_survey
      ) |> 
      fct_relevel(
        'No practice', 'Less than 1 month', '1-2 months', '3-4 months', 'Complete period'
        ),
    practice_duration_survey_numerical =
      case_when(
        practice_duration_survey == 'No practice' ~ 0,
        practice_duration_survey == 'Less than 1 month' ~ 0.5,
        practice_duration_survey == '1-2 months' ~ 1,
        practice_duration_survey == '3-4 months' ~ 3,
        practice_duration_survey == 'Complete period' ~ 5,
      ),
    practice_duration_logs_numerical =
      case_when(
        practice_duration_logs == 'No practice' ~ 0,
        practice_duration_logs == 'Less than 1 month' ~ 0.5,
        practice_duration_logs == '1-2 months' ~ 1,
        practice_duration_logs == '3-4 months' ~ 3,
        practice_duration_logs == 'Complete period' ~ 5,
      ),
    practice_duration_survey_correct = practice_duration_logs == practice_duration_survey,
    practice_duration_survey_estimation = case_when(
      practice_duration_survey_numerical > practice_duration_logs_numerical ~ 'Overestimate',
      practice_duration_survey_numerical < practice_duration_logs_numerical ~ 'Underestimate',
      practice_duration_survey_numerical == practice_duration_logs_numerical ~ 'Correct Estimate',
    )
  )

data_duration |> 
  summarise(
    overall_accuracy = mean(practice_duration_survey_correct)
  )

data_duration |> 
  group_by(
    group_ID
  ) |> 
  summarise(
    teacher_students = n(),
    teacher_accurateestimation = mean(as.numeric(practice_duration_survey_correct), na.rm = T),
    teacher_overestimation = 100*sum(practice_duration_survey_estimation == 'Overestimate') / teacher_students,
    teacher_underestimation = 100*sum(practice_duration_survey_estimation == 'Underestimate') / teacher_students,
  )

data_duration |> 
  group_by(
    school_ID
  ) |> 
  summarise(
    school_students = n(),
    school_accuracy = mean(as.numeric(practice_duration_survey_correct), na.rm = T)
  )

data_duration |>
  mutate(
    practice_duration_logs_months = round_to(
      practice_duration_logs_months, 
      )
    ) |> 
  ggplot(
    aes(
      x = practice_duration_logs_months,
      y = practice_duration_survey,
      color = practice_duration_survey_estimation
    )
  ) +
  # geom_point() +
  geom_count() +
  scale_size_area(
    max_size = 10,
    breaks = c(breaks_size, 100),
      name = legend_title
  ) +
  # scale_size_continuous(
  #   limits = c(1, 140),
  #   range = range_size,
  #   breaks = c(breaks_size, 100),
  #   name = legend_title
  # ) +
  scale_color_manual(
    values = c("Overestimate" = color_overestimate,
               "Underestimate" = color_underestimate,
               "Correct Estimate" = color_correctestimate
    ),
    name = 'Estimation'
  ) 
