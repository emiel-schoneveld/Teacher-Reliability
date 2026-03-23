# Transform and combine data
## By Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(here)
library(lubridate)

# Load data ----
## Data from teacher questionnaire
### Raw data
data_questionnaire_raw <- read_xlsx(
  here('input/raw_data/Vragenlijst_nameting_schoongemaakt_anoniem_2-9-2024.xlsx'),
  col_names = F,skip = 2
  )[,-28]

### Columnnames
data_for_colnames <- read_xlsx(
  here('input/raw_data/Vragenlijst_nameting_schoongemaakt_anoniem_2-9-2024.xlsx'),
  col_names = T
  )
colnames_questionnaire_raw <- colnames(data_for_colnames)[-28]

## Data from practice logs
data_logs <- read_xlsx(
  here('input/raw_data/logs_lessons_anonymous.xlsx')
  )

## Student characteristics data
data_characteristics_old <- read_xlsx(
  here('input/raw_data/achtergrondgegevens.xlsx')
  )[,-1]

## Student test data=
data_wordreading_all <- read_xlsx(
  here('input/raw_data/data_toetsgegevens.xlsx')
  )

# Set global variables ----
## Weeks of practice within a semester
weeks_of_practice = 20

## Start and end of semester
start_of_semester = as.Date("2024-02-01")
end_of_semester = as.Date("2024-06-30")

## Session length maximum and minimum
min_session_length = 2
max_session_length = 60

# Transform data_questionaire ----
## Give correct column names ----
colnames(data_questionnaire_raw) <- colnames_questionnaire_raw

## Select and rename columns ----
data_questionnaire <- data_questionnaire_raw |> 
  dplyr::select(
    student_ID, EndDate,
    contains('practice') & !contains('_1') & !contains('tutor')
  ) |> 
  rename(
    date_survey = EndDate,
    # practice_freq_planned = practice_freq_1,
    practice_freq_survey = practice_freq_2,
    # practice_length_planned = practice_dur_plan_1,
    practice_length_survey = practice_dur_plan_2,
    practice_duration_survey = practice_period,
    practice_type_survey = practice_type,
    practice_explanation_survey = practice_explanation
  )

## Translate variables ----
data_questionnaire <- data_questionnaire |> 
  mutate(
    practice_duration_survey = recode(
      practice_duration_survey,
      `Minder dan 1 maand` = 'Less than 1 month',
      `1-2 maanden` = '1-2 months',
      `3-4 maanden` = '3-4 months',
      `De hele periode` = 'Complete period'
    )
  )

## Set missing values to 0 ----
data_questionnaire <- data_questionnaire |> 
  mutate(
    practice_duration_survey = ifelse(
      practice_type_survey == "Naast het klassikale aanbod niet extra geoefend",
      'No practice',
      practice_duration_survey
    ),
    practice_freq_survey = ifelse(
      practice_type_survey == "Naast het klassikale aanbod niet extra geoefend",
      0,
      practice_freq_survey
    ),
    practice_length_survey = ifelse(
      practice_type_survey == "Naast het klassikale aanbod niet extra geoefend",
      0,
      practice_length_survey
    )
  )

## Compute weeks and ciitime columns ----
### Recode duration column to calculate prop
data_questionnaire <- data_questionnaire |> 
  mutate(
    # Recode duration column to reflect number of months
    practice_duration_survey_recoded =  recode(
      practice_duration_survey,
      'No practice' = '0',
      'Less than 1 month' = '0.25',
      '1-2 months' = '1.5',
      '3-4 months' = '3.5',
      'Complete period' = '4.75'
    ) |> 
      as.numeric(),
    practice_duration_survey_prop = practice_duration_survey_recoded / 5
  )

### Calculate weeks and ccitime
data_questionnaire <- data_questionnaire |> 
  mutate(
    practice_weeks_survey = practice_duration_survey_prop * weeks_of_practice,
    practice_ciitime_survey = practice_weeks_survey * practice_freq_survey * practice_length_survey * (1/60)
  )

### Omit columns used for calculations
data_questionnaire <- data_questionnaire |> 
  dplyr::select(
    !contains('survey_')
  )

# Transform data_wordreading ----
## Rename columns and convert to numeric ----
# data_wordreading_all <- data_wordreading_all |> 
#   rename(
#     student_ID = Leerlingnummer,
#     wordreading_version_pre = Woordleestoets_versie_pre,
#     wordreading_version_post = Woordleestoets_versie_post,
#     wordreading_score_pre = Woordleestoets_vaardigheidsscore_pre,
#     wordreading_score_post = Woordleestoets_vaardigheidsscore_post
#   ) |> 
#   mutate(
#     wordreading_score_pre = wordreading_score_pre |> as.numeric(),
#     wordreading_score_post = wordreading_score_post |> as.numeric()
#   )

## Select columns ----
# data_wordreading_all <- data_wordreading_all |> 
#   dplyr::select(
#     student_ID,
#     contains('wordreading') & (contains('pre') | contains('post'))
#   )

## Remove all values that are not a DMT ----
### For premeasurement
# data_wordreading_pre <- data_wordreading_all |> 
#   dplyr::select(
#     student_ID,
#     contains('pre')
#   ) |> 
#   filter(
#     wordreading_version_pre == 'DMT'
#   )

## Join for filtered post measurement with pre measurement ----
# data_wordreading <- data_wordreading_all |> 
#   dplyr::select(
#     student_ID,
#     contains('post')
#   ) |> 
#   filter(
#     wordreading_version_post == 'DMT'
#   ) |> 
#   full_join(
#     data_wordreading_pre
#   )

# Transform data_characteristics ----
## Rename and transform columns
data_characteristics <- data_characteristics_old |> 
  rename(
    student_ID = Leerlingnummer,
    grade = Leerjaar,
    condition = Conditie,
    group_ID_old = Groep,
    gender = Geslacht,
    dateofbirth_numeric = Geboortedatum,
    language_home = Thuistaal,
    language_preference = Voorkeurstaal,
    school_ID = school
  ) |> 
  mutate(
    grade = grade |> as.numeric(),
    condition = condition |> as_factor(),
    gender = gender |> as_factor(),
    language_home = language_home |> as_factor(),
    language_preference = language_preference |> as_factor(),
  ) |> 
  mutate(
    grade = grade - 2,
    condition = recode(
      condition,
      'flitsen' = "flashcard",
      'flitsen&rijtjes' = 'combination',
      'rijtjes' = 'listreading',
      'controle' = 'control'
    ),
    gender = recode(
      gender,
      "jongen" = "boy",
      'meisje' = "girl",
      'X' = 'X'
    ),
    group_ID = paste0(
      school_ID, '-', group_ID_old
    )
  ) |> 
  dplyr::select(
    !group_ID_old
  )

## Computing age
data_characteristics <- data_characteristics|>
  mutate(
    dateofbirth_numeric = as.numeric(dateofbirth_numeric),
    dateofbirth = as.Date(dateofbirth_numeric, origin = "1899-12-30"),
    age = difftime(
      as.Date("2024-02-01"),
      dateofbirth,
      units = 'days'), 
    age = as.numeric(age) /365.25
  ) |> 
  dplyr::select(
    !dateofbirth_numeric
  )

# Transform data_logs ----
## Rename columns
data_logs <- data_logs |> 
  rename(
    # lesson_dose = TotalLessonItems,
    lesson_form = LessonType
  )

## Rename variables ----
data_logs <- data_logs |> 
  rename(
    student_ID = Leerlingnummer
  )

## Recode missing values ----
data_logs <- data_logs |> 
  mutate(
    EndDate = na_if(EndDate, "NULL")
  )

## Filter data ----
data_logs <- data_logs |> 
  filter(
    lesson_form %in% c("ResearchFlits", "Flits", "ResearchRowRead", "RowRead"),
    !is.na(EndDate),
    StartDate >= start_of_semester,
    EndDate <= end_of_semester
  )

## Convert strings into date and time class ----
data_logs <- data_logs |> 
  mutate(
    lesson_date = as.Date.character(CreationDate),
    lesson_time_start = substring(StartDate, 0, 19) |> as.POSIXlt.character(tz = "CET"),
    lesson_time_end = substring(EndDate, 0, 19) |> as.POSIXlt.character(tz = "CET")
  )

## Compute lesson duration
data_logs <- data_logs |> 
  mutate(
    lesson_length = difftime(lesson_time_end, lesson_time_start,  units = "mins")
  )

## Summarise logs per day ----
data_logs_session_unfiltered <- data_logs |> 
  group_by(
    student_ID,
    lesson_date
  ) |> 
  summarise(
    session_starttime = min(lesson_time_start, na.rm = T),
    session_endtime = max(lesson_time_end, na.rm = T),
    # session_dose = sum(lesson_dose, na.rm = T),
    # session_length_readingtime = sum(lesson_length, na.rm = T)
  ) |> 
  ungroup()|> 
  mutate(
    session_length = difftime(session_endtime, session_starttime, units = "mins")
  )

## Filter by session length ----
data_logs_session <- data_logs_session_unfiltered |> 
  filter(
    session_length >= min_session_length,
    session_length <= max_session_length,
  )

### Calculate percentage of sessions that were removed and check if no students are omitted because of it
100 - ((nrow(data_logs_session) / nrow(data_logs_session_unfiltered)) * 100)
data_logs_session$student_ID |> unique() |> length()
data_logs_session_unfiltered$student_ID |> unique() |> length()

## Compute student level variables ----
data_logs_student <- data_logs_session |> 
  group_by(
    student_ID
  ) |> 
  summarise(
    practice_length_logs = mean(session_length),
    practice_length_logs = as.numeric(practice_length_logs),
    
    # practice_dose_logs = mean(session_dose, na.rm = T),
    # practice_dose_logs = as.numeric(practice_dose_logs),
    
    practice_ciitime_logs = sum(session_length, na.rm = T)/60,
    practice_ciitime_logs = as.numeric(practice_ciitime_logs),
    
    # practice_ciireadingtime_logs = sum(session_length_readingtime, na.rm = T)/60,
    # practice_ciireadingtime_logs = as.numeric(practice_ciireadingtime_logs),
    # 
    # practice_ciiwords_logs = sum(session_dose, na.rm = T),
    # practice_ciiwords_logs = as.numeric(practice_ciiwords_logs)
    
  )

### Compute and add duration ----
data_logs_student <- data_logs_session |> 
  group_by(
    student_ID
  ) |> 
  summarise(
    MIN = min(lesson_date, na.rm = T),
    MAX = max(lesson_date, na.rm = T),
    DIFF_days = difftime(MAX, MIN, units = "days") |> as.numeric(),
    practice_duration_logs_months = DIFF_days / (365/12)
  ) |> 
  mutate(
    practice_duration_logs = as.character(NA),
    practice_duration_logs = if_else(
      practice_duration_logs_months < 0.5,
      'Less than 1 month',
      practice_duration_logs,
      missing = NA
    ),
    practice_duration_logs = if_else(
      (practice_duration_logs_months >= 0.5) & (practice_duration_logs_months < 2.5),
      '1-2 months',
      practice_duration_logs,
      missing = NA
    ),
    practice_duration_logs = if_else(
      (practice_duration_logs_months >= 2.5) & (practice_duration_logs_months < 4.5),
      '3-4 months',
      practice_duration_logs,
      missing = NA
    ),
    practice_duration_logs = if_else(
      (practice_duration_logs_months >= 4.5),
      'Complete period',
      practice_duration_logs,
      missing = NA
    )
  ) |> 
  dplyr::select(
    student_ID, 
    contains('practice_duration_logs') & !contains('months')
  ) |> 
  right_join(
    y = data_logs_student
  )

### Add week number ----
data_logs_session <- data_logs_session |> 
  mutate(
    session_week = strftime(session_starttime, format = "%V") |> as.numeric()
  )

### Compute and add frequency ----
data_logs_student <- data_logs_session |> 
  group_by(
    student_ID,
    session_week
  ) |> 
  summarise(
    practice_freq_logs = n()
  ) |> 
  ungroup() |> 
  group_by(
    student_ID
  ) |> 
  summarise(
    practice_freq_logs = mean(practice_freq_logs, na.rm = T)
  ) |> 
  right_join(
    y = data_logs_student
  )

### Compute and add number of weeks practised ----
data_logs_student <- data_logs_session |> 
  group_by(
    student_ID,
    session_week
  ) |> 
  summarise(
    n()
  ) |> 
  group_by(
    student_ID
  ) |> 
  summarise(
    practice_weeks_logs = n()
  ) |> 
  right_join(
    y = data_logs_student
  )

# Combine data_logs_student, data_questionnaire, data_DMT and data_characteristics ----
data <- data_logs_student |> 
  inner_join(
    data_questionnaire
  ) |> 
  # left_join(
  #   data_wordreading
  # ) |> 
  left_join(
    data_characteristics
  )

# Inspect omitted students
data_all_potential <- data_logs_student |> 
  full_join(
    data_questionnaire
  ) |> 
  right_join(
    data_characteristics
    ) |>
  filter(
    group_ID %in% data$group_ID
  )

data_omitted <- data_all_potential |> 
  filter(
    !(student_ID %in% data$student_ID)
  )

nrow(data_all_potential)
nrow(data_omitted)
nrow(data)

# Pivot longer ----
data_long <- data |> 
  pivot_longer(
    contains('practice') & !(contains('type') | contains('explanation') | contains('duration')),
    names_to = 'measure',
    values_to = 'score',
    names_prefix = 'practice_'
  ) |>
  separate_wider_delim(
    measure,
    delim = "_",
    names = c('measure', 'report')
  )

# Save data ----
## Wide format
save(
  data,
  file = here('input/cleaned_data/data_wide.rds')
)

## Long format
save(
  data_long,
  file = here('input/cleaned_data/data_long.rds')
)