sample_patient_attribute_value <- function(case_mix_table, attribute_name, current_time_min = 0, current_quarter = NULL) {
  current_hour <- floor((current_time_min %% 1440) / 60)
  current_day <- floor(current_time_min / 1440) %% 7 + 1
  
  matching_rows <- case_mix_table %>%
    dplyr::filter(.data$attribute_name == attribute_name)
  
  if ("hour_of_day" %in% names(matching_rows)) {
    matching_rows <- matching_rows %>%
      dplyr::filter(as.integer(.data$hour_of_day) == current_hour)
  }
  
  if ("day_of_week_num" %in% names(matching_rows)) {
    matching_rows <- matching_rows %>%
      dplyr::filter(as.integer(.data$day_of_week_num) == current_day)
  }
  
  if (!is.null(current_quarter) && "quarter" %in% names(matching_rows)) {
    quarter_filtered_rows <- matching_rows %>%
      dplyr::filter(as.character(.data$quarter) == as.character(current_quarter))
    
    if (nrow(quarter_filtered_rows) > 0) {
      matching_rows <- quarter_filtered_rows
    }
  }
  
  if (nrow(matching_rows) == 0) {
    matching_rows <- case_mix_table %>%
      dplyr::filter(.data$attribute_name == attribute_name)
  }
  
  if (nrow(matching_rows) == 0) {
    return(NA_character_)
  }
  
  probabilities <- normalize_probabilities(matching_rows$probability)
  
  sample(
    x = as.character(matching_rows$attribute_value),
    size = 1,
    prob = probabilities
  )
}

convert_triage_priority_to_code <- function(triage_priority) {
  triage_priority <- as.character(triage_priority)
  triage_number <- suppressWarnings(as.integer(triage_priority))
  
  if (!is.na(triage_number)) {
    return(triage_number)
  }
  
  dplyr::case_when(
    stringr::str_detect(stringr::str_to_lower(triage_priority), "1") ~ 1L,
    stringr::str_detect(stringr::str_to_lower(triage_priority), "2") ~ 2L,
    stringr::str_detect(stringr::str_to_lower(triage_priority), "3") ~ 3L,
    stringr::str_detect(stringr::str_to_lower(triage_priority), "4") ~ 4L,
    stringr::str_detect(stringr::str_to_lower(triage_priority), "5") ~ 5L,
    TRUE ~ 0L
  )
}

convert_complexity_bucket_to_code <- function(complexity_bucket) {
  complexity_bucket <- stringr::str_to_lower(as.character(complexity_bucket))
  
  dplyr::case_when(
    complexity_bucket == "minimal" ~ 1L,
    complexity_bucket == "straightforward" ~ 2L,
    complexity_bucket == "low" ~ 3L,
    complexity_bucket == "moderate" ~ 4L,
    complexity_bucket == "high" ~ 5L,
    complexity_bucket %in% c("critical_care", "critical care", "critical") ~ 6L,
    TRUE ~ 0L
  )
}

convert_complexity_code_to_bucket <- function(complexity_code) {
  dplyr::case_when(
    complexity_code == 1 ~ "minimal",
    complexity_code == 2 ~ "straightforward",
    complexity_code == 3 ~ "low",
    complexity_code == 4 ~ "moderate",
    complexity_code == 5 ~ "high",
    complexity_code == 6 ~ "critical_care",
    TRUE ~ "UNKNOWN"
  )
}

convert_arrival_mode_to_code <- function(arrival_mode) {
  arrival_mode <- stringr::str_to_lower(as.character(arrival_mode))
  
  dplyr::case_when(
    stringr::str_detect(arrival_mode, "walk") ~ 1L,
    stringr::str_detect(arrival_mode, "ambulance|ems|als|bls") ~ 2L,
    stringr::str_detect(arrival_mode, "transfer") ~ 3L,
    TRUE ~ 0L
  )
}

convert_age_group_to_code <- function(age_group) {
  age_group <- stringr::str_to_lower(as.character(age_group))
  
  dplyr::case_when(
    stringr::str_detect(age_group, "<18|0_17|under") ~ 1L,
    stringr::str_detect(age_group, "18") ~ 2L,
    stringr::str_detect(age_group, "65") ~ 3L,
    stringr::str_detect(age_group, "85") ~ 4L,
    TRUE ~ 0L
  )
}

convert_behavioral_health_flag_to_code <- function(behavioral_health_flag) {
  behavioral_health_flag <- stringr::str_to_lower(as.character(behavioral_health_flag))
  
  dplyr::case_when(
    behavioral_health_flag %in% c("1", "true", "yes", "y") ~ 1L,
    behavioral_health_flag %in% c("0", "false", "no", "n") ~ 0L,
    TRUE ~ 0L
  )
}

assign_patient_attributes <- function(case_mix_table, current_time_min = 0, current_quarter = NULL, ...) {
  triage_priority_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "acuity",
    current_time_min = current_time_min,
    current_quarter = current_quarter
  )
  
  complexity_bucket_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "complexity_bucket",
    current_time_min = current_time_min,
    current_quarter = current_quarter
  )
  
  arrival_mode_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "arrival_mode",
    current_time_min = current_time_min,
    current_quarter = current_quarter
  )
  
  age_group_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "age_group",
    current_time_min = current_time_min,
    current_quarter = current_quarter
  )
  
  behavioral_health_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "behavioral_health_flag",
    current_time_min = current_time_min,
    current_quarter = current_quarter
  )
  
  c(
    triage_priority = convert_triage_priority_to_code(triage_priority_raw),
    complexity_code = convert_complexity_bucket_to_code(complexity_bucket_raw),
    arrival_mode_code = convert_arrival_mode_to_code(arrival_mode_raw),
    age_group_code = convert_age_group_to_code(age_group_raw),
    behavioral_health_flag = convert_behavioral_health_flag_to_code(behavioral_health_raw)
  )
}