is_unknown_attribute_value <- function(x) {
  x <- stringr::str_to_upper(stringr::str_squish(as.character(x)))
  is.na(x) | x %in% c("UNKNOWN", "UNK", "NA", "N/A", "")
}

sample_patient_attribute_value <- function(case_mix_table,
                                           attribute_name,
                                           current_time_min = 0,
                                           current_quarter = NULL,
                                           config = NULL,
                                           default_value = NA_character_,
                                           allow_unknown = TRUE) {
  target_attribute_name <- attribute_name
  
  if (!is.null(config)) {
    candidate_rows <- filter_rows_for_simulation_time(
      data = case_mix_table,
      current_time_min = current_time_min,
      config = config
    )
  } else {
    candidate_rows <- case_mix_table
    
    if (!is.null(current_quarter) && "quarter" %in% names(candidate_rows)) {
      candidate_rows <- candidate_rows %>%
        dplyr::filter(.data$quarter == .env$current_quarter)
    }
  }
  
  matching_rows <- candidate_rows %>%
    dplyr::filter(.data$attribute_name == .env$target_attribute_name)
  
  if (nrow(matching_rows) == 0) {
    matching_rows <- case_mix_table %>%
      dplyr::filter(.data$attribute_name == .env$target_attribute_name)
  }
  
  if (!allow_unknown) {
    matching_rows <- matching_rows %>%
      dplyr::filter(!is_unknown_attribute_value(.data$attribute_value))
  }
  
  if (nrow(matching_rows) == 0) {
    return(default_value)
  }
  
  matching_rows <- matching_rows %>%
    dplyr::mutate(
      probability_numeric = suppressWarnings(as.numeric(.data$probability)),
      n_obs_numeric = suppressWarnings(as.numeric(.data$n_obs)),
      row_weight = dplyr::case_when(
        !is.na(.data$probability_numeric) & .data$probability_numeric > 0 ~ .data$probability_numeric,
        !is.na(.data$n_obs_numeric) & .data$n_obs_numeric > 0 ~ .data$n_obs_numeric,
        TRUE ~ 0
      )
    ) %>%
    dplyr::filter(!is.na(.data$row_weight), .data$row_weight > 0)
  
  if (nrow(matching_rows) == 0) {
    return(default_value)
  }
  
  sample(
    x = as.character(matching_rows$attribute_value),
    size = 1,
    prob = matching_rows$row_weight / sum(matching_rows$row_weight)
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
    TRUE ~ 3L
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
    TRUE ~ 4L
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
  arrival_mode <- stringr::str_to_lower(stringr::str_squish(as.character(arrival_mode)))
  
  dplyr::case_when(
    arrival_mode == "self_presented" ~ 1L,
    arrival_mode == "ground_ambulance" ~ 2L,
    arrival_mode == "police_custody" ~ 3L,
    arrival_mode == "hospital_transport" ~ 4L,
    arrival_mode == "air_transport" ~ 5L,
    arrival_mode == "other_unknown" ~ 6L,
    TRUE ~ 6L
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

convert_behavioral_health_flag_to_code <- function(behavioral_health_value) {
  value <- stringr::str_squish(as.character(behavioral_health_value))
  
  invalid_values <- unique(value[!is.na(value) & !value %in% c("0", "1")])
  if (length(invalid_values) > 0) {
    stop(
      "Unexpected behavioral_health_flag label(s): ",
      paste(invalid_values, collapse = ", "),
      ". Expected only database labels: 0 or 1."
    )
  }
  
  dplyr::case_when(
    value == "1" ~ 1L,
    value == "0" ~ 0L,
    is.na(value) ~ 0L
  )
}

assign_patient_attributes <- function(case_mix_table,
                                      current_time_min = 0,
                                      current_quarter = NULL,
                                      config = NULL,
                                      ...) {
  triage_priority_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "acuity",
    current_time_min = current_time_min,
    current_quarter = current_quarter,
    config = config,
    default_value = "3",
    allow_unknown = FALSE
  )
  
  complexity_bucket_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "complexity_bucket",
    current_time_min = current_time_min,
    current_quarter = current_quarter,
    config = config,
    default_value = "moderate",
    allow_unknown = FALSE
  )
  
  arrival_mode_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "arrival_mode",
    current_time_min = current_time_min,
    current_quarter = current_quarter,
    config = config,
    default_value = "self_presented",
    allow_unknown = TRUE
  )

  age_group_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "age_group",
    current_time_min = current_time_min,
    current_quarter = current_quarter,
    config = config,
    default_value = "18-64",
    allow_unknown = TRUE
  )
  
  behavioral_health_raw <- sample_patient_attribute_value(
    case_mix_table = case_mix_table,
    attribute_name = "behavioral_health_flag",
    current_time_min = current_time_min,
    current_quarter = current_quarter,
    config = config,
    default_value = "0",
    allow_unknown = TRUE
  )
  
  c(
    triage_priority = convert_triage_priority_to_code(triage_priority_raw),
    complexity_code = convert_complexity_bucket_to_code(complexity_bucket_raw),
    arrival_mode_code = convert_arrival_mode_to_code(arrival_mode_raw),
    age_group_code = convert_age_group_to_code(age_group_raw),
    behavioral_health_flag = convert_behavioral_health_flag_to_code(behavioral_health_raw)
  )
}