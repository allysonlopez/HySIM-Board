convert_consult_group_code_to_name <- function(consult_group_code) {
  dplyr::case_when(
    consult_group_code == 1 ~ "social_work_case_management",
    consult_group_code == 2 ~ "psych_behavioral_health",
    consult_group_code == 3 ~ "ancillary_support",
    consult_group_code == 4 ~ "cardiology",
    consult_group_code == 5 ~ "neuro_neurosurgery",
    consult_group_code == 6 ~ "orthopedics",
    consult_group_code == 7 ~ "surgery_specialty",
    consult_group_code == 8 ~ "medicine_specialty",
    consult_group_code == 9 ~ "other_specialty",
    consult_group_code == 10 ~ "radiology_ir",
    consult_group_code == 11 ~ "other_unknown",
    TRUE ~ "none"
  )
}

sample_consult_los_adjustment <- function(consult_group_code, config) {
  consult_group <- convert_consult_group_code_to_name(consult_group_code)
  
  if (consult_group == "none") {
    return(0)
  }
  
  sample_from_weibull(
    median_min = config$consult_los_adjustment_median_min,
    p90_min = config$consult_los_adjustment_p90_min,
    max_allowed_min = config$max_consult_adjustment_min
  )
}