# 3_model/sample_patient_inputs.R
# All patient-level random sampling lives here.

encode_acuity <- function(x) {
  x <- as.character(x)
  if (x %in% c("1", "2", "3", "4", "5")) return(as.numeric(x))
  return(0)
}

encode_complexity <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "minimal" ~ 1,
    x == "straightforward" ~ 2,
    x == "low" ~ 3,
    x == "moderate" ~ 4,
    x == "high" ~ 5,
    x == "critical_care" ~ 6,
    TRUE ~ 0
  )
}

decode_complexity <- function(x) {
  dplyr::case_when(
    x == 1 ~ "minimal",
    x == 2 ~ "straightforward",
    x == 3 ~ "low",
    x == 4 ~ "moderate",
    x == 5 ~ "high",
    x == 6 ~ "critical_care",
    TRUE ~ "UNKNOWN"
  )
}

sample_attribute <- function(case_mix_data, current_time, current_quarter, attribute_name_target) {
  rows <- filter_time_block(case_mix_data, current_time, current_quarter) %>%
    filter(attribute_name == attribute_name_target)
  
  if (nrow(rows) == 0) {
    rows <- case_mix_data %>% filter(attribute_name == attribute_name_target)
  }
  
  safe_sample(rows$attribute_value, rows$probability)
}

assign_patient_attributes <- function(case_mix_data, current_time, current_quarter) {
  acuity_raw <- sample_attribute(case_mix_data, current_time, current_quarter, "acuity")
  complexity_raw <- sample_attribute(case_mix_data, current_time, current_quarter, "complexity_bucket")
  
  c(
    acuity = encode_acuity(acuity_raw),
    complexity_bucket = encode_complexity(complexity_raw)
  )
}

sample_first_seen_delay <- function(first_seen_empirical_data, first_seen_summary_data, acuity) {
  # Prefer empirical encounter-level data when available.
  empirical_rows <- first_seen_empirical_data %>%
    filter(as.character(triage_priority) == as.character(acuity), !is.na(duration_min))
  
  if (nrow(empirical_rows) > 0) {
    return(max(1, as.numeric(safe_sample(empirical_rows$duration_min))))
  }
  
  # Fallback to summary median.
  summary_rows <- first_seen_summary_data %>%
    filter(as.character(triage_priority) == as.character(acuity))
  
  if (nrow(summary_rows) == 0) {
    summary_rows <- first_seen_summary_data %>% filter(triage_priority == "UNKNOWN")
  }
  
  max(1, as.numeric(summary_rows$median_min[1]))
}

sample_workup_duration <- function(workup_empirical_data, workup_summary_data, complexity_bucket) {
  complexity_text <- decode_complexity(complexity_bucket)
  
  empirical_rows <- workup_empirical_data %>%
    filter(complexity_bucket == complexity_text, !is.na(duration_min))
  
  if (nrow(empirical_rows) > 0) {
    return(max(1, as.numeric(safe_sample(empirical_rows$duration_min))))
  }
  
  summary_rows <- workup_summary_data %>%
    filter(complexity_bucket == complexity_text)
  
  if (nrow(summary_rows) == 0) {
    summary_rows <- workup_summary_data %>% filter(complexity_bucket == "UNKNOWN")
  }
  
  max(1, as.numeric(summary_rows$median_min[1]))
}

sample_imaging_duration <- function(imaging_probability_data, imaging_duration_data, acuity) {
  rows <- imaging_probability_data %>%
    filter(as.character(triage_priority) == as.character(acuity))
  
  if (nrow(rows) == 0) {
    rows <- imaging_probability_data %>% filter(triage_priority == "UNKNOWN")
  }
  
  needs_imaging <- rbinom(1, 1, as.numeric(rows$needs_imaging_prob[1]))
  if (needs_imaging == 0) return(0)
  
  modality <- sample(
    c("XR", "CT", "MRI", "US"),
    size = 1,
    prob = c(rows$xr_prob[1], rows$ct_prob[1], rows$mri_prob[1], rows$us_prob[1])
  )
  
  duration_row <- imaging_duration_data %>% filter(imaging_type == modality)
  if (nrow(duration_row) == 0) return(0)
  
  max(1, as.numeric(duration_row$total_imaging_median_min[1]))
}

sample_consult_flag <- function(consult_probability_data, acuity) {
  rows <- consult_probability_data %>%
    filter(as.character(triage_priority) == as.character(acuity))
  
  if (nrow(rows) == 0) {
    rows <- consult_probability_data %>% filter(triage_priority == "UNKNOWN")
  }
  
  rbinom(1, 1, as.numeric(rows$needs_consult_prob[1]))
}
