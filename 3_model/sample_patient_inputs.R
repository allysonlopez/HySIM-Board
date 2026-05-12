# 3_model/sample_patient_inputs.R
# All patient-level random sampling lives here.

encode_acuity <- function(x) {
  x <- as.character(x)
  if (x %in% c("1", "2", "3", "4", "5")) return(as.numeric(x))
  return(NA_real_)
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
    TRUE ~ NA_real_
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
  
  acuity <- encode_acuity(acuity_raw)
  complexity <- encode_complexity(complexity_raw)
  
  # Fallbacks keep the simulation running, but missing values are no longer coded as 0.
  if (is.na(acuity)) acuity <- 3
  if (is.na(complexity)) complexity <- 4
  
  c(
    acuity = acuity,
    complexity_bucket = complexity
  )
}

sample_first_seen_delay <- function(first_seen_empirical_data,
                                    first_seen_summary_data,
                                    acuity,
                                    scale_factor = 0.35) {
  # The input first-seen time already contains real-world waiting/crowding.
  # Because this DES also creates queues explicitly, we use a scaled residual delay
  # so we do not double-count all observed waiting.
  empirical_rows <- first_seen_empirical_data %>%
    filter(as.character(triage_priority) == as.character(acuity), !is.na(duration_min))
  
  if (nrow(empirical_rows) > 0) {
    return(max(1, as.numeric(safe_sample(empirical_rows$duration_min)) * scale_factor))
  }
  
  summary_rows <- first_seen_summary_data %>%
    filter(as.character(triage_priority) == as.character(acuity))
  
  if (nrow(summary_rows) == 0) {
    summary_rows <- first_seen_summary_data %>% filter(triage_priority == "UNKNOWN")
  }
  
  max(1, as.numeric(summary_rows$median_min[1]) * scale_factor)
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

sample_between_median_and_p90 <- function(median_value, p90_value) {
  median_value <- as.numeric(median_value)
  p90_value <- as.numeric(p90_value)
  
  if (is.na(median_value) || median_value <= 0) return(0)
  if (is.na(p90_value) || p90_value <= median_value) return(max(1, median_value))
  
  # Lognormal parameterized so median and p90 approximately match the input table.
  sigma <- log(p90_value / median_value) / qnorm(0.90)
  max(1, rlnorm(1, meanlog = log(median_value), sdlog = sigma))
}

sample_imaging_duration <- function(imaging_probability_data, imaging_duration_data, acuity) {
  rows <- imaging_probability_data %>%
    filter(as.character(triage_priority) == as.character(acuity))
  
  if (nrow(rows) == 0) {
    rows <- imaging_probability_data %>% filter(triage_priority == "UNKNOWN")
  }
  
  if (nrow(rows) == 0) return(0)
  
  needs_imaging_prob <- as.numeric(rows$needs_imaging_prob[1])
  if (is.na(needs_imaging_prob)) needs_imaging_prob <- 0
  
  needs_imaging <- rbinom(1, 1, needs_imaging_prob)
  if (needs_imaging == 0) return(0)
  
  modality_probs <- as.numeric(c(rows$xr_prob[1], rows$ct_prob[1], rows$mri_prob[1], rows$us_prob[1]))
  modality_probs[is.na(modality_probs)] <- 0
  if (sum(modality_probs) <= 0) return(0)
  
  modality <- sample(c("XR", "CT", "MRI", "US"), size = 1, prob = modality_probs)
  duration_row <- imaging_duration_data %>% filter(imaging_type == modality)
  if (nrow(duration_row) == 0) return(0)
  
  sample_between_median_and_p90(
    duration_row$total_imaging_median_min[1],
    duration_row$total_imaging_p90_min[1]
  )
}

sample_consult_duration <- function(consult_probability_data, acuity) {
  rows <- consult_probability_data %>%
    filter(as.character(triage_priority) == as.character(acuity))
  
  if (nrow(rows) == 0) {
    rows <- consult_probability_data %>% filter(triage_priority == "UNKNOWN")
  }
  
  if (nrow(rows) == 0 || !("needs_consult_prob" %in% names(rows))) return(0)
  
  consult_prob <- as.numeric(rows$needs_consult_prob[1])
  if (is.na(consult_prob)) consult_prob <- 0
  
  needs_consult <- rbinom(1, 1, consult_prob)
  if (needs_consult == 0) return(0)
  
  # Placeholder until a consult-duration table is available.
  # Mean is roughly 90-150 minutes with a long right tail.
  sample_between_median_and_p90(90, 240)
}

sample_admission_flag <- function(acuity, complexity_bucket) {
  # Placeholder disposition model until an admission probability table is available.
  # Higher-acuity and higher-complexity patients are more likely to be admitted.
  admit_prob <- dplyr::case_when(
    acuity <= 2 ~ 0.45,
    acuity == 3 & complexity_bucket >= 5 ~ 0.35,
    acuity == 3 & complexity_bucket == 4 ~ 0.22,
    acuity == 4 & complexity_bucket >= 5 ~ 0.18,
    TRUE ~ 0.08
  )
  rbinom(1, 1, admit_prob)
}

sample_boarding_duration <- function(acuity, complexity_bucket) {
  # Placeholder boarding delay from admit decision to inpatient bed placement.
  # This is intentionally separated from generic ED workup so the model can report boarding.
  median_boarding <- dplyr::case_when(
    acuity <= 2 ~ 360,
    complexity_bucket >= 5 ~ 300,
    TRUE ~ 180
  )
  p90_boarding <- dplyr::case_when(
    acuity <= 2 ~ 900,
    complexity_bucket >= 5 ~ 720,
    TRUE ~ 480
  )
  sample_between_median_and_p90(median_boarding, p90_boarding)
}
