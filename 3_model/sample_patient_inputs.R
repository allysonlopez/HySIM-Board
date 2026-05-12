# 3_model/sample_patient_inputs.R

#   Store all patient-level sampling functions for the cleaned model.
#
#   After a patient arrives, the simulation needs to decide:
#     1) how sick/urgent the patient is: acuity
#     2) how complicated the patient is: complexity bucket
#     3) how long until the patient is first seen
#     4) how long the generic ED workup takes
#     5) whether imaging happens and, if yes, how long it takes


#   Converts an acuity value from the input data into a numeric value from 1 to 5.
encode_acuity <- function(x) {
  x <- as.character(x)
  if (x %in% c("1", "2", "3", "4", "5")) return(as.numeric(x))
  return(NA_real_)
}


#   Converts text complexity categories into ordered numeric values.
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


#   Converts numeric complexity values back into the text labels used in the
#   workup-duration input tables.

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


#   Samples one patient attribute, such as acuity or complexity, from the case-mix
#   probability table for the current time block.

sample_attribute <- function(case_mix_data, current_time, current_quarter, attribute_name_target) {
  rows <- filter_time_block(case_mix_data, current_time, current_quarter) %>%
    filter(attribute_name == attribute_name_target)
  
  if (nrow(rows) == 0) {
    rows <- case_mix_data %>% filter(attribute_name == attribute_name_target)
  }
  
  safe_sample(rows$attribute_value, rows$probability)
}


#   Assigns the two patient attributes currently used by the MVP model:
#     1) acuity
#     2) complexity_bucket

assign_patient_attributes <- function(case_mix_data, current_time, current_quarter) {
  acuity_raw <- sample_attribute(case_mix_data, current_time, current_quarter, "acuity")
  complexity_raw <- sample_attribute(case_mix_data, current_time, current_quarter, "complexity_bucket")
  
  acuity <- encode_acuity(acuity_raw)
  complexity <- encode_complexity(complexity_raw)
  
  if (is.na(acuity)) acuity <- 3
  if (is.na(complexity)) complexity <- 4
  
  c(
    acuity = acuity,
    complexity_bucket = complexity
  )
}


#   Samples the time from ED arrival to first practitioner contact for one patient.

sample_first_seen_delay <- function(first_seen_empirical_data,
                                    first_seen_summary_data,
                                    acuity,
                                    scale_factor = 0.35) {
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


#   Samples the generic ED workup duration for one patient.
#
#   Workup duration depends on complexity. A minimal-complexity patient should
#   generally have a shorter workup than a high-complexity or critical-care
#   patient.

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


#   Samples a positive duration using a lognormal distribution whose median and
#   90th percentile approximately match the values from an input summary table.

sample_between_median_and_p90 <- function(median_value, p90_value) {
  median_value <- as.numeric(median_value)
  p90_value <- as.numeric(p90_value)
  
  if (is.na(median_value) || median_value <= 0) return(0)
  if (is.na(p90_value) || p90_value <= median_value) return(max(1, median_value))
  
  sigma <- log(p90_value / median_value) / qnorm(0.90)
  max(1, rlnorm(1, meanlog = log(median_value), sdlog = sigma))
}


#   Simulates the imaging subprocess for one patient.
#
# Modeling logic:
#   1) Use the patient's acuity to find the probability that imaging is needed.
#   2) Randomly decide whether imaging happens.
#   3) If imaging happens, sample the modality: XR, CT, MRI, or US.
#   4) Use the modality-specific duration table to sample total imaging time.
#   5) If imaging does not happen, return 0 minutes.
#

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
