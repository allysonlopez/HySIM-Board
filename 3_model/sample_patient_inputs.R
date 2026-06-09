# 3_model/sample_patient_inputs.R

# convert acuity to a number
encode_acuity <- function(x) {
  x <- as.character(x)
  if (x %in% c("1", "2", "3", "4", "5")) return(as.numeric(x))
  return(NA_real_)
}


# convert complexity to a number
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


# convert complexity back to text
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


# sample one patient attribute from case mix data
sample_attribute <- function(case_mix_data, current_time, current_quarter, attribute_name_target) {
  rows <- filter_time_block(case_mix_data, current_time, current_quarter) %>%
    filter(attribute_name == attribute_name_target)
  
  if (nrow(rows) == 0) {
    rows <- case_mix_data %>% filter(attribute_name == attribute_name_target)
  }
  
  safe_sample(rows$attribute_value, rows$probability)
}


# assign main patient attributes used by model
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


# sample time from arrival to first provider
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

# sample consult time
sample_consult_duration <- function(consult_probability_data, acuity) {
  rows <- consult_probability_data %>%
    filter(as.character(triage_priority) == as.character(acuity))
  
  if (nrow(rows) == 0) {
    rows <- consult_probability_data %>%
      filter(triage_priority == "UNKNOWN")
  }
  
  if (nrow(rows) == 0) return(0)
  
  prob <- as.numeric(rows$needs_consult_prob[1])
  if (is.na(prob)) prob <- 0
  
  needs_consult <- rbinom(1, 1, prob)
  
  if (needs_consult == 1) {
    return(240)   # 4 hours
  } else {
    return(0)
  }
}

# sample ED workup time
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


# sample duration
sample_between_median_and_p90 <- function(median_value, p90_value) {
  median_value <- as.numeric(median_value)
  p90_value <- as.numeric(p90_value)
  
  if (is.na(median_value) || median_value <= 0) return(0)
  if (is.na(p90_value) || p90_value <= median_value) return(max(1, median_value))
  
  sigma <- log(p90_value / median_value) / qnorm(0.90)
  max(1, rlnorm(1, meanlog = log(median_value), sdlog = sigma))
}


# sample imaging time
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
