sample_first_seen_duration <- function(acuity_value) {
  first_seen_emp %>%
    filter(triage_priority == acuity_value) %>%
    pull(duration_min) %>%
    sample(1)
}

sample_workup_duration <- function(complexity_value) {
  workup_emp %>%
    filter(complexity_bucket == complexity_value) %>%
    pull(duration_min) %>%
    sample(1)
}

# Determine whether a patient needs imaging
sample_imaging_needed <- function(acuity_value) {
  
  row_current <- imaging_prob %>%
    filter(triage_priority == acuity_value)
  
  if(nrow(row_current) == 0) {
    stop("No imaging probability row found for this acuity")
  }
  
  rbinom(
    n = 1,
    size = 1,
    prob = row_current$needs_imaging_prob
  )
}


# Sample imaging modality
# only if patient needs imaging 
sample_imaging_modality <- function(acuity_value) {
  
  row_current <- imaging_prob %>%
    filter(triage_priority == acuity_value)
  
  if(nrow(row_current) == 0) {
    stop("No imaging modalilty row found for this acuity.")
  }
  
  sample(
    x = c("XR", "CT", "MRI", "US"),
    size = 1,
    prob = c(
      row_current$xr_prob,
      row_current$ct_prob,
      row_current$mri_prob,
      row_current$us_prob
    )
  )
}


# Sample imaging duration from empirical data
# For now, we use an exponential distribution based on the mean total imaging time
sample_imaging_duration <- function(imaging_type_value) {
  
  row_current <- imaging_time %>% filter(imaging_type == imaging_type_value)
  
  if (nrow(row_current) == 0) {
    stop("No imaging duration row found for this imaging type.")
  }
  
  rexp(
    n = 1,
    rate = 1/ row_current$total_imaging_mean_min
  )
}


# Determines whether a patient needs a consult
# Uses consult probability based on triage acuity
sample_consult_needed <- function(acuity_value) {
  
  row_current <- consult_prob %>%
    filter(triage_priority == acuity_value)
  
  if (nrow(row_current) == 0) {
    stop("No consult probability row found for this acuity.")
  }
  
  rbinom(
    n = 1,
    size = 1,
    prob = row_current$needs_consult_prob
  )
}


# Samples the consult group if a patient needs a consult
sample_consult_group <- function(acuity_value) {
  
  row_current <- consult_prob %>%
    filter(triage_priority == acuity_value)
  
  if (nrow(row_current) == 0) {
    stop("No consult group row found for this acuity.")
  }
  
  sample(
    x = c(
      "social_work_case_management",
      "psych_behavioral_health",
      "ancillary_support",
      "cardiology",
      "neuro_neurosurgery",
      "orthopedics",
      "surgery_specialty",
      "medicine_specialty",
      "other_specialty",
      "radiology_ir",
      "other_unknown"
    ),
    size = 1,
    prob = c(
      row_current$social_work_case_management_prob,
      row_current$psych_behavioral_health_prob,
      row_current$ancillary_support_prob,
      row_current$cardiology_prob,
      row_current$neuro_neurosurgery_prob,
      row_current$orthopedics_prob,
      row_current$surgery_specialty_prob,
      row_current$medicine_specialty_prob,
      row_current$other_specialty_prob,
      row_current$radiology_ir_prob,
      row_current$other_unknown_prob
    )
  )
}