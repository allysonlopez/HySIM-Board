sample_imaging_decision <- function(imaging_probability_data, patient_acuity) {
  
  matching_row <- imaging_probability_data[
    as.character(imaging_probability_data$triage_priority) == as.character(patient_acuity),
  ]
  
  if (nrow(matching_row) == 0) {
    matching_row <- imaging_probability_data[
      imaging_probability_data$triage_priority == "UNKNOWN",
    ]
  }
  
  imaging_prob <- matching_row$needs_imaging_prob
  
  gets_imaging <- rbinom(
    n = 1,
    size = 1,
    prob = imaging_prob
  )
  
  if (gets_imaging == 0) {
    return("none")
  }
  
  modality <- sample(
    x = c("XR", "CT", "MRI", "US"),
    size = 1,
    replace = TRUE,
    prob = c(
      matching_row$xr_prob,
      matching_row$ct_prob,
      matching_row$mri_prob,
      matching_row$us_prob
    )
  )
  
  return(modality)
}