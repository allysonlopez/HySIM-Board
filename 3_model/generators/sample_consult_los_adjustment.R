sample_consult_los_adjustment <- function(consult_los_data,
                                          patient_acuity,
                                          consult_group) {
  
  if (consult_group == "none") {
    return(0)
  }
  
  # MVP approach:
  # Compare median LOS for patients with consults vs. no consults.
  # If possible, stratify by acuity.
  
  consult_rows <- consult_los_data[
    as.character(consult_los_data$triage_priority) == as.character(patient_acuity) &
      consult_los_data$consult_flag == 1,
  ]
  
  no_consult_rows <- consult_los_data[
    as.character(consult_los_data$triage_priority) == as.character(patient_acuity) &
      consult_los_data$consult_flag == 0,
  ]
  
  if (nrow(consult_rows) == 0 || nrow(no_consult_rows) == 0) {
    return(0)
  }
  
  consult_median_los <- median(consult_rows$ed_los_min, na.rm = TRUE)
  no_consult_median_los <- median(no_consult_rows$ed_los_min, na.rm = TRUE)
  
  adjustment <- consult_median_los - no_consult_median_los
  
  return(max(0, adjustment))
}