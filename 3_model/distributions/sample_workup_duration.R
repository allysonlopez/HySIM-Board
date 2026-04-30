sample_workup_duration <- function(workup_summary_data, patient_complexity_bucket) {
  matching_row <- workup_summary_data[
    workup_summary_data$complexity_bucket == patient_complexity_bucket,
  ]
  
  if (nrow(matching_row) == 0) {
    matching_row <- workup_summary_data[workup_summary_data$complexity_bucket == "UNKNOWN", ]
  }
  
  max(1, matching_row$median_min)
}
