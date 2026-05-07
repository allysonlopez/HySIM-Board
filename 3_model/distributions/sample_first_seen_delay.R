# how long does this atient wait before being seen

sample_first_seen_delay <- function(first_seen_summary_data, patient_acuity) {
  
  
  if (is.na(patient_acuity) || patient_acuity == 0) {
    matching_row <- first_seen_summary_data[first_seen_summary_data$triage_priority == "UNKNOWN", ]
  } else {
    matching_row <- first_seen_summary_data[
      as.character(first_seen_summary_data$triage_priority) == as.character(patient_acuity),
    ]
  }
  
  if (nrow(matching_row) == 0) {
    matching_row <- first_seen_summary_data[first_seen_summary_data$triage_priority == "UNKNOWN", ]
  }
  max(1, matching_row$median_min)
}