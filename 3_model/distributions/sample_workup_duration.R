# how long does general workup take?

sample_workup_duration <- function(workup_summary_data, patient_complexity_bucket) {
  
  complexity_text <- dplyr::case_when(
    patient_complexity_bucket == 1 ~ "minimal",
    patient_complexity_bucket == 2 ~ "straightforward",
    patient_complexity_bucket == 3 ~ "low",
    patient_complexity_bucket == 4 ~ "moderate",
    patient_complexity_bucket == 5 ~ "high",
    patient_complexity_bucket == 6 ~ "critical_care",
    TRUE ~ "UNKNOWN"
  )
  
  matching_row <- workup_summary_data[
    workup_summary_data$complexity_bucket == complexity_text,
  ]
  
  if (nrow(matching_row) == 0) {
    matching_row <- workup_summary_data[
      workup_summary_data$complexity_bucket == "UNKNOWN",
    ]
  }
  
  max(1, matching_row$median_min[1])
}