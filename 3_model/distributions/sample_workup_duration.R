sample_workup_time <- function(workup_summary, complexity_code, config) {
  complexity_bucket <- convert_complexity_code_to_bucket(complexity_code)
  
  matching_row <- workup_summary %>%
    dplyr::filter(.data$complexity_bucket == complexity_bucket) %>%
    safe_first_row()
  
  if (is.null(matching_row)) {
    matching_row <- workup_summary %>%
      dplyr::filter(.data$complexity_bucket == "UNKNOWN") %>%
      safe_first_row()
  }
  
  if (is.null(matching_row)) {
    return(config$default_workup_duration_min)
  }
  
  sample_from_weibull(
    median_min = matching_row$median_min,
    p90_min = matching_row$p90_min,
    max_allowed_min = config$max_workup_duration_min
  )
}
