sample_workup_time <- function(workup_empirical, complexity_code, config) {
  complexity_bucket <- convert_complexity_code_to_bucket(complexity_code)
  
  matching_rows <- workup_empirical %>%
    dplyr::filter(.data$complexity_bucket == complexity_bucket)
  
  if (nrow(matching_rows) == 0) {
    matching_rows <- workup_empirical %>%
      dplyr::filter(.data$complexity_bucket == "UNKNOWN")
  }
  
  if (nrow(matching_rows) == 0) {
    matching_rows <- workup_empirical
  }
  
  if (nrow(matching_rows) == 0) {
    return(config$default_workup_duration_min)
  }
  
  duration <- sample(
    x = matching_rows$duration_min,
    size = 1
  )
  
  duration <- suppressWarnings(as.numeric(duration))
  
  if (is.na(duration) || duration <= 0) {
    return(config$default_workup_duration_min)
  }
  
  pmin(duration, config$max_workup_duration_min)
}