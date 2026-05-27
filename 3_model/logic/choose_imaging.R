choose_imaging_attributes <- function(imaging_probability, triage_priority) {
  matching_row <- imaging_probability %>%
    dplyr::filter(as.character(.data$triage_priority) == as.character(triage_priority)) %>%
    safe_first_row()
  
  if (is.null(matching_row)) {
    return(c(needs_imaging = 0, imaging_type_code = 0))
  }
  
  imaging_probability_value <- as.numeric(matching_row$needs_imaging_prob)
  if (is.na(imaging_probability_value)) {
    imaging_probability_value <- 0
  }
  imaging_probability_value <- min(max(imaging_probability_value, 0), 1)
  
  needs_imaging <- stats::rbinom(1, size = 1, prob = imaging_probability_value)
  
  if (needs_imaging == 0) {
    return(c(needs_imaging = 0, imaging_type_code = 0))
  }
  
  modality_probabilities <- normalize_probabilities(c(
    matching_row$xr_prob,
    matching_row$ct_prob,
    matching_row$mri_prob,
    matching_row$us_prob
  ))
  
  imaging_type_code <- sample(1:4, size = 1, prob = modality_probabilities)
  
  c(needs_imaging = 1, imaging_type_code = imaging_type_code)
}
