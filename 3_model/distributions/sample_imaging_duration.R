convert_imaging_type_code_to_name <- function(imaging_type_code) {
  dplyr::case_when(
    imaging_type_code == 1 ~ "XR",
    imaging_type_code == 2 ~ "CT",
    imaging_type_code == 3 ~ "MRI",
    imaging_type_code == 4 ~ "US",
    TRUE ~ "none"
  )
}

sample_imaging_acquisition_duration <- function(imaging_duration, imaging_type_code, config) {
  imaging_type <- convert_imaging_type_code_to_name(imaging_type_code)
  
  if (imaging_type == "none") {
    return(0)
  }
  
  matching_row <- imaging_duration %>%
    dplyr::filter(.data$imaging_type == imaging_type) %>%
    safe_first_row()
  
  if (is.null(matching_row)) {
    return(config$default_imaging_acquisition_min)
  }
  
  sample_from_weibull(
    median_min = matching_row$acquisition_median_min,
    p90_min = matching_row$acquisition_p90_min,
    max_allowed_min = config$max_imaging_acquisition_min
  )
}

sample_imaging_interpretation_duration <- function(imaging_duration, imaging_type_code, config) {
  imaging_type <- convert_imaging_type_code_to_name(imaging_type_code)
  
  if (imaging_type == "none") {
    return(0)
  }
  
  matching_row <- imaging_duration %>%
    dplyr::filter(.data$imaging_type == imaging_type) %>%
    safe_first_row()
  
  if (is.null(matching_row)) {
    return(config$default_imaging_interpretation_min)
  }
  
  sample_from_weibull(
    median_min = matching_row$interpretation_median_min,
    p90_min = matching_row$interpretation_p90_min,
    max_allowed_min = config$max_imaging_interpretation_min
  )
}