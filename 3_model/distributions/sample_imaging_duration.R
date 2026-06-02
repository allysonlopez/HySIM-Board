# 3_model/distributions/sample_imaging_duration.R
#
# Imaging duration sampling helpers

convert_imaging_type_code_to_name <- function(imaging_type_code) {
  dplyr::case_when(
    imaging_type_code == 1 ~ "XR",
    imaging_type_code == 2 ~ "CT",
    imaging_type_code == 3 ~ "MRI",
    imaging_type_code == 4 ~ "US",
    TRUE ~ "none"
  )
}

get_config_value_or_default <- function(config, field_name, default_value) {
  if (is.null(config)) {
    return(default_value)
  }
  
  if (!field_name %in% names(config)) {
    return(default_value)
  }
  
  value <- config[[field_name]]
  
  if (is.null(value) || is.na(value)) {
    return(default_value)
  }
  
  value
}

sample_imaging_acquisition_duration <- function(imaging_duration,
                                                imaging_type_code,
                                                config) {
  imaging_type <- convert_imaging_type_code_to_name(imaging_type_code)
  
  if (imaging_type == "none") {
    return(0)
  }
  
  matching_row <- imaging_duration %>%
    dplyr::filter(.data$imaging_type == imaging_type) %>%
    safe_first_row()
  
  default_total_imaging_min <- get_config_value_or_default(
    config = config,
    field_name = "default_total_imaging_min",
    default_value = 120
  )
  
  max_total_imaging_min <- get_config_value_or_default(
    config = config,
    field_name = "max_total_imaging_min",
    default_value = 720
  )
  
  if (is.null(matching_row)) {
    return(default_total_imaging_min)
  }
  
  total_median_min <- suppressWarnings(
    as.numeric(matching_row$total_imaging_median_min)
  )
  
  total_p90_min <- suppressWarnings(
    as.numeric(matching_row$total_imaging_p90_min)
  )
  
  if (
    is.na(total_median_min) ||
    is.na(total_p90_min) ||
    total_median_min <= 0 ||
    total_p90_min <= total_median_min
  ) {
    return(default_total_imaging_min)
  }
  
  sample_from_weibull(
    median_min = total_median_min,
    p90_min = total_p90_min,
    max_allowed_min = max_total_imaging_min
  )
}

sample_imaging_interpretation_duration <- function(imaging_duration,
                                                   imaging_type_code,
                                                   config) {
  
  # imaging duration is already sampled in sample_imaging_acquisition_duration()
  # returning 0 to avoids double-counting 
  return(0)
}