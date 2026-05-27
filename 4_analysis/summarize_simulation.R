build_patient_outcome_table <- function(sim_output) {
  arrivals <- sim_output$arrivals
  attributes <- sim_output$attributes
  config <- sim_output$config
  
  if (nrow(arrivals) == 0) {
    return(tibble::tibble())
  }
  
  attribute_table <- attributes %>%
    dplyr::select(.data$name, .data$key, .data$value) %>%
    tidyr::pivot_wider(
      names_from = .data$key,
      values_from = .data$value,
      values_fn = dplyr::last
    )
  
  patient_table <- arrivals %>%
    dplyr::left_join(attribute_table, by = "name") %>%
    dplyr::mutate(
      imaging_duration_min = 
        dplyr::coalesce(.data$imaging_acquisition_duration_min, 0) +
        dplyr::coalesce(.data$imaging_interpretation_duration_min, 0),
      
      los_min = dplyr::if_else(.data$finished, .data$end_time - .data$start_time, NA_real_),
      after_warmup = .data$start_time >= get_warmup_minutes(config),
      in_analysis_window = .data$start_time >= get_analysis_start_minutes(config) &
        .data$start_time < get_analysis_end_minutes(config),
      complexity_bucket = convert_complexity_code_to_bucket(.data$complexity_code),
      care_area = decode_care_area(.data$care_area_code),
      disposition = decode_disposition(.data$disposition_code),
      imaging_type = convert_imaging_type_code_to_name(.data$imaging_type_code),
      consult_group = convert_consult_group_code_to_name(.data$consult_group_code),
      has_imaging = .data$needs_imaging == 1,
      has_consult = .data$needs_consult == 1,
      has_boarding_delay = .data$boarding_delay_min > 0
    )
  
  patient_table
}

summarize_los <- function(patient_table) {
  patient_table %>%
    dplyr::filter(.data$in_analysis_window, .data$finished) %>%
    dplyr::summarise(
      completed_patients = dplyr::n(),
      mean_los_min = mean(.data$los_min, na.rm = TRUE),
      median_los_min = median(.data$los_min, na.rm = TRUE),
      p75_los_min = stats::quantile(.data$los_min, 0.75, na.rm = TRUE),
      p90_los_min = stats::quantile(.data$los_min, 0.90, na.rm = TRUE),
      p95_los_min = stats::quantile(.data$los_min, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_patient_flow <- function(patient_table) {
  patient_table %>%
    dplyr::filter(.data$in_analysis_window) %>%
    dplyr::summarise(
      total_patients = dplyr::n(),
      finished_patients = sum(.data$finished, na.rm = TRUE),
      unfinished_patients = sum(!.data$finished, na.rm = TRUE),
      imaging_patients = sum(.data$has_imaging, na.rm = TRUE),
      consult_patients = sum(.data$has_consult, na.rm = TRUE),
      boarded_or_obs_patients = sum(.data$has_boarding_delay, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_by_group <- function(patient_table, group_column) {
  patient_table %>%
    dplyr::filter(.data$in_analysis_window, .data$finished) %>%
    dplyr::group_by(.data[[group_column]]) %>%
    dplyr::summarise(
      n = dplyr::n(),
      median_los_min = median(.data$los_min, na.rm = TRUE),
      p90_los_min = stats::quantile(.data$los_min, 0.90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(group_value = 1)
}

summarize_resources <- function(resource_table) {
  resource_table %>%
    dplyr::group_by(.data$resource) %>%
    dplyr::summarise(
      max_in_service = max(.data$server, na.rm = TRUE),
      max_queue = max(.data$queue, na.rm = TRUE),
      mean_in_service = mean(.data$server, na.rm = TRUE),
      mean_queue = mean(.data$queue, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_simulation <- function(sim_output) {
  patient_table <- build_patient_outcome_table(sim_output)
  
  list(
    patient_table = patient_table,
    los_summary = summarize_los(patient_table),
    flow_summary = summarize_patient_flow(patient_table),
    care_area_summary = summarize_by_group(patient_table, "care_area"),
    disposition_summary = summarize_by_group(patient_table, "disposition"),
    imaging_summary = summarize_by_group(patient_table, "imaging_type"),
    consult_summary = summarize_by_group(patient_table, "consult_group"),
    resource_summary = summarize_resources(sim_output$resources)
  )
}

save_simulation_tables <- function(summary_output, output_dir = "6_outputs/tables") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  readr::write_csv(summary_output$patient_table, file.path(output_dir, "patient_outcomes.csv"))
  readr::write_csv(summary_output$los_summary, file.path(output_dir, "los_summary.csv"))
  readr::write_csv(summary_output$flow_summary, file.path(output_dir, "flow_summary.csv"))
  readr::write_csv(summary_output$care_area_summary, file.path(output_dir, "care_area_summary.csv"))
  readr::write_csv(summary_output$disposition_summary, file.path(output_dir, "disposition_summary.csv"))
  readr::write_csv(summary_output$imaging_summary, file.path(output_dir, "imaging_summary.csv"))
  readr::write_csv(summary_output$consult_summary, file.path(output_dir, "consult_summary.csv"))
  readr::write_csv(summary_output$resource_summary, file.path(output_dir, "resource_summary.csv"))
}
