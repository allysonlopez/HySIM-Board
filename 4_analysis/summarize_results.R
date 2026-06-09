# 04/summarize_results.R

# summarize ED resource use.
summarize_resources <- function(resources_analysis) {
  resources_analysis %>%
    group_by(resource) %>%
    summarise(
      max_queue = max(queue, na.rm = TRUE),
      max_server = max(server, na.rm = TRUE),
      avg_server = mean(server, na.rm = TRUE),
      utilization = mean(server, na.rm = TRUE) / max(server, na.rm = TRUE),
      .groups = "drop"
    )
}

calculate_los_metrics <- function(arrivals_analysis) {
  arrivals_analysis %>%
    mutate(los_minute = end_time - start_time) %>%
    summarise(
      n_finished = n(),
      mean_los_minute = mean(los_minute, na.rm = TRUE),
      median_los_minute = median(los_minute, na.rm = TRUE),
      p75_los_minute = quantile(los_minute, 0.75, na.rm = TRUE),
      p90_los_minute = quantile(los_minute, 0.90, na.rm = TRUE),
      p95_los_minute = quantile(los_minute, 0.95, na.rm = TRUE)
    )
}

summarize_attributes <- function(attributes_analysis, attribute_name) {
  attributes_analysis %>%
    filter(key == attribute_name) %>%
    count(value, name = "n") %>%
    mutate(prop = n / sum(n))
}

calculate_observed_process_baseline <- function(first_seen_empirical_data,
                                                workup_empirical_data) {
  tibble(
    baseline_type = "approximate_process_time",
    mean_first_seen_min = mean(first_seen_empirical_data$duration_min, na.rm = TRUE),
    mean_workup_min = mean(workup_empirical_data$duration_min, na.rm = TRUE),
    approximate_mean_first_seen_plus_workup_min =
      mean_first_seen_min + mean_workup_min,
    median_first_seen_min = median(first_seen_empirical_data$duration_min, na.rm = TRUE),
    median_workup_min = median(workup_empirical_data$duration_min, na.rm = TRUE),
    approximate_median_first_seen_plus_workup_min =
      median_first_seen_min + median_workup_min
  )
}

# create summary tables and validation metrics
create_summary_outputs <- function() {
  
  dir.create("5_outputs", showWarnings = FALSE)
  
  patients_per_day <<- tibble(start_time = arrival_times) %>%
    filter(
      start_time >= warmup_time,
      start_time < analysis_end_time
    ) %>%
    mutate(day = floor((start_time - warmup_time) / 1440) + 1) %>%
    count(day, name = "n_patients")
  
  patients_per_day_summary <<- patients_per_day %>%
    summarise(
      mean_patients_per_day = mean(n_patients),
      min_patients_per_day = min(n_patients),
      max_patients_per_day = max(n_patients)
    )
  
  los_metrics <<- calculate_los_metrics(arrivals_analysis)
  resource_summary <<- summarize_resources(resources_analysis)
  acuity_summary <<- summarize_attributes(attributes_analysis, "acuity")
  complexity_summary <<- summarize_attributes(attributes_analysis, "complexity_bucket")
  
  bed_wait_summary <<- arrivals_analysis %>%
    mutate(
      total_los_min = end_time - start_time,
      process_time_min = activity_time,
      bed_wait_time_min = total_los_min - process_time_min
    ) %>%
    summarise(
      mean_bed_wait_min = mean(bed_wait_time_min, na.rm = TRUE),
      median_bed_wait_min = median(bed_wait_time_min, na.rm = TRUE),
      p75_bed_wait_min = quantile(bed_wait_time_min, 0.75, na.rm = TRUE),
      p90_bed_wait_min = quantile(bed_wait_time_min, 0.90, na.rm = TRUE),
      max_bed_wait_min = max(bed_wait_time_min, na.rm = TRUE)
    )
  
  process_summary <<- attributes_analysis %>%
    filter(key %in% c(
      "first_seen_duration",
      "consult_duration",
      "workup_duration",
      "imaging_duration"
    )) %>%
    group_by(key) %>%
    summarise(
      n = n(),
      mean_min = mean(value, na.rm = TRUE),
      median_min = median(value, na.rm = TRUE),
      p75_min = quantile(value, 0.75, na.rm = TRUE),
      p90_min = quantile(value, 0.90, na.rm = TRUE),
      max_min = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_min))
  
  consult_summary <<- attributes_analysis %>%
    filter(key == "consult_duration") %>%
    summarise(
      total_patients = n(),
      n_consults = sum(value > 0, na.rm = TRUE),
      percent_consult = mean(value > 0, na.rm = TRUE),
      mean_consult_duration_min = mean(value[value > 0], na.rm = TRUE)
    )
  
  imaging_summary <<- attributes_analysis %>%
    filter(key == "imaging_duration") %>%
    summarise(
      total_patients = n(),
      n_imaging = sum(value > 0, na.rm = TRUE),
      percent_imaging = mean(value > 0, na.rm = TRUE),
      mean_imaging_duration_min = mean(value[value > 0], na.rm = TRUE)
    )
  
  observed_process_baseline <<- calculate_observed_process_baseline(
    first_seen_empirical_data,
    workup_empirical_data
  )
  
  create_validation_tables()
}


# create validation tables
create_validation_tables <- function() {
  
  historical_patients_per_day <- interarrival_data %>%
    filter(quarter == current_quarter) %>%
    summarise(
      historical_value = sum(arrivals_n, na.rm = TRUE) /
        (sum(hours_observed, na.rm = TRUE) / 24)
    ) %>%
    pull(historical_value)
  
  historical_first_provider_wait <- first_seen_empirical_data %>%
    filter(triage_priority != "UNKNOWN") %>%
    summarise(historical_value = mean(duration_min, na.rm = TRUE)) %>%
    pull(historical_value)
  
  historical_workup_duration <- workup_empirical_data %>%
    filter(complexity_bucket != "UNKNOWN") %>%
    summarise(historical_value = mean(duration_min, na.rm = TRUE)) %>%
    pull(historical_value)
  
  historical_imaging_duration <- imaging_duration_data %>%
    summarise(
      historical_value = weighted.mean(
        total_imaging_mean_min,
        n_obs,
        na.rm = TRUE
      )
    ) %>%
    pull(historical_value)
  
  historical_imaging_utilization <- imaging_probability_data %>%
    filter(triage_priority != "UNKNOWN") %>%
    summarise(
      historical_value = weighted.mean(
        needs_imaging_prob,
        n_obs,
        na.rm = TRUE
      ) * 100
    ) %>%
    pull(historical_value)
  
  historical_consult_utilization <- consult_probability_data %>%
    filter(triage_priority != "UNKNOWN") %>%
    summarise(
      historical_value = weighted.mean(
        needs_consult_prob,
        n_obs,
        na.rm = TRUE
      ) * 100
    ) %>%
    pull(historical_value)
  
  sim_first_provider_wait <- process_summary %>%
    filter(key == "first_seen_duration") %>%
    pull(mean_min)
  
  sim_workup_duration <- process_summary %>%
    filter(key == "workup_duration") %>%
    pull(mean_min)
  
  validation_summary <<- tibble(
    metric = c(
      "Patients per Day (patients/day)",
      "Mean First Provider Wait (min)",
      "Mean Workup Duration (min)",
      "Mean Imaging Duration (min)",
      "Imaging Utilization (%)",
      "Consult Utilization (%)"
    ),
    unit = c(
      "patients/day",
      "minutes",
      "minutes",
      "minutes",
      "percent",
      "percent"
    ),
    historical = c(
      historical_patients_per_day,
      historical_first_provider_wait,
      historical_workup_duration,
      historical_imaging_duration,
      historical_imaging_utilization,
      historical_consult_utilization
    ),
    simulated = c(
      patients_per_day_summary$mean_patients_per_day,
      sim_first_provider_wait,
      sim_workup_duration,
      imaging_summary$mean_imaging_duration_min,
      imaging_summary$percent_imaging * 100,
      consult_summary$percent_consult * 100
    )
  ) %>%
    mutate(
      historical = round(historical, 2),
      simulated = round(simulated, 2),
      percent_difference = round(
        100 * (simulated - historical) / historical,
        1
      ),
      absolute_error = abs(simulated - historical),
      absolute_percent_error = abs(simulated - historical) / historical * 100
    )
  
  overall_mape <<- mean(
    validation_summary$absolute_percent_error,
    na.rm = TRUE
  )
  
  mape_summary <<- validation_summary %>%
    select(metric, absolute_percent_error) %>%
    rename(MAPE = absolute_percent_error) %>%
    mutate(MAPE = round(MAPE, 1))
  
  write_csv(validation_summary, "5_outputs/validation_summary.csv")
  write_csv(mape_summary, "5_outputs/mape_summary.csv")
  
  create_triage_validation()
  create_complexity_validation()
}


# create triage validation table
create_triage_validation <- function() {
  
  historical_triage <- case_mix_data %>%
    filter(
      quarter == current_quarter,
      attribute_name == "acuity",
      attribute_value != "UNKNOWN"
    ) %>%
    group_by(attribute_value) %>%
    summarise(
      historical = sum(n_obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(historical_percent = 100 * historical / sum(historical)) %>%
    rename(triage_priority = attribute_value)
  
  simulated_triage <- attributes_analysis %>%
    filter(key == "acuity") %>%
    mutate(triage_priority = as.character(value)) %>%
    filter(triage_priority != "UNKNOWN") %>%
    count(triage_priority, name = "simulated") %>%
    mutate(simulated_percent = 100 * simulated / sum(simulated))
  
  triage_validation <<- historical_triage %>%
    full_join(simulated_triage, by = "triage_priority") %>%
    mutate(
      historical_percent = replace_na(historical_percent, 0),
      simulated_percent = replace_na(simulated_percent, 0),
      difference = simulated_percent - historical_percent
    ) %>%
    arrange(triage_priority)
  
  write_csv(triage_validation, "5_outputs/triage_validation.csv")
}


# create complexity validation table
create_complexity_validation <- function() {
  
  historical_complexity <- case_mix_data %>%
    filter(
      quarter == current_quarter,
      attribute_name == "complexity_bucket",
      attribute_value != "UNKNOWN"
    ) %>%
    group_by(attribute_value) %>%
    summarise(
      historical = sum(n_obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(historical_percent = 100 * historical / sum(historical)) %>%
    rename(complexity_bucket = attribute_value)
  
  simulated_complexity <- attributes_analysis %>%
    filter(key == "complexity_bucket") %>%
    mutate(complexity_bucket = decode_complexity(value)) %>%
    filter(complexity_bucket != "UNKNOWN") %>%
    count(complexity_bucket, name = "simulated") %>%
    mutate(simulated_percent = 100 * simulated / sum(simulated))
  
  complexity_validation <<- historical_complexity %>%
    full_join(simulated_complexity, by = "complexity_bucket") %>%
    mutate(
      historical_percent = replace_na(historical_percent, 0),
      simulated_percent = replace_na(simulated_percent, 0),
      difference = simulated_percent - historical_percent
    ) %>%
    arrange(complexity_bucket)
  
  write_csv(complexity_validation, "5_outputs/complexity_validation.csv")
}


# print all outputs
print_summary_outputs <- function() {
  
  cat("\n--- Patients Per Day by Analysis Day ---\n")
  print(patients_per_day)
  
  cat("\n--- Patients Per Day Summary ---\n")
  print(patients_per_day_summary)
  
  cat("\n--- Simulation LOS Metrics ---\n")
  print(los_metrics)
  
  cat("\n--- Resource Summary ---\n")
  print(resource_summary)
  
  cat("\n--- Bed Wait Summary ---\n")
  print(bed_wait_summary)
  
  cat("\n--- Acuity Mix ---\n")
  print(acuity_summary)
  
  cat("\n--- Complexity Mix ---\n")
  print(complexity_summary)
  
  cat("\n--- Process Summary ---\n")
  print(process_summary)
  
  cat("\n--- Consult Summary ---\n")
  print(consult_summary)
  
  cat("\n--- Imaging Summary ---\n")
  print(imaging_summary)
  
  cat("\n--- Observed Baseline from Input Data ---\n")
  print(observed_process_baseline)
  
  cat("\n--- Validation Summary ---\n")
  print(validation_summary)
  
  cat("\n--- MAPE Summary ---\n")
  print(mape_summary)
  
  cat(
    "\nOverall Operational Validation MAPE:",
    round(overall_mape, 2),
    "%\n"
  )
  
  cat("\n--- Triage Validation ---\n")
  print(triage_validation)
  
  cat("\n--- Complexity Validation ---\n")
  print(complexity_validation)
}