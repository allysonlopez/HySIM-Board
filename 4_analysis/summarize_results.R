# 4_analysis/summarize_results.R
# Summaries for simulation output.

calculate_los_metrics <- function(arrivals) {
  arrivals %>%
    filter(finished == TRUE) %>%
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

summarize_resources <- function(resources) {
  resources %>%
    arrange(resource, time) %>%
    group_by(resource) %>%
    mutate(
      next_time = lead(time, default = max(time, na.rm = TRUE)),
      duration = pmax(0, next_time - time)
    ) %>%
    summarise(
      max_queue = max(queue, na.rm = TRUE),
      max_server = max(server, na.rm = TRUE),
      avg_server = weighted.mean(server, w = duration, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_attributes <- function(attributes, attribute_key) {
  attributes %>%
    filter(key == attribute_key) %>%
    count(value) %>%
    mutate(prop = n / sum(n))
}

calculate_observed_process_baseline <- function(first_seen_empirical_data, workup_empirical_data) {
  first_seen <- first_seen_empirical_data %>%
    summarise(
      mean_first_seen_min = mean(as.numeric(duration_min), na.rm = TRUE),
      median_first_seen_min = median(as.numeric(duration_min), na.rm = TRUE)
    )
  
  workup <- workup_empirical_data %>%
    summarise(
      mean_workup_min = mean(as.numeric(duration_min), na.rm = TRUE),
      median_workup_min = median(as.numeric(duration_min), na.rm = TRUE)
    )
  
  tibble(
    baseline_type = "approximate_process_time_only",
    mean_first_seen_min = first_seen$mean_first_seen_min,
    mean_workup_min = workup$mean_workup_min,
    approximate_mean_first_seen_plus_workup_min =
      first_seen$mean_first_seen_min + workup$mean_workup_min,
    median_first_seen_min = first_seen$median_first_seen_min,
    median_workup_min = workup$median_workup_min,
    approximate_median_first_seen_plus_workup_min =
      first_seen$median_first_seen_min + workup$median_workup_min
  )
}