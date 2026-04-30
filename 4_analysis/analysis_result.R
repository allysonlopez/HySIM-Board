calculate_los_metrics <- function(arrivals) {
  arrivals %>%
    mutate(los_minute = end_time - start_time) %>%
    summarise(
      n_patients = n(),
      mean_los_minute = mean(los_minute, na.rm = TRUE),
      median_los_minute = median(los_minute, na.rm = TRUE),
      p75_los_minute = quantile(los_minute, 0.75, na.rm = TRUE),
      p90_los_minute = quantile(los_minute, 0.90, na.rm = TRUE),
      p95_los_minute = quantile(los_minute, 0.95, na.rm = TRUE)
    )
}

summarize_resources <- function(resources) {
  resources %>%
    group_by(resource) %>%
    summarise(
      max_queue = max(queue, na.rm = TRUE),
      max_server = max(server, na.rm = TRUE),
      avg_server = mean(server, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_attributes <- function(attributes, attribute_key) {
  attributes %>%
    filter(key == attribute_key) %>%
    count(value) %>%
    mutate(prop = n / sum(n))
}
