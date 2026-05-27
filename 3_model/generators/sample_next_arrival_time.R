sample_next_arrival_time <- function(interarrival_table, current_time_min, config) {
  matching_rows <- filter_rows_for_simulation_time(
    data = interarrival_table,
    current_time_min = current_time_min,
    config = config
  )

  selected_row <- matching_rows[sample.int(nrow(matching_rows), 1), ]
  arrival_rate_per_hour <- as.numeric(selected_row$arrival_rate_per_hour[1])
  fallback_mean_min <- as.numeric(selected_row$mean_interarrival_min[1])

  if (is.na(arrival_rate_per_hour) || arrival_rate_per_hour <= 0) {
    return(max(1, fallback_mean_min))
  }

  max(1, stats::rexp(1, rate = arrival_rate_per_hour / 60))
}
 