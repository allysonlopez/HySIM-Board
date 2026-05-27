get_simulation_day_hour <- function(current_time_min, config) {
  start_offset_min <- (config$start_day_of_week_num - 1) * 1440 +
    config$start_hour_of_day * 60
  total_min <- start_offset_min + current_time_min

  list(
    day_of_week_num = floor(total_min / 1440) %% 7 + 1,
    hour_of_day = floor((total_min %% 1440) / 60)
  )
}

filter_rows_for_simulation_time <- function(data, current_time_min, config) {
  sim_time <- get_simulation_day_hour(current_time_min, config)

  matching_rows <- data %>%
    dplyr::filter(
      .data$quarter == config$current_quarter,
      .data$day_of_week_num == sim_time$day_of_week_num,
      .data$hour_of_day == sim_time$hour_of_day
    )

  if (nrow(matching_rows) == 0) {
    warning("No rows matched the current simulated time block. Falling back to all rows.")
    matching_rows <- data
  }
  
  matching_rows
}
