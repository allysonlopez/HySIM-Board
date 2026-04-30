# data use from interarrival and case-mix; need to have day_of_week_num and hour_of_day

sample_time_rows <- function (data, current_sim_time_min, current_quarter) {
  
  current_day_of_week <- floor(current_sim_time_min / 1440) %% 7 + 1
  
  current_minute_in_day <- current_sim_time_min %% 1440
  current_hour <- floor(current_minute_in_day / 60)
  
  matching_time_rows <- data[
    data$day_of_week_num == current_day_of_week &
      data$hour_of_day == current_hour &
      data$quarter == current_quarter,
  ]
  
  if (nrow(matching_time_rows) == 0) {
    warning("Using the whole datasets") 
    matching_time_rows <- data
    
  }
  
  return (matching_time_rows)
  
}
  
