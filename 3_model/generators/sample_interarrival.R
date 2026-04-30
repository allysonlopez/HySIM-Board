source("3_model/generators/helper_functions.R")

sample_interarrival <- function(data, current_time, current_quarter) {
  matching_time_rows <- sample_time_rows(
    data = data,
    current_sim_time_min = current_time,
    current_quarter = current_quarter
  )
  
  if (nrow(matching_time_rows) == 0) {
    matching_time_rows <- data
    stop("No matching rows.")
  }
  
  selected_row <- matching_time_rows[sample.int(nrow(matching_time_rows), 1), ]
  
  arrival_rate_per_hour <- selected_row$arrival_rate_per_hour
  
  if (is.na(arrival_rate_per_hour) || arrival_rate_per_hour <= 0) {
    return(max(1, selected_row$mean_interarrival_min))
  }
  
  sampled_minutes <- rexp(
    n = 1,
    rate = arrival_rate_per_hour / 60
  )
  
  max(1, sampled_minutes)
}
