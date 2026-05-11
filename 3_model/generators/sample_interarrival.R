# 3_model/generators/sample_interarrival.R

source("3_model/generators/helper_functions.R")

create_empirical_arrival_times <- function(interarrival_data,
                                           current_quarter,
                                           sim_days = 1) {
  
  all_arrival_times <- c()
  
  for (day_index in 0:(sim_days - 1)) {
    
    for (hour in 0:23) {
      
      current_sim_time_min <- day_index * 1440 + hour * 60
      
      matching_rows <- sample_time_rows(
        data = interarrival_data,
        current_sim_time_min = current_sim_time_min,
        current_quarter = current_quarter
      )
      
      selected_row <- matching_rows[sample.int(nrow(matching_rows), 1), ]
      
      # Since the data is hourly, estimate the number of arrivals in this hour.
      # If arrivals_n is total across multiple observed hours, use rate * 1 hour.
      expected_arrivals <- selected_row$arrival_rate_per_hour
      
      if (is.na(expected_arrivals) || expected_arrivals < 0) {
        expected_arrivals <- 0
      }
      
      # Use rounded empirical hourly rate for MVP.
      n_arrivals_this_hour <- round(expected_arrivals)
      
      if (n_arrivals_this_hour > 0) {
        
        # We do not know exact minutes, so distribute uniformly within the hour.
        arrival_minutes_within_hour <- runif(
          n = n_arrivals_this_hour,
          min = 0,
          max = 60
        )
        
        absolute_arrival_times <- current_sim_time_min + arrival_minutes_within_hour
        
        all_arrival_times <- c(all_arrival_times, absolute_arrival_times)
      }
    }
  }
  
  sort(all_arrival_times)
}

make_interarrival_function <- function(arrival_times) {
  
  if (length(arrival_times) == 0) {
    stop("No arrival times were generated.")
  }
  
  interarrival_times <- c(arrival_times[1], diff(arrival_times))
  index <- 0
  
  function() {
    index <<- index + 1
    
    if (index > length(interarrival_times)) {
      return(-1)
    }
    
    max(0.001, interarrival_times[index])
  }
}