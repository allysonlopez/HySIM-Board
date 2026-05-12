# 3_model/generate_arrivals.R
# Creates patient arrival times, then converts them into interarrival times for simmer.

create_arrival_times <- function(interarrival_data, current_quarter, sim_days) {
  arrival_times <- c()
  
  for (day_index in 0:(sim_days - 1)) {
    for (hour in 0:23) {
      current_time <- day_index * 1440 + hour * 60
      rows <- filter_time_block(interarrival_data, current_time, current_quarter)
      selected_row <- rows[sample.int(nrow(rows), 1), ]
      
      # Use a Poisson draw so hourly arrivals vary naturally around the empirical rate.
      arrival_rate <- as.numeric(selected_row$arrival_rate_per_hour[1])
      if (is.na(arrival_rate) || arrival_rate < 0) arrival_rate <- 0
      n_arrivals <- rpois(1, lambda = arrival_rate)
      
      if (n_arrivals > 0) {
        arrival_times <- c(arrival_times, current_time + runif(n_arrivals, 0, 60))
      }
    }
  }
  
  sort(arrival_times)
}

make_interarrival_function <- function(arrival_times) {
  if (length(arrival_times) == 0) stop("No arrival times generated.")
  interarrival_times <- c(arrival_times[1], diff(arrival_times))
  i <- 0
  
  function() {
    i <<- i + 1
    if (i > length(interarrival_times)) return(-1)
    max(0.001, interarrival_times[i])
  }
}
