# 3_model/generate_arrivals.R


# create patient arrival times for each hour of the sim
create_arrival_times <- function(interarrival_data, current_quarter, sim_days) {
  arrival_times <- c()
  
  for (day in 0:(sim_days - 1)) {
    for (hour in 0:23) {
      
      # get matching arrival data for current sim hour
      current_time <- day * 1440 + hour * 60
      rows <- filter_time_block(interarrival_data, current_time, current_quarter)
      row <- rows[sample.int(nrow(rows), 1), ]
      
      # convert average minutes between arrivals into arrivals per hour
      mean_time <- as.numeric(row$mean_interarrival_min[1])
      arrival_rate <- ifelse(is.na(mean_time) || mean_time <= 0, 0, 60 / mean_time)
      
      # generate the number of arrivals for current sim hour
      n_arrivals <- rpois(1, arrival_rate)
      
      # randomly spread arrivals across the hour
      if (n_arrivals > 0) {
        new_times <- current_time + runif(n_arrivals, 0, 60)
        arrival_times <- c(arrival_times, new_times)
      }
    }
  }
  
  sort(arrival_times)
}


# gives the time until the next arrival
make_interarrival_function <- function(arrival_times) {
  
  # stop if no arrivals were generated
  if (length(arrival_times) == 0) {
    stop("No arrival times generated.")
  }
  
  # calculate time between arrivals
  interarrival_times <- c(arrival_times[1], diff(arrival_times))
  index <- 0
  
  function() {
    index <<- index + 1
    
    # break when all arrivals have been used
    if (index > length(interarrival_times)) {
      return(-1)
    }
    
    max(0.001, interarrival_times[index])
  }
}