# Function to generate the next patient interarrival time
# using an exponential distribution based on the
# historical mean interarrival time for a given time block

sample_interarrival_time <- function(quarter_current,
                                     dow_current,
                                     hour_current) {
  
  # Pull the matching row from the interarrival table
  row_current <- interarrival %>%
    filter(
      quarter == quarter_current,
      day_of_week_num == dow_current,
      hour_of_day == hour_current
    )
  
  # Stop the function if no matching row exists
  if (nrow(row_current) == 0) {
    stop("No interarrival row found for this time block.")
  }
  
  # Generate one exponential interarrival time
  # rate = 1 / mean
  rexp(
    n = 1,
    rate = 1 / row_current$mean_interarrival_min
  )
}


# Generates patient arrival times over a simulation window
# Arrival gaps are sampled using the interarrival distribution
# for the current quarter, day of week, and hour.

generate_arrival_schedule <- function(sim_minutes,
                                      quarter_current,
                                      dow_current,
                                      start_hour = 0) {
  
  # Store arrival times here
  arrival_times <- c()
  
  # Simulation clock starts at 0 minutes
  current_time <- 0
  
  while (current_time < sim_minutes) {
    
    # Convert current simulation time into an hour of day
    hour_current <- (start_hour + floor(current_time / 60)) %% 24
    
    # Sample time until next patient arrives
    next_gap <- sample_interarrival_time(
      quarter_current = quarter_current,
      dow_current = dow_current,
      hour_current = hour_current
    )
    
    # Move simulation clock forward
    current_time <- current_time + next_gap
    
    # Save arrival if it occurs inside the simulation window
    if (current_time < sim_minutes) {
      arrival_times <- c(arrival_times, current_time)
    }
  }
  
  tibble(
    patient_id = seq_along(arrival_times),
    arrival_time = arrival_times,
    arrival_hour = (start_hour + floor(arrival_times / 60)) %% 24
  )
}