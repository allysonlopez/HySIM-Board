# Runs a simple patient-level simulation for a fixed number of patients
# This version does not include resources yet.
# It is used to test patient-level logic before adding the full DES engine.

run_simple_patient_simulation <- function(n_patients,
                                          quarter_current,
                                          dow_current,
                                          hour_current) {
  
  sim_results <- purrr::map_dfr(
    1:n_patients,
    ~ simulate_one_patient_path(
      quarter_current = quarter_current,
      dow_current = dow_current,
      hour_current = hour_current
    )
  )
  
  sim_results
}

# Runs a one-day simulation with a core room capacity constraint.
# Patients must wait if all core ED rooms are occupied.

run_one_day_simulation_with_rooms <- function(quarter_current,
                                              dow_current,
                                              start_hour = 0,
                                              sim_minutes = 24 * 60,
                                              core_capacity = 43) {
  
  # Generate arrivals for the day
  arrivals <- generate_arrival_schedule(
    sim_minutes = sim_minutes,
    quarter_current = quarter_current,
    dow_current = dow_current,
    start_hour = start_hour
  )
  
  # Each room has a next available time.
  # At the start, all 43 rooms are available at time 0.
  room_available_times <- rep(0, core_capacity)
  
  patient_results <- list()
  
  for (i in 1:nrow(arrivals)) {
    
    patient_hour <- arrivals$arrival_hour[i]
    arrival_time <- arrivals$arrival_time[i]
    
    # Simulate patient attributes and sampled durations
    patient_result <- simulate_one_patient_path(
      quarter_current = quarter_current,
      dow_current = dow_current,
      hour_current = patient_hour
    )
    
    # Sampled time when the patient would be ready to be first seen
    candidate_first_seen_time <- arrival_time +
      patient_result$first_seen_duration
    
    # Find the room that becomes available soonest
    room_id <- which.min(room_available_times)
    next_available_room_time <- room_available_times[room_id]
    
    # Actual first seen time must wait for both:
    # 1. the patient's sampled first-seen delay
    # 2. an available room
    first_seen_time <- max(
      candidate_first_seen_time,
      next_available_room_time
    )
    
    # Extra wait caused only by room capacity
    room_wait_duration <- first_seen_time - candidate_first_seen_time
    
    # Calculate downstream event times
    workup_end_time <- first_seen_time +
      patient_result$workup_duration
    
    imaging_end_time <- workup_end_time +
      patient_result$imaging_duration
    
    exit_time <- imaging_end_time
    
    # Once this patient exits, the room becomes available again
    room_available_times[room_id] <- exit_time
    
    # Save patient result
    patient_results[[i]] <- patient_result %>%
      mutate(
        patient_id = arrivals$patient_id[i],
        arrival_time = arrival_time,
        arrival_hour = patient_hour,
        
        candidate_first_seen_time = candidate_first_seen_time,
        first_seen_time = first_seen_time,
        room_wait_duration = room_wait_duration,
        
        room_id = room_id,
        workup_end_time = workup_end_time,
        imaging_end_time = imaging_end_time,
        exit_time = exit_time
      )
  }
  
  bind_rows(patient_results)
}