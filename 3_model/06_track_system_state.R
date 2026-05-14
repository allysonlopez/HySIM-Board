# Tracks how many patients are in each model state at each time point
# This uses the simulated start and end times for each patient stage.

track_system_state <- function(sim_results,
                               time_step = 1,
                               max_time = 24 * 60) {
  
  time_grid <- seq(0, max_time, by = time_step)
  
  purrr::map_dfr(time_grid, function(t) {
    
    tibble(
      time = t,
      
      waiting_first_seen = sum(
        sim_results$arrival_time <= t &
          sim_results$first_seen_time > t
      ),
      
      in_workup = sum(
        sim_results$first_seen_time <= t &
          sim_results$workup_end_time > t
      ),
      
      in_imaging = sum(
        sim_results$workup_end_time <= t &
          sim_results$imaging_end_time > t &
          sim_results$needs_imaging == 1
      ),
      
      exited = sum(
        sim_results$exit_time <= t
      )
    ) %>%
      mutate(
        total_in_ed = waiting_first_seen + in_workup + in_imaging
      )
  })
}

# Tracks core room occupancy using the MVP capacity assumption.
# Core room use begins at first_seen_time and ends at exit_time.

track_core_room_capacity <- function(sim_results,
                                     time_step = 1,
                                     max_time = 24 * 60,
                                     core_capacity = 43) {
  
  time_grid <- seq(0, max_time, by = time_step)
  
  purrr::map_dfr(time_grid, function(t) {
    
    occupied_rooms <- sum(
      sim_results$first_seen_time <= t &
        sim_results$exit_time > t
    )
    
    tibble(
      time = t,
      occupied_core_rooms = occupied_rooms,
      core_capacity = core_capacity,
      rooms_available = core_capacity - occupied_rooms,
      over_capacity = occupied_rooms > core_capacity
    )
  })
}