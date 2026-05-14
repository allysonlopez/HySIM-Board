# Creates a simulation that starts before 12AM.
# The warm-up period allows patients to already be in the ED
# when the official evaluation window begins.

run_simulation_with_warmup <- function(quarter_current,
                                       dow_current,
                                       warmup_minutes = 6 * 60,
                                       sim_minutes = 24 * 60) {
  
  # Run simulation for warm-up period plus the actual day
  full_sim <- run_one_day_simulation(
    quarter_current = quarter_current,
    dow_current = dow_current,
    start_hour = 18,
    sim_minutes = warmup_minutes + sim_minutes
  )
  
  # Shift time so that 12AM becomes time 0
  full_sim <- full_sim %>%
    mutate(
      arrival_time = arrival_time - warmup_minutes,
      first_seen_time = first_seen_time - warmup_minutes,
      workup_end_time = workup_end_time - warmup_minutes,
      imaging_end_time = imaging_end_time - warmup_minutes,
      exit_time = exit_time - warmup_minutes
    )
  
  # Keep patients who are present during the evaluation day
  eval_sim <- full_sim %>%
    filter(
      exit_time > 0,
      arrival_time < sim_minutes
    )
  
  eval_sim
}