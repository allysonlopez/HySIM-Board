source("2_prep/load_packages.R")
unconstrained_results <- purrr::map_dfr(1:20, function(rep_id) {
  
  sim_day <- run_one_day_simulation(
    quarter_current = 1,
    dow_current = 1
  )
  
  tibble(
    replication = rep_id,
    model = "unconstrained",
    
    n_patients = nrow(sim_day),
    mean_total_duration = mean(sim_day$total_duration),
    median_total_duration = median(sim_day$total_duration)
  )
})

constrained_results <- purrr::map_dfr(1:20, function(rep_id) {
  
  sim_day <- run_one_day_simulation_with_rooms(
    quarter_current = 1,
    dow_current = 1,
    core_capacity = 43
  )
  
  tibble(
    replication = rep_id,
    model = "43_room_constraint",
    
    n_patients = nrow(sim_day),
    mean_total_duration = mean(sim_day$total_duration),
    median_total_duration = median(sim_day$total_duration),
    mean_room_wait = mean(sim_day$room_wait_duration),
    percent_waited = mean(sim_day$room_wait_duration > 0)
  )
})

comparison_results <- bind_rows(
  unconstrained_results,
  constrained_results
)

comparison_results

ggplot(
  comparison_results,
  aes(x = model,
      y = mean_total_duration,
      fill = model)
) +
  geom_boxplot() +
  labs(
    title = "Average Total ED Duration by Model Type",
    x = "Model",
    y = "Mean Total Duration (Minutes)"
  ) +
  theme_minimal()