run_one_simulation <- function(input_data, config, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  env <- build_simulation_environment(config)
  patient_trajectory <- build_patient_trajectory(
    input_data = input_data,
    env = env,
    config = config
  )
  
  env %>%
    simmer::add_generator(
      name_prefix = "patient",
      trajectory = patient_trajectory,
      distribution = function() {
        sample_next_arrival_time(
          interarrival_table = input_data$interarrival_table,
          current_time_min = simmer::now(env),
          config = config
        )
      },
      mon = 2
    ) %>%
    simmer::run(until = get_total_simulation_minutes(config))
  
  list(
    env = env,
    arrivals = simmer::get_mon_arrivals(
      env,
      per_resource = FALSE,
      ongoing = TRUE
    ),
    resources = simmer::get_mon_resources(env),
    attributes = simmer::get_mon_attributes(env),
    config = config,
    seed = seed
  )
}
