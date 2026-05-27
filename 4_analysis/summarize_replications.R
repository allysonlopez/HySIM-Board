summarize_one_replication <- function(sim_output, scenario_name, replication_id) {
  summary_output <- summarize_simulation(sim_output)
  
  dplyr::bind_cols(
    tibble::tibble(
      scenario = scenario_name,
      replication = replication_id,
      seed = sim_output$seed
    ),
    summary_output$los_summary,
    summary_output$flow_summary
  )
}

run_replications <- function(input_data, config, scenario_name, n_replications = 10, seed_start = 1000) {
  purrr::map_dfr(seq_len(n_replications), function(replication_id) {
    sim_output <- run_one_simulation(
      input_data = input_data,
      config = config,
      seed = seed_start + replication_id
    )
    summarize_one_replication(sim_output, scenario_name, replication_id)
  })
}
