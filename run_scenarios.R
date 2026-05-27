source("source_model_files.R")

input_data <- load_model_input_data("1_data")
validate_model_input_data(input_data)

base_config <- default_model_config()
scenario_functions <- get_scenarios()

scenario_results <- purrr::imap_dfr(scenario_functions, function(scenario_function, scenario_name) {
  scenario_config <- scenario_function(base_config)
  run_replications(
    input_data = input_data,
    config = scenario_config,
    scenario_name = scenario_name,
    n_replications = 10,
    seed_start = 2000
  )
})

dir.create("6_outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("6_outputs/figures", recursive = TRUE, showWarnings = FALSE)

readr::write_csv(scenario_results, "6_outputs/tables/scenario_replication_results.csv")

scenario_plot <- plot_scenario_comparison(scenario_results)
ggplot2::ggsave("6_outputs/figures/scenario_comparison_median_los.png", scenario_plot, width = 9, height = 6, dpi = 300)

print(
  scenario_results %>%
    dplyr::group_by(.data$scenario) %>%
    dplyr::summarise(
      mean_median_los = mean(.data$median_los_min, na.rm = TRUE),
      mean_p90_los = mean(.data$p90_los_min, na.rm = TRUE),
      mean_completed_patients = mean(.data$completed_patients, na.rm = TRUE),
      .groups = "drop"
    )
)
