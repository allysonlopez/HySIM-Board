source("source_model_files.R")

input_data <- load_model_input_data("1_data")

config <- default_model_config()

sim_output <- run_one_simulation(
  input_data = input_data,
  config = config,
  seed = config$random_seed
)

summary_output <- summarize_simulation(sim_output)

dir.create("6_outputs/model_runs", recursive = TRUE, showWarnings = FALSE)
saveRDS(sim_output, "6_outputs/model_runs/baseline_sim_output.rds")
saveRDS(summary_output, "6_outputs/model_runs/baseline_summary_output.rds")

save_simulation_tables(summary_output, "6_outputs/tables")
save_simulation_plots(sim_output, summary_output, "6_outputs/figures")

print(summary_output$flow_summary)
print(summary_output$los_summary)
print(summary_output$resource_summary)
