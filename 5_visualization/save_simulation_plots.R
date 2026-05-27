save_simulation_plots <- function(sim_output, summary_output, output_dir = "6_outputs/figures") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  los_plot <- plot_los_distribution(summary_output$patient_table)
  step_plot <- plot_step_durations(summary_output$patient_table)
  resource_plot <- plot_resource_use(sim_output$resources)
  
  ggplot2::ggsave(file.path(output_dir, "los_distribution.png"), los_plot, width = 9, height = 6, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "step_duration_distributions.png"), step_plot, width = 12, height = 8, dpi = 300)
  ggplot2::ggsave(file.path(output_dir, "resource_use_over_time.png"), resource_plot, width = 12, height = 8, dpi = 300)
  
  list(
    los_plot = los_plot,
    step_plot = step_plot,
    resource_plot = resource_plot
  )
}
