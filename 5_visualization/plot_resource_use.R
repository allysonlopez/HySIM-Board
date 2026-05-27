plot_resource_use <- function(resource_table) {
  ggplot2::ggplot(resource_table, ggplot2::aes(x = .data$time, y = .data$server)) +
    ggplot2::geom_line(linewidth = 0.6, color = "steelblue") +
    ggplot2::facet_wrap(~ resource, scales = "free_y") +
    ggplot2::labs(
      title = "Resource Use Over Simulation Time",
      x = "Simulation time (minutes)",
      y = "Number in service"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
