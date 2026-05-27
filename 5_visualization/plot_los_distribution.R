plot_los_distribution <- function(patient_table) {
  plot_data <- patient_table %>%
    dplyr::filter(.data$in_analysis_window, .data$finished, !is.na(.data$los_min))
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$los_min)) +
    ggplot2::geom_histogram(bins = 40, fill = "steelblue", color = "white") +
    ggplot2::labs(
      title = "Simulated ED Length of Stay Distribution",
      x = "Length of stay (minutes)",
      y = "Number of completed patients"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}
