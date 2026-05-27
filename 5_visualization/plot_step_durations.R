plot_step_durations <- function(patient_table) {
  plot_data <- patient_table %>%
    dplyr::filter(.data$after_warmup) %>%
    dplyr::select(
      front_end_delay_min,
      workup_duration_min,
      imaging_duration_min,
      consult_delay_min,
      boarding_delay_min
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "step",
      values_to = "duration_min"
    ) %>%
    dplyr::filter(.data$duration_min > 0) %>%
    dplyr::mutate(
      step = dplyr::recode(
        .data$step,
        front_end_delay_min = "Arrival to first provider",
        workup_duration_min = "Generic workup",
        imaging_duration_min = "Imaging",
        consult_delay_min = "Consult LOS adjustment",
        boarding_delay_min = "Boarding / observation delay"
      )
    )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$duration_min)) +
    ggplot2::geom_histogram(bins = 35, fill = "steelblue", color = "white") +
    ggplot2::facet_wrap(~ step, scales = "free") +
    ggplot2::labs(
      title = "Duration Distribution by Patient Pathway Step",
      x = "Duration (minutes)",
      y = "Patient count"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
