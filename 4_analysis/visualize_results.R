# 04/visualize_results.R


# validation plots
create_validation_plots <- function() {
  
  main_blue <- "#1F4E79"
  light_blue <- "#9ECAE1"
  
  # operational metric plot
  validation_plot <- validation_summary %>%
    select(metric, historical, simulated) %>%
    pivot_longer(
      cols = c(historical, simulated),
      names_to = "source",
      values_to = "value"
    ) %>%
    mutate(
      source = recode(
        source,
        historical = "Historical",
        simulated = "Simulated"
      )
    ) %>%
    ggplot(aes(x = metric, y = value, fill = source)) +
    geom_col(position = "dodge", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c(
      "Historical" = light_blue,
      "Simulated" = main_blue
    )) +
    labs(
      title = "Operational Validation Summary: Historical vs Simulated Metrics",
      x = NULL,
      y = "Operational Metric Value",
      fill = NULL
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(
    "5_outputs/validation_summary_plot.png",
    validation_plot,
    width = 10,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  # triage distributoins
  triage_plot <- triage_validation %>%
    select(triage_priority, historical_percent, simulated_percent) %>%
    pivot_longer(
      cols = c(historical_percent, simulated_percent),
      names_to = "source",
      values_to = "percent"
    ) %>%
    mutate(
      source = recode(
        source,
        historical_percent = "Historical",
        simulated_percent = "Simulated"
      )
    ) %>%
    ggplot(aes(x = factor(triage_priority), y = percent, fill = source)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c(
      "Historical" = light_blue,
      "Simulated" = main_blue
    )) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20)
    ) +
    labs(
      title = "Model Validation: Historical vs Simulated Triage Distribution",
      x = "Triage Level",
      y = "Percent of Patients",
      fill = NULL
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(
    "5_outputs/triage_validation_plot.png",
    triage_plot,
    width = 8,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
  # complexiy distributions
  complexity_plot <- complexity_validation %>%
    select(complexity_bucket, historical_percent, simulated_percent) %>%
    pivot_longer(
      cols = c(historical_percent, simulated_percent),
      names_to = "source",
      values_to = "percent"
    ) %>%
    mutate(
      source = recode(
        source,
        historical_percent = "Historical",
        simulated_percent = "Simulated"
      ),
      complexity_bucket = factor(
        complexity_bucket,
        levels = c(
          "minimal",
          "straightforward",
          "low",
          "moderate",
          "high",
          "critical_care"
        )
      )
    ) %>%
    ggplot(aes(x = complexity_bucket, y = percent, fill = source)) +
    geom_col(position = "dodge", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c(
      "Historical" = light_blue,
      "Simulated" = main_blue
    )) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20)
    ) +
    labs(
      title = "Model Validation: Historical vs Simulated Complexity Distribution",
      x = NULL,
      y = "Percent of Patients",
      fill = NULL
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(
    "5_outputs/complexity_validation_plot.png",
    complexity_plot,
    width = 9,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}