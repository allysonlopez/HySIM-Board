# 4_analysis/visualize_results.R

library(tidyverse)

main_blue <- "#1F77B4"
light_blue <- "#A6CEE3"
dark_blue <- "#0B3C5D"


# ----------------------------
# 1. Patients per day
# ----------------------------

sim_7days <- purrr::map_dfr(1:7, function(day_id) {
  
  sim_day <- run_one_day_simulation_with_rooms(
    quarter_current = 1,
    dow_current = day_id,
    core_capacity = 43
  )
  
  sim_day %>%
    mutate(day = day_id)
})

patients_per_day <- sim_7days %>%
  group_by(day) %>%
  summarise(
    n_patients = n()
  )

plot_patients_per_day <- function(patients_per_day) {
  ggplot(patients_per_day, aes(x = factor(day), y = n_patients)) +
    geom_col(fill = main_blue, color = "black") +
    geom_hline(yintercept = 170, linetype = "dashed") +
    geom_hline(yintercept = 190, linetype = "dashed") +
    labs(
      title = "Simulated Patient Arrivals Across 7 Days",
      subtitle = "Dashed lines show target range: 170–190 patients/day",
      x = "Simulation Day",
      y = "Number of Patients"
    ) +
    theme_minimal()
}


# ----------------------------
# 2. Overall LOS distribution
# ----------------------------

plot_los_distribution <- function(sim_day_rooms) {
  ggplot(sim_day_rooms, aes(x = (exit_time - arrival_time) / 60)) +
    geom_histogram(fill = "#1F77B4", color = "black", bins = 30) +
    labs(
      title = "Overall Length of Stay Distribution",
      x = "Length of Stay (Hours)",
      y = "Number of Patients"
    ) +
    theme_minimal()
}


# ----------------------------
# 3. Bed wait distribution
# ----------------------------

plot_bed_wait_distribution <- function(sim_day_rooms) {
  ggplot(sim_day_rooms, aes(x = room_wait_duration / 60)) +
    geom_histogram(fill = "#0B3C5D", color = "black", bins = 30) +
    labs(
      title = "ED Room Wait Distribution",
      subtitle = "Additional waiting caused by 43-room capacity constraint",
      x = "Room Wait (Hours)",
      y = "Number of Patients"
    ) +
    theme_minimal()
}


# ----------------------------
# 4. Flow step duration comparison
# ----------------------------

process_summary <- step_duration_long %>%
  group_by(step) %>%
  summarise(
    mean_min = mean(duration_min, na.rm = TRUE)
  ) %>%
  rename(key = step)

plot_flow_step_means <- function(process_summary) {
  process_summary %>%
    mutate(
      key = recode(
        key,
        "first_seen_duration" = "First Seen",
        "consult_duration" = "Consult",
        "workup_duration" = "Workup",
        "imaging_duration" = "Imaging"
      )
    ) %>%
    ggplot(aes(x = reorder(key, mean_min), y = mean_min)) +
    geom_col(fill = main_blue, color = "black") +
    coord_flip() +
    labs(
      title = "Average Time by ED Flow Step",
      subtitle = "Workup is the longest clinical process, but bed waiting is the largest overall bottleneck",
      x = "Flow Step",
      y = "Average Time (minutes)"
    ) +
    theme_minimal()
}


# ----------------------------
# 5. Flow step distributions
# ----------------------------

attributes_analysis <- step_duration_long %>%
  rename(
    key = step,
    value = duration_min
  )

plot_flow_step_distributions <- function(attributes_analysis) {
  attributes_analysis %>%
    filter(key %in% c(
      "first_seen_duration",
      "consult_duration",
      "workup_duration",
      "imaging_duration"
    )) %>%
    mutate(
      step = recode(
        key,
        "first_seen_duration" = "First Seen",
        "consult_duration" = "Consult",
        "workup_duration" = "Workup",
        "imaging_duration" = "Imaging"
      )
    ) %>%
    ggplot(aes(x = step, y = value)) +
    geom_boxplot(fill = light_blue, color = "black", outlier.alpha = 0.3) +
    labs(
      title = "Distribution of Time Spent in Each ED Flow Step",
      subtitle = "Shows variability across patients for each process block",
      x = "Flow Step",
      y = "Duration (minutes)"
    ) +
    theme_minimal()
}


# ----------------------------
# 6. Acuity mix
# ----------------------------

plot_acuity_mix <- function(acuity_summary) {
  ggplot(acuity_summary, aes(x = acuity, y = prop)) +
    geom_col(fill = "#1F77B4", color = "black") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Simulated Patient Acuity Mix",
      x = "Acuity Level",
      y = "Percent of Patients"
    ) +
    theme_minimal()
}


# ----------------------------
# 7. Complexity mix
# ----------------------------
complexity_summary <- sim_day_rooms %>%
  count(complexity) %>%
  mutate(prop = n / sum(n)) %>%
  rename(value = complexity)

plot_complexity_mix <- function(complexity_summary) {
  ggplot(complexity_summary, aes(x = factor(value), y = prop)) +
    geom_col(fill = main_blue, color = "black") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Simulated Patient Complexity Mix",
      subtitle = "Higher complexity patients drive longer ED bed occupancy",
      x = "Complexity Bucket",
      y = "Percent of Patients"
    ) +
    theme_minimal()
}


# ----------------------------
# 8. Save all plots
# ----------------------------

source("4_analysis/visualize_results.R")
save_simulation_plots <- function(arrivals_analysis,
                                  attributes_analysis,
                                  patients_per_day,
                                  process_summary,
                                  acuity_summary,
                                  complexity_summary) {
  
  output_folder <- "5_outputs"
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  p1 <- plot_patients_per_day(patients_per_day)
  p2 <- plot_los_distribution(arrivals_analysis)
  p3 <- plot_bed_wait_distribution(arrivals_analysis)
  p4 <- plot_flow_step_means(process_summary)
  p5 <- plot_flow_step_distributions(attributes_analysis)
  p6 <- plot_acuity_mix(acuity_summary)
  p7 <- plot_complexity_mix(complexity_summary)
  
  ggsave(filename = file.path(output_folder, "01_patients_per_day.png"), plot = p1, width = 8, height = 5)
  ggsave(filename = file.path(output_folder, "02_los_distribution.png"), plot = p2, width = 8, height = 5)
  ggsave(filename = file.path(output_folder, "03_bed_wait_distribution.png"), plot = p3, width = 8, height = 5)
  ggsave(filename = file.path(output_folder, "04_flow_step_means.png"), plot = p4, width = 8, height = 5)
  ggsave(filename = file.path(output_folder, "05_flow_step_distributions.png"), plot = p5, width = 8, height = 5)
  ggsave(filename = file.path(output_folder, "06_acuity_mix.png"), plot = p6, width = 8, height = 5)
  ggsave(filename = file.path(output_folder, "07_complexity_mix.png"), plot = p7, width = 8, height = 5)
}

