model_step_distributions <- sim_day_rooms %>%
  dplyr::select(
    patient_id,
    acuity,
    complexity,
    first_seen_duration,
    room_wait_duration,
    workup_duration,
    imaging_duration,
    arrival_time,
    exit_time
  ) %>%
  mutate(
    total_los = exit_time - arrival_time
  )


step_duration_long <- model_step_distributions %>%
  pivot_longer(
    cols = c(
      first_seen_duration,
      room_wait_duration,
      workup_duration,
      imaging_duration,
      total_los
    ),
    names_to = "step",
    values_to = "duration_min"
  )


step_summary <- step_duration_long %>%
  group_by(step) %>%
  summarise(
    n = n(),
    mean_min = mean(duration_min, na.rm = TRUE),
    median_min = median(duration_min, na.rm = TRUE),
    p25_min = quantile(duration_min, 0.25, na.rm = TRUE),
    p75_min = quantile(duration_min, 0.75, na.rm = TRUE),
    p90_min = quantile(duration_min, 0.90, na.rm = TRUE),
    max_min = max(duration_min, na.rm = TRUE)
  )

step_summary

ggplot(step_duration_long,
       aes(x = step, y = duration_min)) +
  geom_boxplot(fill = "#A6CEE3", color = "black", outlier.alpha = 0.25) +
  coord_flip() +
  labs(
    title = "Simulated Duration Distributions by ED Step",
    x = "ED Step",
    y = "Duration (Minutes)"
  ) +
  theme_minimal()