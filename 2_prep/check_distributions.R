# Summary of arrival-to-first-provider duration
summary(first_seen_emp$duration_min)

# Summary of generic workup duration
summary(workup_emp$duration_min)

# Workup duration summary by complexity bucket
workup_emp %>%
  group_by(complexity_bucket) %>%
  summarise(
    n = n(),
    mean = mean(duration_min),
    median = median(duration_min),
    p90 = quantile(duration_min, 0.90),
    max = max(duration_min)
  )

#The first-seen and workup duration distributions are strongly right-skewed. 
#Most patients have moderate wait and workup times, but a small number of 
#patients have extremely long durations. 
#Because the DES samples directly from the empirical distributions, these long-tail values can 
#appear in simulated patient paths and increase the mean total duration.

ggplot(workup_emp, aes(x = duration_min)) +
  geom_histogram(bins = 51) +
  scale_x_log10() +
  labs(
    title = "Generic Workup Duration Distribution",
    x = "Duration in minutes, log scale",
    y = "Number of encounters"
  )


# Plot total ED census over time

ggplot(state_warm, aes(x = time, y = total_in_ed)) +
  geom_line() +
  labs(
    title = "Simulated ED Census Over Time",
    x = "Simulation Time (Minutes)",
    y = "Patients in ED"
  )

#hour scale
state_warm %>%
  mutate(hour = time / 60) %>%
  ggplot(aes(x = hour, y = total_in_ed)) +
  geom_line() +
  labs(
    title = "Simulated ED Census Over 24 Hours",
    x = "Hour of Day",
    y = "Patients in ED"
  )


state_long <- state_warm %>%
  pivot_longer(
    cols = c(waiting_first_seen, in_workup, in_imaging),
    names_to = "state",
    values_to = "count"
  )

ggplot(state_long,
       aes(x = time / 60,
           y = count,
           color = state)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Patients in Each ED State Over Time",
    x = "Hour of Day",
    y = "Number of Patients"
  )


#capacity plot 
core_capacity_day %>%
  mutate(hour = time / 60) %>%
  ggplot(aes(x = hour, y = occupied_core_rooms)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 43, linetype = "dashed") +
  labs(
    title = "Simulated Core ED Room Occupancy",
    x = "Hour of Day",
    y = "Occupied Core Rooms"
  )

source("3_model/04_sample_state_durations.R")
daily_summary <- sim_multi_days %>%
  group_by(day) %>%
  summarise(
    n_patients = n(),
    mean_total_duration = mean(total_duration),
    median_total_duration = median(total_duration),
    mean_room_wait = mean(room_wait_duration),
    median_room_wait = median(room_wait_duration),
    percent_waited_for_room = mean(room_wait_duration > 0),
    imaging_rate = mean(needs_imaging),
    consult_rate = mean(needs_consult)
  )
daily_summary

model_flow_table <- tibble(
  step = 1:7,
  model_event = c(
    "Patient arrival",
    "Assign patient attributes",
    "Wait for first provider",
    "Wait for core ED room if needed",
    "Generic workup",
    "Imaging branch",
    "Consult branch / exit"
  ),
  file_location = c(
    "02_generate_arrivals.R",
    "03_sample_patient_attributes.R",
    "04_sample_state_durations.R",
    "07_run_simulation.R",
    "04_sample_state_durations.R",
    "04_sample_state_durations.R / 05_patient_trajectory.R",
    "04_sample_state_durations.R / 05_patient_trajectory.R"
  ),
  output_created = c(
    "arrival_time, arrival_hour",
    "acuity, complexity, age group, arrival mode",
    "first_seen_duration",
    "room_wait_duration, room_id",
    "workup_duration",
    "needs_imaging, imaging_type, imaging_duration",
    "needs_consult, consult_group, exit_time"
  )
)
model_flow_table

sim_multi_days %>%
  count(day, arrival_hour) %>%
  ggplot(aes(x = arrival_hour, y = n, color = factor(day))) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Simulated Patient Arrivals by Hour Across Days",
    x = "Hour of Day",
    y = "Number of Arrivals",
    color = "Day"
  )

ggplot(sim_multi_days, aes(x = room_wait_duration)) +
  geom_histogram(bins = 51,
                 fill = "steelblue",
                 color = "black") +
  scale_x_log10() +
  labs(
    title = "Distribution of Room Wait Duration",
    x = "Room Wait Duration in Minutes, Log Scale",
    y = "Number of Patients"
  )

ggplot(sim_multi_days, aes(x = room_wait_duration)) +
  geom_histogram(bins = 51,
                 fill = "steelblue",
                 outline = "black") +
  labs(
    title = "Distribution of Room Wait Duration",
    x = "Room Wait Duration in Minutes, Log Scale",
    y = "Number of Patients"
  )

ggplot(
  sim_multi_days,
  aes(x = room_wait_duration / 60)
) +
  geom_histogram(bins = 51) +
  scale_x_log10() +
  labs(
    title = "Distribution of Room Wait Duration",
    x = "Room Wait Duration (Hours, Log Scale)",
    y = "Number of Patients"
  )

sim_multi_days %>%
  count(day, arrival_hour) %>%
  ggplot(aes(x = arrival_hour,
             y = n,
             color = factor(day))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "1" = "darkblue",
    "2" = "blue",
    "3" = "dodgerblue",
    "4" = "royalblue",
    "5" = "deepskyblue",
    "6" = "skyblue",
    "7" = "cornflowerblue"
  )) +
  labs(
    title = "Simulated Patient Arrivals by Hour Across Days",
    x = "Hour of Day",
    y = "Number of Arrivals",
    color = "Day"
  ) +
  theme_minimal()
