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