# ============================================================================
# run_simulation.R
# Runs the ED simulation and creates summary and validation outputs.
# ============================================================================

# load files
source("2_prep/load_packages.R")
source("2_prep/load_data.R")

source("3_model/helper_functions.R")
source("3_model/generate_arrivals.R")
source("3_model/sample_patient_inputs.R")
source("3_model/register_resources.R")
source("3_model/patient_trajectory.R")

source("4_analysis/summarize_results.R")
source("4_analysis/visualize_results.R")

set.seed(123)

# set simulation parameters
current_quarter <- 2
first_seen_scale <- 0.7

warmup_days <- 1
analysis_days <- 21

total_days <- warmup_days + analysis_days
warmup_time <- warmup_days * 24 * 60
analysis_end_time <- total_days * 24 * 60


# create simulation
env <- simmer("ED") %>%
  register_resources()


# generate patient arrivals
arrival_times <- create_arrival_times(
  interarrival_data = interarrival_data,
  current_quarter = current_quarter,
  sim_days = total_days
)

arrival_distribution <- make_interarrival_function(arrival_times)


# build patient workflow
patient_trajectory <- build_patient_trajectory(
  env = env,
  current_quarter = current_quarter,
  case_mix_data = case_mix_data,
  first_seen_empirical_data = first_seen_empirical_data,
  first_seen_summary_data = first_seen_summary_data,
  consult_probability_data = consult_probability_data,
  workup_empirical_data = workup_empirical_data,
  workup_summary_data = workup_summary_data,
  imaging_probability_data = imaging_probability_data,
  imaging_duration_data = imaging_duration_data,
  first_seen_scale = first_seen_scale
)


# add patients to simulation
env <- env %>%
  add_generator(
    name_prefix = "patient",
    trajectory = patient_trajectory,
    distribution = arrival_distribution,
    mon = 2
  )


# run simulation
env <- env %>%
  run(until = analysis_end_time)


# collect simulation outputs
arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)


# remove warmup period
arrivals_analysis <- arrivals %>%
  filter(start_time >= warmup_time)

resources_analysis <- resources %>%
  filter(time >= warmup_time)

attributes_analysis <- attributes %>%
  filter(time >= warmup_time)


# calculate summary statistics
patients_per_day <- arrivals_analysis %>%
  mutate(day = floor((start_time - warmup_time) / 1440) + 1) %>%
  filter(day >= 1, day <= analysis_days) %>%
  count(day, name = "n_patients")

patients_per_day_summary <- patients_per_day %>%
  summarise(
    mean_patients_per_day = mean(n_patients),
    min_patients_per_day = min(n_patients),
    max_patients_per_day = max(n_patients)
  )

los_metrics <- calculate_los_metrics(arrivals_analysis)

resource_summary <- summarize_resources(resources_analysis)

acuity_summary <- summarize_attributes(attributes_analysis, "acuity")

complexity_summary <- summarize_attributes(attributes_analysis, "complexity_bucket")

bed_wait_summary <- arrivals_analysis %>%
  mutate(
    total_los_min = end_time - start_time,
    process_time_min = activity_time,
    bed_wait_time_min = total_los_min - process_time_min
  ) %>%
  summarise(
    mean_bed_wait_min = mean(bed_wait_time_min, na.rm = TRUE),
    median_bed_wait_min = median(bed_wait_time_min, na.rm = TRUE),
    p75_bed_wait_min = quantile(bed_wait_time_min, 0.75, na.rm = TRUE),
    p90_bed_wait_min = quantile(bed_wait_time_min, 0.90, na.rm = TRUE),
    max_bed_wait_min = max(bed_wait_time_min, na.rm = TRUE)
  )

process_summary <- attributes_analysis %>%
  filter(key %in% c(
    "first_seen_duration",
    "consult_duration",
    "workup_duration",
    "imaging_duration"
  )) %>%
  group_by(key) %>%
  summarise(
    n = n(),
    mean_min = mean(value, na.rm = TRUE),
    median_min = median(value, na.rm = TRUE),
    p75_min = quantile(value, 0.75, na.rm = TRUE),
    p90_min = quantile(value, 0.90, na.rm = TRUE),
    max_min = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_min))

consult_summary <- attributes_analysis %>%
  filter(key == "consult_duration") %>%
  summarise(
    total_patients = n(),
    n_consults = sum(value > 0, na.rm = TRUE),
    percent_consult = mean(value > 0, na.rm = TRUE),
    mean_consult_duration_min = mean(value[value > 0], na.rm = TRUE)
  )

imaging_summary <- attributes_analysis %>%
  filter(key == "imaging_duration") %>%
  summarise(
    total_patients = n(),
    n_imaging = sum(value > 0, na.rm = TRUE),
    percent_imaging = mean(value > 0, na.rm = TRUE),
    mean_imaging_duration_min = mean(value[value > 0], na.rm = TRUE)
  )

observed_process_baseline <- calculate_observed_process_baseline(
  first_seen_empirical_data,
  workup_empirical_data
)


# create output folder
dir.create("5_outputs", showWarnings = FALSE)


# create operational validation table
historical_patients_per_day <- interarrival_data %>%
  filter(quarter == current_quarter) %>%
  summarise(
    historical_value = sum(arrivals_n, na.rm = TRUE) /
      (sum(hours_observed, na.rm = TRUE) / 24)
  ) %>%
  pull(historical_value)

historical_first_provider_wait <- first_seen_empirical_data %>%
  filter(triage_priority != "UNKNOWN") %>%
  summarise(
    historical_value = mean(duration_min, na.rm = TRUE)
  ) %>%
  pull(historical_value)

historical_workup_duration <- workup_empirical_data %>%
  filter(complexity_bucket != "UNKNOWN") %>%
  summarise(
    historical_value = mean(duration_min, na.rm = TRUE)
  ) %>%
  pull(historical_value)

historical_imaging_duration <- imaging_duration_data %>%
  summarise(
    historical_value = weighted.mean(
      total_imaging_mean_min,
      n_obs,
      na.rm = TRUE
    )
  ) %>%
  pull(historical_value)

historical_imaging_utilization <- imaging_probability_data %>%
  filter(triage_priority != "UNKNOWN") %>%
  summarise(
    historical_value = weighted.mean(
      needs_imaging_prob,
      n_obs,
      na.rm = TRUE
    ) * 100
  ) %>%
  pull(historical_value)

historical_consult_utilization <- consult_probability_data %>%
  filter(triage_priority != "UNKNOWN") %>%
  summarise(
    historical_value = weighted.mean(
      needs_consult_prob,
      n_obs,
      na.rm = TRUE
    ) * 100
  ) %>%
  pull(historical_value)


# get simulated values for validation
sim_first_provider_wait <- process_summary %>%
  filter(key == "first_seen_duration") %>%
  pull(mean_min)

sim_workup_duration <- process_summary %>%
  filter(key == "workup_duration") %>%
  pull(mean_min)

sim_imaging_duration <- imaging_summary$mean_imaging_duration_min

sim_imaging_utilization <- imaging_summary$percent_imaging * 100

sim_consult_utilization <- consult_summary$percent_consult * 100


validation_summary <- tibble(
  metric = c(
    "Patients per Day (patients/day)",
    "Mean First Provider Wait (min)",
    "Mean Workup Duration (min)",
    "Mean Imaging Duration (min)",
    "Imaging Utilization (%)",
    "Consult Utilization (%)"
  ),
  unit = c(
    "patients/day",
    "minutes",
    "minutes",
    "minutes",
    "percent",
    "percent"
  ),
  historical = c(
    historical_patients_per_day,
    historical_first_provider_wait,
    historical_workup_duration,
    historical_imaging_duration,
    historical_imaging_utilization,
    historical_consult_utilization
  ),
  simulated = c(
    patients_per_day_summary$mean_patients_per_day,
    sim_first_provider_wait,
    sim_workup_duration,
    sim_imaging_duration,
    sim_imaging_utilization,
    sim_consult_utilization
  )
) %>%
  mutate(
    historical = round(historical, 2),
    simulated = round(simulated, 2),
    percent_difference = round(
      100 * (simulated - historical) / historical,
      1
    ),
    absolute_error = abs(simulated - historical),
    absolute_percent_error = abs(simulated - historical) / historical * 100
  )

overall_mape <- mean(
  validation_summary$absolute_percent_error,
  na.rm = TRUE
)

mape_summary <- validation_summary %>%
  select(metric, absolute_percent_error) %>%
  rename(MAPE = absolute_percent_error) %>%
  mutate(
    MAPE = round(MAPE, 1)
  )

write_csv(
  validation_summary,
  "5_outputs/validation_summary.csv"
)

write_csv(
  mape_summary,
  "5_outputs/mape_summary.csv"
)


# create triage validation table
historical_triage <- case_mix_data %>%
  filter(
    quarter == current_quarter,
    attribute_name == "acuity",
    attribute_value != "UNKNOWN"
  ) %>%
  group_by(attribute_value) %>%
  summarise(
    historical = sum(n_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    historical_percent = 100 * historical / sum(historical)
  ) %>%
  rename(triage_priority = attribute_value)

simulated_triage <- attributes_analysis %>%
  filter(key == "acuity") %>%
  mutate(
    triage_priority = as.character(value)
  ) %>%
  filter(triage_priority != "UNKNOWN") %>%
  count(triage_priority, name = "simulated") %>%
  mutate(
    simulated_percent = 100 * simulated / sum(simulated)
  )

triage_validation <- historical_triage %>%
  full_join(
    simulated_triage,
    by = "triage_priority"
  ) %>%
  mutate(
    historical_percent = replace_na(historical_percent, 0),
    simulated_percent = replace_na(simulated_percent, 0),
    difference = simulated_percent - historical_percent
  ) %>%
  arrange(triage_priority)

write_csv(
  triage_validation,
  "5_outputs/triage_validation.csv"
)


# create complexity validation table
historical_complexity <- case_mix_data %>%
  filter(
    quarter == current_quarter,
    attribute_name == "complexity_bucket",
    attribute_value != "UNKNOWN"
  ) %>%
  group_by(attribute_value) %>%
  summarise(
    historical = sum(n_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    historical_percent = 100 * historical / sum(historical)
  ) %>%
  rename(complexity_bucket = attribute_value)

simulated_complexity <- attributes_analysis %>%
  filter(key == "complexity_bucket") %>%
  mutate(
    complexity_bucket = as.character(value)
  ) %>%
  filter(complexity_bucket != "UNKNOWN") %>%
  count(complexity_bucket, name = "simulated") %>%
  mutate(
    simulated_percent = 100 * simulated / sum(simulated)
  )

complexity_validation <- historical_complexity %>%
  full_join(
    simulated_complexity,
    by = "complexity_bucket"
  ) %>%
  mutate(
    historical_percent = replace_na(historical_percent, 0),
    simulated_percent = replace_na(simulated_percent, 0),
    difference = simulated_percent - historical_percent
  ) %>%
  arrange(complexity_bucket)

write_csv(
  complexity_validation,
  "5_outputs/complexity_validation.csv"
)


# create validation plots
main_blue <- "#1F4E79"
light_blue <- "#9ECAE1"

complexity_validation <- complexity_validation %>%
  mutate(
    complexity_bucket = case_when(
      complexity_bucket == "1" ~ "minimal",
      complexity_bucket == "2" ~ "straightforward",
      complexity_bucket == "3" ~ "low",
      complexity_bucket == "4" ~ "moderate",
      complexity_bucket == "5" ~ "high",
      complexity_bucket == "6" ~ "critical_care",
      TRUE ~ complexity_bucket
    )
  )

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


# print outputs
cat("\n--- Patients Per Day by Analysis Day ---\n")
print(patients_per_day)

cat("\n--- Patients Per Day Summary ---\n")
print(patients_per_day_summary)

cat("\n--- Simulation LOS Metrics ---\n")
print(los_metrics)

cat("\n--- Resource Summary ---\n")
print(resource_summary)

cat("\n--- Bed Wait Summary ---\n")
print(bed_wait_summary)

cat("\n--- Acuity Mix ---\n")
print(acuity_summary)

cat("\n--- Complexity Mix ---\n")
print(complexity_summary)

cat("\n--- Process Summary ---\n")
print(process_summary)

cat("\n--- Consult Summary ---\n")
print(consult_summary)

cat("\n--- Imaging Summary ---\n")
print(imaging_summary)

cat("\n--- Observed Baseline from Input Data ---\n")
print(observed_process_baseline)

cat("\n--- Validation Summary ---\n")
print(validation_summary)

cat("\n--- MAPE Summary ---\n")
print(mape_summary)

cat(
  "\nOverall Operational Validation MAPE:",
  round(overall_mape, 2),
  "%\n"
)

cat("\n--- Triage Validation ---\n")
print(triage_validation)

cat("\n--- Complexity Validation ---\n")
print(complexity_validation)