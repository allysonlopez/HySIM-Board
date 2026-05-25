# 3_model/run_simulation.R

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

current_quarter <- 2
first_seen_scale <- 0.35

warmup_days <- 1
analysis_days <- 21

total_days <- warmup_days + analysis_days
warmup_time <- warmup_days * 24 * 60
analysis_end_time <- total_days * 24 * 60


# ----------------------------
# Build simulation environment
# ----------------------------

env <- simmer("ED") %>%
  register_resources()


# ----------------------------
# Generate arrivals
# ----------------------------

arrival_times <- create_arrival_times(
  interarrival_data = interarrival_data,
  current_quarter = current_quarter,
  sim_days = total_days
)

arrival_distribution <- make_interarrival_function(arrival_times)


# ----------------------------
# Build patient trajectory
# ----------------------------

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


# ----------------------------
# Add patient generator
# ----------------------------

env <- env %>%
  add_generator(
    name_prefix = "patient",
    trajectory = patient_trajectory,
    distribution = arrival_distribution,
    mon = 2
  )


# ----------------------------
# Run simulation
# ----------------------------

env <- env %>%
  run(until = analysis_end_time)


# ----------------------------
# Get monitor outputs
# ----------------------------

arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)


# ----------------------------
# Remove warmup period
# ----------------------------

arrivals_analysis <- arrivals %>%
  filter(start_time >= warmup_time)

resources_analysis <- resources %>%
  filter(time >= warmup_time)

attributes_analysis <- attributes %>%
  filter(time >= warmup_time)


# ----------------------------
# Patients per day check
# ----------------------------

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


# ----------------------------
# Overall LOS and resource summaries
# ----------------------------

los_metrics <- calculate_los_metrics(arrivals_analysis)

resource_summary <- summarize_resources(resources_analysis)

acuity_summary <- summarize_attributes(attributes_analysis, "acuity")

complexity_summary <- summarize_attributes(attributes_analysis, "complexity_bucket")


# ----------------------------
# Bed wait / queue bottleneck
# ----------------------------

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


# ----------------------------
# Process-level summaries
# ----------------------------

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


# ----------------------------
# Print outputs
# ----------------------------

cat("\n--- Patients Per Day by Analysis Day ---\n")
print(patients_per_day)

cat("\n--- Patients Per Day Summary ---\n")
print(patients_per_day_summary)

cat("\n--- Simulation LOS Metrics ---\n")
print(los_metrics)

cat("\n--- Resource Summary: ED Beds Only ---\n")
print(resource_summary)

cat("\n--- Bed Wait / Queue Bottleneck Summary ---\n")
print(bed_wait_summary)

cat("\n--- Acuity Mix ---\n")
print(acuity_summary)

cat("\n--- Complexity Mix ---\n")
print(complexity_summary)

cat("\n--- Flow Step Bottleneck Summary ---\n")
print(process_summary)

cat("\n--- Consult Summary ---\n")
print(consult_summary)

cat("\n--- Imaging Summary ---\n")
print(imaging_summary)

cat("\n--- Observed Baseline from Input Data ---\n")
print(observed_process_baseline)

save_simulation_plots(
  arrivals_analysis = arrivals_analysis,
  attributes_analysis = attributes_analysis,
  patients_per_day = patients_per_day,
  process_summary = process_summary,
  acuity_summary = acuity_summary,
  complexity_summary = complexity_summary
)