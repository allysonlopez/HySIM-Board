# 3_model/run_simulation.R

#   Main script for running the cleaned MVP ED discrete-event simulation.


source("2_prep/load_packages.R")
source("2_prep/load_data.R")

source("3_model/helper_functions.R")
source("3_model/generate_arrivals.R")
source("3_model/sample_patient_inputs.R")
source("3_model/register_resources.R")
source("3_model/patient_trajectory.R")

source("4_analysis/summarize_results.R")

set.seed(123)

# -----------------------------------------------------------------------------
# 1. Simulation settings
#
# current_quarter:
#   Chooses which quarter of the input data to simulate.
#
# first_seen_scale:
#   Scales down observed arrival-to-first-seen time to reduce double-counting.
#   The raw observed first-seen delay already includes real-world crowding, while
#   this DES also creates queues through limited ED resources.
#
# warmup_days:
#   The first day is run but excluded from analysis so the model is not judged
#   only on an empty starting ED.
#
# analysis_days:
#   Number of post-warmup days used for output summaries.
# -----------------------------------------------------------------------------
current_quarter <- 2
first_seen_scale <- 0.35

warmup_days <- 1
analysis_days <- 7

total_days <- warmup_days + analysis_days
warmup_time <- warmup_days * 24 * 60
analysis_end_time <- total_days * 24 * 60

# Build simulation environment
env <- simmer("ED") %>%
  register_resources()

# Generate patient arrivals
#
# create_arrival_times() creates absolute patient arrival times from the hourly
# arrival-rate table. make_interarrival_function() converts those times into the
# format simmer needs: minutes until the next patient arrives.
arrival_times <- create_arrival_times(
  interarrival_data = interarrival_data,
  current_quarter = current_quarter,
  sim_days = total_days
)

arrival_distribution <- make_interarrival_function(arrival_times)

# Build patient trajectory

# This defines what happens to each patient after arrival: assign attributes,
# route to an ED care space, wait for that space if needed, receive first-seen
# delay, complete workup, possibly receive imaging, and exit.
# -----------------------------------------------------------------------------
patient_trajectory <- build_patient_trajectory(
  env = env,
  current_quarter = current_quarter,
  case_mix_data = case_mix_data,
  first_seen_empirical_data = first_seen_empirical_data,
  first_seen_summary_data = first_seen_summary_data,
  workup_empirical_data = workup_empirical_data,
  workup_summary_data = workup_summary_data,
  imaging_probability_data = imaging_probability_data,
  imaging_duration_data = imaging_duration_data,
  first_seen_scale = first_seen_scale
)

# Add patient generator
#
# This tells simmer to create patients using the patient trajectory and the
# interarrival-time function generated above.
env <- env %>%
  add_generator(
    name_prefix = "patient",
    trajectory = patient_trajectory,
    distribution = arrival_distribution,
    mon = 2
  )

# Run simulation

# The simulation runs from time 0 until the end of the warmup + analysis window.
env <- env %>%
  run(until = analysis_end_time)

# Collect raw outputs
#
# arrivals: patient start/end times and whether the patient finished.
# resources: resource occupancy and queue history over time.
# attributes: patient-level attributes recorded during the run.
# -----------------------------------------------------------------------------
arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)

# Remove warm-up period
#
# We exclude the first simulated day from summaries because the ED starts empty,
# which can make early results look artificially smooth.

arrivals_analysis <- arrivals %>%
  filter(start_time >= warmup_time)

resources_analysis <- resources %>%
  filter(time >= warmup_time)

attributes_analysis <- attributes %>%
  filter(time >= warmup_time)


# Calculate presentation-ready outputs
#
# These summaries are the main numbers to discuss: LOS, bottlenecks, patient mix,
# route mix, and a rough observed-process baseline.
los_metrics <- calculate_los_metrics(arrivals_analysis)
resource_summary <- summarize_resources(resources_analysis)
acuity_summary <- summarize_attributes(attributes_analysis, "acuity")
complexity_summary <- summarize_attributes(attributes_analysis, "complexity_bucket")
route_summary <- summarize_attributes(attributes_analysis, "route")

observed_process_baseline <- calculate_observed_process_baseline(
  first_seen_empirical_data,
  workup_empirical_data
)


# Print outputs
cat("\n--- Simulation LOS Metrics ---\n")
print(los_metrics)

cat("\n--- Resource Summary ---\n")
print(resource_summary)

cat("\n--- Acuity Mix ---\n")
print(acuity_summary)

cat("\n--- Complexity Mix ---\n")
print(complexity_summary)

cat("\n--- Route Mix ---\n")
print(route_summary)

cat("\n--- Observed Baseline from Input Data ---\n")
print(observed_process_baseline)
