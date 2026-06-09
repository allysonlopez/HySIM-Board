# 03\run_simulation.R

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

# set sim params
current_quarter <- 2
first_seen_scale <- 0.7
warmup_days <- 1
analysis_days <- 21

total_days <- warmup_days + analysis_days
warmup_time <- warmup_days * 24 * 60
analysis_end_time <- total_days * 24 * 60

# set up sim
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

# add patients to sim
env <- env %>%
  add_generator(
    name_prefix = "patient",
    trajectory = patient_trajectory,
    distribution = arrival_distribution,
    mon = 2
  )

# run sim
env <- env %>%
  run(until = analysis_end_time)

# collect sim outputs
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

# create summaries, valdation tabls, and plots
create_summary_outputs()
create_validation_plots()
print_summary_outputs()