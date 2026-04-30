source("2_prep/00_load_package.R")
source("2_prep/01_load_input_data.R")

source("3_model/generators/helper_functions.R")
source("3_model/generators/sample_attributes.R")
source("3_model/generators/sample_interarrival.R")
source("3_model/register_resources.R")
source("3_model/distributions/sample_first_seen_delay.R")
source("3_model/distributions/sample_workup_duration.R")
source("3_model/build_patient_trajectory.R")

source("4_analysis/analysis_result.R")

current_quarter <- 2

env <- simmer("ED") %>%
  register_resources()

patient_trajectory <- build_patient_trajectory(
  case_mix_data = case_mix_data,
  first_seen_data = first_seen_summary_data,
  workup_data = workup_summary_data,
  current_quarter = current_quarter,
  env = env
)

env %>%
  add_generator(
    name_prefix = "patient",
    trajectory = patient_trajectory,
    distribution = function() {
      sample_interarrival(
        data = interarrival_data,
        current_time = now(env),
        current_quarter = current_quarter
      )
    },
    mon = 2
  ) %>%
  run(until = 24 * 60)

arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)

los_metrics <- calculate_los_metrics(arrivals)
resource_summary <- summarize_resources(resources)
acuity_summary <- summarize_attributes(attributes, "acuity")
complexity_summary <- summarize_attributes(attributes, "complexity_bucket")

los_metrics
resource_summary
acuity_summary
complexity_summary