# 2_prep/load_data.R
# loads data tables needed for simulation
read_csv_file <- function(file_name) {
  file_path <- file.path("1_data", file_name)
  
  if (!file.exists(file_path)) {
    stop(paste0("Missing file: ", file_path))
  }
  
  read.csv(file_path, stringsAsFactors = FALSE)
}

# controls when patients enter the ED simulation.
# Interarrival data
interarrival <- read_csv(
  "1_data/01_interarrival_by_timeblock_hourly_cy2025.csv"
)

# controls what type of patient arrives at each time block.
# Case mix probabilities
case_mix <- read_csv(
  "1_data/02_case_mix_by_timeblock_cy2025.csv"
)


# time from ED arrival to first practitioner contact.
# First seen distribution summary
first_seen_dist <- read_csv(
  "1_data/03_arrival_to_first_seen_distribution_by_triage_cy2025.csv"
)

# Empirical first seen durations
first_seen_emp <- read_csv(
  "1_data/04_arrival_to_first_seen_empirical_deid_cy2025.csv"
)

# broad ED evaluation/treatment time after first seen.
# Workup distribution summary
workup_dist <- read_csv(
  "1_data/05_generic_workup_duration_distribution_by_complexity_cy2025.csv"
)

# Empirical workup durations
workup_emp <- read_csv(
  "1_data/06_generic_workup_duration_empirical_deid_cy2025.csv"
)

# probability of imaging, modality mix, and imaging duration.
# Imaging probabilities
imaging_prob <- read_csv(
  "1_data/07_imaging_probability_and_modality_mix_by_acuity_historical_2018_2022.csv"
)

# Imaging durations
imaging_time <- read_csv(
  "1_data/08_imaging_duration_distribution_by_modality_historical_2018_2022.csv"
)

# Consult probabilities
consult_prob <- read_csv(
  "1_data/09_consult_probability_and_group_mix_by_acuity_historical_2018_2022.csv"
)