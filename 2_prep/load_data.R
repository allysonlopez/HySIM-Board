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
interarrival_data <- read_csv_file("01_interarrival_by_timeblock_hourly_cy2025.csv")

# controls what type of patient arrives at each time block.
case_mix_data <- read_csv_file("02_case_mix_by_timeblock_cy2025.csv")

# time from ED arrival to first practitioner contact.
first_seen_summary_data <- read_csv_file("03_arrival_to_first_seen_distribution_by_triage_cy2025.csv")
first_seen_empirical_data <- read_csv_file("04_arrival_to_first_seen_empirical_deid_cy2025.csv")

# broad ED evaluation/treatment time after first seen.
workup_summary_data <- read_csv_file("05_generic_workup_duration_distribution_by_complexity_cy2025.csv")
workup_empirical_data <- read_csv_file("06_generic_workup_duration_empirical_deid_cy2025.csv")

# probability of imaging, modality mix, and imaging duration.
imaging_probability_data <- read_csv_file("07_imaging_probability_and_modality_mix_by_acuity_historical_2018_2022.csv")
imaging_duration_data <- read_csv_file("08_imaging_duration_distribution_by_modality_historical_2018_2022.csv")
