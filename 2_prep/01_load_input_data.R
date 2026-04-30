# 2_prep/01_load_input_data

read_csv_file <- function(file_path){
  if (!file.exists(file_path)){
    stop(past0("File not found: ", file_path))
  }
  return(read.csv(file_path))
}

data_dir <- "1_data"

interarrival_data <- read_csv_file(file_path = file.path(data_dir, "01_interarrival_by_timeblock_hourly_cy2025.csv"))
case_mix_data <- read_csv_file(file_path = file.path(data_dir, "02_case_mix_by_timeblock_cy2025.csv"))
first_seen_summary_data <- read_csv_file(file_path = file.path(data_dir, "03_arrival_to_first_seen_distribution_by_triage_cy2025.csv"))
first_seen_empirical_data <- read_csv_file(file_path = file.path(data_dir, "04_arrival_to_first_seen_empirical_deid_cy2025.csv"))
workup_summary_data <- read_csv_file(file_path = file.path(data_dir, "05_generic_workup_duration_distribution_by_complexity_cy2025.csv"))
workup_empirical_data <- read_csv_file(file_path = file.path(data_dir, "06_generic_workup_duration_empirical_deid_cy2025.csv"))
imaging_probability_data <- read_csv_file(file_path = file.path(data_dir, "07_imaging_probability_and_modality_mix_by_complexity_acuity_cy2025.csv"))
imaging_duration_data <- read_csv_file(file_path = file.path(data_dir, "08_imaging_duration_distribution_by_modality_cy2025.csv"))

input_data_list <- list(
  interarrival_data = interarrival_data,
  case_mix_data = case_mix_data,
  first_seen_summary_data = first_seen_summary_data,
  first_seen_empirical_data = first_seen_empirical_data,
  workup_summary_data = workup_summary_data,
  workup_empirical_data = workup_empirical_data,
  imaging_probability_data = imaging_probability_data,
  imaging_duration_data = imaging_duration_data
)