# 2_prep/01_load_input_data

read_csv_file <- function(file_path){
  if (!file.exists(file_path)){
    stop(past0("File not found: ", file_path))
  }
  return(readr::read_csv(file_path, show_col_types = FALSE))
}

data_dir <- "1_data"

interarrival_by_hourly <- read_csv_file(file_path = file.path(data_dir, "01_interarrival_by_timeblock_hourly_cy2025.csv"))
case_mix <- read_csv_file(file_path = file.path(data_dir, "02_case_mix_by_timeblock_cy2025.csv"))
arrival_to_first_seen_summary <- read_csv_file(file_path = file.path(data_dir, "03_arrival_to_first_seen_distribution_by_triage_cy2025.csv"))
arrival_to_first_seen_empirical <- read_csv_file(file_path = file.path(data_dir, "04_arrival_to_first_seen_empirical_deid_cy2025.csv"))
generic_workup_summary <- read_csv_file(file_path = file.path(data_dir, "05_generic_workup_duration_distribution_by_complexity_cy2025.csv"))
generic_workup_empirical <- read_csv_file(file_path = file.path(data_dir, "06_generic_workup_duration_empirical_deid_cy2025.csv"))
imaging_probability <- read_csv_file(file_path = file.path(data_dir, "07_imaging_probability_and_modality_mix_by_complexity_acuity_cy2025.csv"))
imaging_duration <- read_csv_file(file_path = file.path(data_dir, "08_imaging_duration_distribution_by_modality_cy2025.csv"))

input_data_list <- list(
  interarrival_by_hourly = interarrival_by_hourly,
  case_mix = case_mix,
  arrival_to_first_seen_summary = arrival_to_first_seen_summary,
  arrival_to_first_seen_empirical = arrival_to_first_seen_empirical,
  generic_workup_summary = generic_workup_summary,
  generic_workup_empirical = generic_workup_empirical,
  imaging_probability = imaging_probability,
  imaging_duration = imaging_duration
)