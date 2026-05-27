read_model_csv <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Input file not found: ", file_path)
  }
  readr::read_csv(file_path, show_col_types = FALSE)
}

load_model_input_data <- function(data_dir = "1_data") {
  list(
    interarrival_table = read_model_csv(file.path(data_dir, "01_interarrival_by_timeblock_hourly_cy2025.csv")),
    case_mix_table = read_model_csv(file.path(data_dir, "02_case_mix_by_timeblock_cy2025.csv")),
    first_seen_summary = read_model_csv(file.path(data_dir, "03_arrival_to_first_seen_distribution_by_triage_cy2025.csv")),
    first_seen_empirical = read_model_csv(file.path(data_dir, "04_arrival_to_first_seen_empirical_deid_cy2025.csv")),
    workup_summary = read_model_csv(file.path(data_dir, "05_generic_workup_duration_distribution_by_complexity_cy2025.csv")),
    workup_empirical = read_model_csv(file.path(data_dir, "06_generic_workup_duration_empirical_deid_cy2025.csv")),
    imaging_probability = read_model_csv(file.path(data_dir, "07_imaging_probability_and_modality_mix_by_acuity_historical_2018_2022.csv")),
    imaging_duration = read_model_csv(file.path(data_dir, "08_imaging_duration_distribution_by_modality_historical_2018_2022.csv")),
    consult_probability = read_model_csv(file.path(data_dir, "09_consult_probability_and_group_mix_by_acuity_historical_2018_2022.csv")),
    consult_los_adjustment = read_model_csv(file.path(data_dir, "10_consult_los_adjustment_modeling_table_deid_historical_2018_2022.csv"))
  )
}
