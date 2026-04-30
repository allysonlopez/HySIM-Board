source("3_model/generators/helper_functions.R")
source("3_model/generators/sample_attributes.R")
source("3_model/generators/sample_interarrival.R")

interarrival_data <- read.csv("1_data/01_interarrival_by_timeblock_hourly_cy2025.csv")
case_mix_data <- read.csv("1_data/02_case_mix_by_timeblock_cy2025.csv")

current_sim_time_min <- 8 * 60 # 8am morning at Monday
current_quarter <- 2

patients <- data.frame()

for (patient_id in 1:20) {
  
  interarrival_time_rows <- sample_time_rows(data = interarrival_data,
                                             current_sim_time_min = current_sim_time_min,
                                             current_quarter = current_quarter)
  
  minutes_until_next_patient <- sample_interarrival(time_rows = interarrival_time_rows)
  
  current_sim_time_min <- current_sim_time_min + minutes_until_next_patient
  
  case_mix_time_rows <- sample_time_rows(data = case_mix_data,
                                         current_sim_time_min = current_sim_time_min,
                                         current_quarter = current_quarter)
  
  patient_attributes <- assign_patient_attributes(
    case_mix_data = case_mix_data,
    time_rows = case_mix_time_rows
  )
  
  one_patient <- data.frame(
    patient_id = patient_id,
    arrival_time_min = current_sim_time_min,
    acuity = patient_attributes$acuity,
    complexity_bucket = patient_attributes$complexity_bucket,
    arrival_mode = patient_attributes$arrival_mode,
    age_group = patient_attributes$age_group,
    behavioral_health_flag = patient_attributes$behavioral_health_flag
  )
  
  patients <- bind_rows(patients, one_patient)
}

patients