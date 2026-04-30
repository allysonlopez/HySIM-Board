source("3_model/generators/helper_functions.R")

sample_one_attributes <- function(case_mix_data, attr_name, time_rows) {
  
  matching_attribute_rows <- time_rows[time_rows$attribute_name == attr_name,]
  
  if (nrow(matching_attribute_rows) == 0) {
    matching_attribute_rows <- case_mix_data[
      case_mix_data$attribute_name == attr_name,
    ]
    warning("No matching rows. Using the whole datasets") 
  }
  
  sample(
    matching_attribute_rows$attribute_value,
    size = 1,
    replace = TRUE,
    prob = matching_attribute_rows$probability
  )
  
}

assign_patient_attributes <- function(data, current_time, current_quarter) {
  
  time_rows <- sample_time_rows(data, current_time, current_quarter)
  
  c(
    acuity = encode_acuity(sample_one_attributes(data, 
                                              "acuity", 
                                              time_rows)),
    
    complexity_bucket = encode_complexity_bucket(sample_one_attributes(data, 
                                                                       "complexity_bucket", 
                                                                       time_rows)),
    
    arrival_mode = encode_arrival_mode(sample_one_attributes(data, 
                                                             "arrival_mode", 
                                                             time_rows)),
    
    age_group = encode_age_group(sample_one_attributes(data, 
                                                       "age_group", 
                                                       time_rows)),
    
    behavioral_health_flag = as.numeric(sample_one_attributes(data, 
                                                              "behavioral_health_flag", 
                                                              time_rows))
  )
}

encode_acuity <- function(acuity) {
  if (acuity == "1") return(1)
  if (acuity == "2") return(2)
  if (acuity == "3") return(3)
  if (acuity == "4") return(4)
  if (acuity == "5") return(5)
  return(0)  # UNKNOWN
}

encode_complexity_bucket <- function(complexity_bucket) {
  if (complexity_bucket == "minimal") return(1)
  if (complexity_bucket == "straightforward") return(2)
  if (complexity_bucket == "low") return(3)
  if (complexity_bucket == "moderate") return(4)
  if (complexity_bucket == "high") return(5)
  if (complexity_bucket == "critical_care") return(6)
  return(0)
}

encode_arrival_mode <- function(arrival_mode) {
  if (arrival_mode == "self_presented") return(1)
  if (arrival_mode == "ground_ambulance") return(2)
  if (arrival_mode == "police_custody") return(3)
  if (arrival_mode == "hospital_transport") return(4)
  if (arrival_mode == "air_transport") return(5)
  return(0)
}

encode_age_group <- function(age_group) {
  if (age_group == "<18") return(1)
  if (age_group == "18-64") return(2)
  if (age_group == "65-84") return(3)
  if (age_group == "85+") return(4)
  return(0)
}


