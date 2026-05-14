sample_patient_attributes <- function(quarter_current,
                                      dow_current,
                                      hour_current) {
  
  acuity_value <- sample_attribute(
    quarter_current,
    dow_current,
    hour_current,
    "acuity"
  )
  
  complexity_value <- sample_attribute(
    quarter_current,
    dow_current,
    hour_current,
    "complexity_bucket"
  )
  
  age_group_value <- sample_attribute(
    quarter_current,
    dow_current,
    hour_current,
    "age_group"
  )
  
  arrival_mode_value <- sample_attribute(
    quarter_current,
    dow_current,
    hour_current,
    "arrival_mode"
  )
  
  tibble(
    acuity = acuity_value,
    complexity = complexity_value,
    age_group = age_group_value,
    arrival_mode = arrival_mode_value
  )
}