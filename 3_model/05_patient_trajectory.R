# Simulates one patient's path through the ED
# This does not use simmer yet.
# It helps us test whether the patient logic works correctly.

simulate_one_patient_path <- function(quarter_current,
                                      dow_current,
                                      hour_current) {
  
  # Step 1: sample patient characteristics
  patient_attributes <- sample_patient_attributes(
    quarter_current = quarter_current,
    dow_current = dow_current,
    hour_current = hour_current
  )
  
  # Pull patient attributes into simple objects
  acuity_value <- patient_attributes$acuity
  complexity_value <- patient_attributes$complexity
  age_group_value <- patient_attributes$age_group
  arrival_mode_value <- patient_attributes$arrival_mode
  
  # Step 2: sample time from arrival to first provider
  first_seen_duration <- sample_first_seen_duration(
    acuity_value = acuity_value
  )
  
  # Step 3: sample generic workup duration
  workup_duration <- sample_workup_duration(
    complexity_value = complexity_value
  )
  
  # Step 4: decide whether patient needs imaging
  needs_imaging <- sample_imaging_needed(
    acuity_value = acuity_value
  )
  
  # Step 5: if imaging is needed, sample modality and duration
  if (needs_imaging == 1) {
    
    imaging_type <- sample_imaging_modality(
      acuity_value = acuity_value
    )
    
    imaging_duration <- sample_imaging_duration(
      imaging_type_value = imaging_type
    )
    
  } else {
    
    imaging_type <- "none"
    imaging_duration <- 0
  }
  
  # Step 6: decide whether patient needs consult
  needs_consult <- sample_consult_needed(
    acuity_value = acuity_value
  )
  
  # Step 7: if consult is needed, sample consult group
  if (needs_consult == 1) {
    
    consult_group <- sample_consult_group(
      acuity_value = acuity_value
    )
    
  } else {
    
    consult_group <- "none"
  }
  
  # Step 8: calculate total modeled ED time
  # For now, consult has no duration because we do not have consult duration data yet.
  total_duration <- first_seen_duration +
    workup_duration +
    imaging_duration
  
  # Step 9: return one clean row describing this patient
  tibble(
    acuity = acuity_value,
    complexity = complexity_value,
    age_group = age_group_value,
    arrival_mode = arrival_mode_value,
    s_duration = first_seen_duration,
    workup_duration = workup_duration,
    needs_imaging = needs_imaging,
    imaging_type = imaging_type,
    imaging_duration = imaging_duration,
    needs_consult = needs_consult,
    consult_group = consult_group,
    total_duration = total_duration
  )
}


