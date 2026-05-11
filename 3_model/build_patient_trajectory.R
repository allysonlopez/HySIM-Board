# 3_model/build_patient_trajectory.R

source("3_model/generators/sample_attributes.R")
source("3_model/distributions/sample_first_seen_delay.R")
source("3_model/distributions/sample_workup_duration.R")
source("3_model/distributions/sample_imaging_decision.R")
source("3_model/distributions/sample_imaging_duration.R")
source("3_model/distributions/sample_consult_decision.R")

build_patient_trajectory <- function(case_mix_data,
                                     first_seen_data,
                                     workup_data,
                                     imaging_probability_data,
                                     imaging_duration_data,
                                     consult_probability_data,
                                     current_quarter,
                                     env) {
  
  trajectory("patient_path") %>%
    
    # 1. Assign patient characteristics
    set_attribute(
      keys = c(
        "acuity",
        "complexity_bucket",
        "arrival_mode",
        "age_group",
        "behavioral_health_flag"
      ),
      values = function() {
        assign_patient_attributes(
          data = case_mix_data,
          current_time = now(env),
          current_quarter = current_quarter
        )
      }
    ) %>%
    
    # 2. Front-end / triage nurse constraint
    seize("triage_rn", 1) %>%
    
    timeout(function() {
      patient_acuity <- get_attribute(env, "acuity")
      
      sample_first_seen_delay(
        first_seen_summary_data = first_seen_data,
        patient_acuity = patient_acuity
      )
    }) %>%
    
    release("triage_rn", 1) %>%
    
    # 3. Main ED treatment space constraint
    seize("core_ed_space", 1) %>%
    
    # 4. Generic workup duration
    timeout(function() {
      patient_complexity_bucket <- get_attribute(env, "complexity_bucket")
      
      sample_workup_duration(
        workup_summary_data = workup_data,
        patient_complexity_bucket = patient_complexity_bucket
      )
    }) %>%
    
    # 5. Imaging decision + imaging duration
    timeout(function() {
      patient_acuity <- get_attribute(env, "acuity")
      
      modality <- sample_imaging_decision(
        imaging_probability_data = imaging_probability_data,
        patient_acuity = patient_acuity
      )
      
      sample_imaging_duration(
        imaging_duration_data = imaging_duration_data,
        modality = modality
      )
    }) %>%
    
    # 6. Consult decision only
    # No consult LOS adjustment because we are not using file 10.
    timeout(function() {
      patient_acuity <- get_attribute(env, "acuity")
      
      consult_group <- sample_consult_decision(
        consult_probability_data = consult_probability_data,
        patient_acuity = patient_acuity
      )
      
      # For MVP, consult is assigned but does not add time.
      return(0)
    }) %>%
    
    # 7. Patient leaves ED treatment space
    release("core_ed_space", 1)
}