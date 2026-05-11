# 3_model/build_patient_trajectory.R

source("3_model/generators/sample_attributes.R")
source("3_model/distributions/sample_first_seen_delay.R")
source("3_model/distributions/sample_workup_duration.R")
source("3_model/distributions/sample_imaging_decision.R")
source("3_model/distributions/sample_imaging_duration.R")
source("3_model/distributions/sample_consult_decision.R")
source("3_model/distributions/sample_consult_los_adjustment.R")

build_patient_trajectory <- function(case_mix_data,
                                     first_seen_data,
                                     workup_data,
                                     imaging_probability_data,
                                     imaging_duration_data,
                                     consult_probability_data,
                                     consult_los_data,
                                     current_quarter,
                                     env) {
  
  trajectory("patient_path") %>%
    
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
    
    seize("triage_rn", 1) %>%
    
    timeout(function() {
      patient_acuity <- get_attribute(env, "acuity")
      
      sample_first_seen_delay(
        first_seen_summary_data = first_seen_data,
        patient_acuity = patient_acuity
      )
    }) %>%
    
    release("triage_rn", 1) %>%
    
    seize("core_ed_space", 1) %>%
    
    timeout(function() {
      patient_complexity_bucket <- get_attribute(env, "complexity_bucket")
      
      sample_workup_duration(
        workup_summary_data = workup_data,
        patient_complexity_bucket = patient_complexity_bucket
      )
    }) %>%
    
    timeout(function() {
      patient_acuity <- get_attribute(env, "acuity")
      
      modality <- sample_imaging_decision(
        imaging_probability_data = imaging_probability_data,
        patient_acuity = patient_acuity
      )
      
      set_attribute(env, "imaging_modality", modality)
      
      sample_imaging_duration(
        imaging_duration_data = imaging_duration_data,
        modality = modality
      )
    }) %>%
    
    timeout(function() {
      patient_acuity <- get_attribute(env, "acuity")
      
      consult_group <- sample_consult_decision(
        consult_probability_data = consult_probability_data,
        patient_acuity = patient_acuity
      )
      
      set_attribute(env, "consult_group", consult_group)
      
      sample_consult_los_adjustment(
        consult_los_data = consult_los_data,
        patient_acuity = patient_acuity,
        consult_group = consult_group
      )
    }) %>%
    
    release("core_ed_space", 1)
}