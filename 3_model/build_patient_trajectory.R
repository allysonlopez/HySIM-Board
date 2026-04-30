build_patient_trajectory <- function(case_mix_data, 
                                     first_seen_data, 
                                     workup_data,
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
    seize("ed_bed", 1) %>%
    
    # missing provider here
    
    timeout(function() {
      patient_acuity <- get_attribute(env, "acuity")
      
      sample_first_seen_delay(
        first_seen_summary_data = first_seen_data,
        patient_acuity = patient_acuity
      )
    }) %>%
    
    # release provider 
    timeout(function() {
      patient_complexity_bucket <- get_attribute(env, "complexity_bucket")
      
      sample_workup_duration(
        workup_summary_data = workup_data,
        patient_complexity_bucket = patient_complexity_bucket
      )
    }) %>%
    release("ed_bed", 1)
}
