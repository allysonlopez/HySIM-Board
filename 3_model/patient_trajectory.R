# 3_model/patient_trajectory.R

build_patient_trajectory <- function(env,
                                     current_quarter,
                                     case_mix_data,
                                     first_seen_empirical_data,
                                     first_seen_summary_data,
                                     consult_probability_data,
                                     workup_empirical_data,
                                     workup_summary_data,
                                     imaging_probability_data,
                                     imaging_duration_data,
                                     first_seen_scale = 1.0) {
  
  trajectory("patient_path") %>%
    
    # assign patient characteristics
    set_attribute(
      keys = c("acuity", "complexity_bucket"),
      values = function() {
        assign_patient_attributes(
          case_mix_data = case_mix_data,
          current_time = simmer::now(env),
          current_quarter = current_quarter
        )
      }
    ) %>%
    
    # patient takes one ED bed
    seize("ed_bed", 1) %>%
    
    # sim time from arrival to first provider
    set_attribute("first_seen_duration", function() {
      sample_first_seen_delay(
        first_seen_empirical_data = first_seen_empirical_data,
        first_seen_summary_data = first_seen_summary_data,
        acuity = get_attribute(env, "acuity"),
        scale_factor = first_seen_scale
      )
    }) %>%
    timeout_from_attribute("first_seen_duration") %>%
    
    # sim consult time if a consult is needed
    set_attribute("consult_duration", function() {
      sample_consult_duration(
        consult_probability_data = consult_probability_data,
        acuity = get_attribute(env, "acuity")
      )
    }) %>%
    timeout_from_attribute("consult_duration") %>%
    
    # sim general workup time
    set_attribute("workup_duration", function() {
      sample_workup_duration(
        workup_empirical_data = workup_empirical_data,
        workup_summary_data = workup_summary_data,
        complexity_bucket = get_attribute(env, "complexity_bucket")
      )
    }) %>%
    timeout_from_attribute("workup_duration") %>%
    
    # sim imaging time if imaging is needed
    set_attribute("imaging_duration", function() {
      sample_imaging_duration(
        imaging_probability_data = imaging_probability_data,
        imaging_duration_data = imaging_duration_data,
        acuity = get_attribute(env, "acuity")
      )
    }) %>%
    timeout_from_attribute("imaging_duration") %>%
    
    # patient leaves ED bed
    release("ed_bed", 1)
}