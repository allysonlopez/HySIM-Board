# 3_model/patient_trajectory.R
# Defines the path one patient follows through the ED.

build_patient_trajectory <- function(env,
                                     current_quarter,
                                     case_mix_data,
                                     first_seen_empirical_data,
                                     first_seen_summary_data,
                                     workup_empirical_data,
                                     workup_summary_data,
                                     imaging_probability_data,
                                     imaging_duration_data,
                                     consult_probability_data) {
  
  core_path <- trajectory("core_ed_path") %>%
    timeout(function() {
      sample_first_seen_delay(
        first_seen_empirical_data,
        first_seen_summary_data,
        get_attribute(env, "acuity")
      )
    }) %>%
    seize("core_ed_space", 1) %>%
    timeout(function() {
      sample_workup_duration(
        workup_empirical_data,
        workup_summary_data,
        get_attribute(env, "complexity_bucket")
      )
    }) %>%
    timeout(function() {
      sample_imaging_duration(
        imaging_probability_data,
        imaging_duration_data,
        get_attribute(env, "acuity")
      )
    }) %>%
    release("core_ed_space", 1)
  
  vertical_path <- trajectory("vertical_flex_path") %>%
    timeout(function() {
      sample_first_seen_delay(
        first_seen_empirical_data,
        first_seen_summary_data,
        get_attribute(env, "acuity")
      )
    }) %>%
    seize("vertical_flex_space", 1) %>%
    timeout(function() {
      sample_workup_duration(
        workup_empirical_data,
        workup_summary_data,
        get_attribute(env, "complexity_bucket")
      )
    }) %>%
    release("vertical_flex_space", 1)
  
  rapid_path <- trajectory("rapid_treatment_path") %>%
    timeout(function() {
      sample_first_seen_delay(
        first_seen_empirical_data,
        first_seen_summary_data,
        get_attribute(env, "acuity")
      )
    }) %>%
    seize("rapid_treatment_space", 1) %>%
    timeout(function() {
      sample_workup_duration(
        workup_empirical_data,
        workup_summary_data,
        get_attribute(env, "complexity_bucket")
      )
    }) %>%
    release("rapid_treatment_space", 1)
  
  trajectory("patient_path") %>%
    set_attribute(
      keys = c("acuity", "complexity_bucket"),
      values = function() {
        assign_patient_attributes(
          case_mix_data = case_mix_data,
          current_time = now(env),
          current_quarter = current_quarter
        )
      }
    ) %>%
    set_attribute(
      keys = "route",
      values = function() {
        acuity <- get_attribute(env, "acuity")
        complexity <- get_attribute(env, "complexity_bucket")
        
        if (acuity <= 2 || complexity >= 5) {
          return(1)   # core ED
        } else if (acuity >= 4 && complexity <= 3) {
          return(3)   # rapid treatment
        } else {
          return(2)   # vertical/flex
        }
      }
    ) %>%
    branch(
      option = function() get_attribute(env, "route"),
      continue = c(TRUE, TRUE, TRUE),
      core_path,
      vertical_path,
      rapid_path
    )
}