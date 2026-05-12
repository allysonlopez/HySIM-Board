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
                                     consult_probability_data,
                                     first_seen_scale = 0.35,
                                     include_boarding = TRUE) {
  
  ed_care_path <- function(resource_name) {
    trajectory(paste0(resource_name, "_path")) %>%
      seize(resource_name, 1) %>%
      timeout(function() {
        sample_first_seen_delay(
          first_seen_empirical_data,
          first_seen_summary_data,
          get_attribute(env, "acuity"),
          scale_factor = first_seen_scale
        )
      }) %>%
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
      timeout(function() {
        sample_consult_duration(
          consult_probability_data,
          get_attribute(env, "acuity")
        )
      }) %>%
      set_attribute(
        keys = "admitted",
        values = function() {
          sample_admission_flag(
            get_attribute(env, "acuity"),
            get_attribute(env, "complexity_bucket")
          )
        }
      ) %>%
      branch(
        option = function() {
          if (isTRUE(include_boarding) && get_attribute(env, "admitted") == 1) return(1)
          return(2)
        },
        continue = c(TRUE, TRUE),
        trajectory("boarding_path") %>%
          seize("inpatient_bed", 1) %>%
          set_attribute(
            keys = "boarding_time",
            values = function() {
              sample_boarding_duration(
                get_attribute(env, "acuity"),
                get_attribute(env, "complexity_bucket")
              )
            }
          ) %>%
          timeout(function() get_attribute(env, "boarding_time")) %>%
          release("inpatient_bed", 1),
        trajectory("discharge_path")
      ) %>%
      release(resource_name, 1)
  }
  
  core_path <- ed_care_path("core_ed_space")
  vertical_path <- ed_care_path("vertical_flex_space")
  rapid_path <- ed_care_path("rapid_treatment_space")
  
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
        
        # More realistic MVP routing:
        # - Very sick patients go to core ED.
        # - Medium acuity/high complexity also goes to core.
        # - Lower acuity/lower complexity can use rapid treatment.
        # - Everyone else uses vertical/flex space.
        # This avoids sending every high-complexity patient to core automatically.
        if (acuity <= 2 || (acuity == 3 && complexity >= 5) || complexity == 6) {
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
