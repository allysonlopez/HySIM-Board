# 3_model/patient_trajectory.R
# Purpose:
#   Define the path each simulated patient follows through the ED.



# What it does:
#   Builds the full simmer trajectory used by every patient in the simulation.
#
# Main steps:
#   1) Assign patient acuity and complexity using case-mix probabilities.
#   2) Use acuity and complexity to choose a route:
#        route 1 = core ED space
#        route 2 = vertical/flex space
#        route 3 = rapid treatment space
#   3) Patient seizes the assigned ED space. If the space is full, they wait.
#   4) Patient experiences a first-seen delay.
#   5) Patient experiences a generic workup duration.
#   6) Patient may receive imaging, adding imaging duration if triggered.
#   7) Patient releases the ED space and exits the model.

build_patient_trajectory <- function(env,
                                     current_quarter,
                                     case_mix_data,
                                     first_seen_empirical_data,
                                     first_seen_summary_data,
                                     workup_empirical_data,
                                     workup_summary_data,
                                     imaging_probability_data,
                                     imaging_duration_data,
                                     first_seen_scale = 0.35) {
  
  
  #   Creates the care sequence for one ED resource group. The same clinical steps
  #   are used for core ED, vertical/flex, and rapid treatment; the only thing
  #   that changes is which resource the patient seizes.
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
        
        # Routing logic for the MVP:
        # - Acuity 1-2 patients are sent to core ED because they are higher acuity.
        # - Acuity 3 patients with high complexity also go to core ED.
        # - Critical-care complexity goes to core ED regardless of acuity.
        # - Lower-acuity/lower-complexity patients go to rapid treatment.
        # - Everyone else goes to vertical/flex space.
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
