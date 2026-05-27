set_patient_attributes <- function(trj, input_data, env, config) {
  trj %>%
    simmer::set_attribute(
      keys = c(
        "triage_priority",
        "complexity_code",
        "arrival_mode_code",
        "age_group_code",
        "behavioral_health_flag"
      ),
      values = function() {
        assign_patient_attributes(
          case_mix_table = input_data$case_mix_table,
          current_time_min = simmer::now(env),
          config = config
        )
      }
    ) %>%
    simmer::set_attribute(
      "care_area_code",
      function() {
        choose_care_area_code(
          triage_priority = simmer::get_attribute(env, "triage_priority"),
          complexity_code = simmer::get_attribute(env, "complexity_code"),
          behavioral_health_flag = simmer::get_attribute(env, "behavioral_health_flag")
        )
      }
    ) %>%
    simmer::set_attribute(
      "disposition_code",
      function() {
        choose_disposition_code(
          triage_priority = simmer::get_attribute(env, "triage_priority"),
          complexity_code = simmer::get_attribute(env, "complexity_code"),
          behavioral_health_flag = simmer::get_attribute(env, "behavioral_health_flag")
        )
      }
    ) %>%
    simmer::set_attribute(
      keys = c("needs_imaging", "imaging_type_code"),
      values = function() {
        choose_imaging_attributes(
          imaging_probability = input_data$imaging_probability,
          triage_priority = simmer::get_attribute(env, "triage_priority")
        )
      }
    ) %>%
    simmer::set_attribute(
      keys = c("needs_consult", "consult_group_code"),
      values = function() {
        choose_consult_attributes(
          consult_probability = input_data$consult_probability,
          triage_priority = simmer::get_attribute(env, "triage_priority")
        )
      }
    )
}

set_patient_durations <- function(trj, input_data, env, config) {
  trj %>%
    simmer::set_attribute(
      "front_end_delay_min",
      function() {
        sample_arrival_to_first_provider_delay(
          first_seen_summary = input_data$first_seen_summary,
          triage_priority = simmer::get_attribute(env, "triage_priority"),
          config = config
        )
      }
    ) %>%
    simmer::set_attribute(
      "workup_duration_min",
      function() {
        sample_workup_time(
          workup_summary = input_data$workup_summary,
          complexity_code = simmer::get_attribute(env, "complexity_code"),
          config = config
        )
      }
    ) %>%
    simmer::set_attribute(
      "imaging_acquisition_duration_min",
      function() {
        sample_imaging_acquisition_duration(
          imaging_duration = input_data$imaging_duration,
          imaging_type_code = simmer::get_attribute(env, "imaging_type_code"),
          config = config
        )
      }
    ) %>%
    simmer::set_attribute(
      "imaging_interpretation_duration_min",
      function() {
        sample_imaging_interpretation_duration(
          imaging_duration = input_data$imaging_duration,
          imaging_type_code = simmer::get_attribute(env, "imaging_type_code"),
          config = config
        )
      }
    ) %>%
    simmer::set_attribute(
      "consult_delay_min",
      function() {
        sample_consult_los_adjustment(
          consult_group_code = simmer::get_attribute(env, "consult_group_code"),
          config = config
        )
      }
    ) %>%
    simmer::set_attribute(
      "boarding_delay_min",
      function() {
        sample_post_disposition_delay(
          disposition_code = simmer::get_attribute(env, "disposition_code"),
          complexity_code = simmer::get_attribute(env, "complexity_code"),
          config = config
        )
      }
    )
}

add_imaging_step <- function(trj, env) {
  trj %>%
    simmer::branch(
      option = function() {
        ifelse(simmer::get_attribute(env, "needs_imaging") == 1, 2, 1)
      },
      continue = c(TRUE, TRUE),
      simmer::trajectory("no_imaging"),
      simmer::trajectory("imaging") %>%
        simmer::seize("imaging_resource", 1) %>%
        simmer::timeout(function() {
          simmer::get_attribute(env, "imaging_acquisition_duration_min")
        }) %>%
        simmer::release("imaging_resource", 1) %>%
        simmer::timeout(function() {
          simmer::get_attribute(env, "imaging_interpretation_duration_min")
        })
    )
}

add_consult_step <- function(trj, env) {
  trj %>%
    simmer::branch(
      option = function() {
        ifelse(simmer::get_attribute(env, "needs_consult") == 1, 2, 1)
      },
      continue = c(TRUE, TRUE),
      simmer::trajectory("no_consult"),
      simmer::trajectory("consult_los_adjustment") %>%
        simmer::timeout(function() simmer::get_attribute(env, "consult_delay_min"))
    )
}

add_boarding_step <- function(trj, env) {
  trj %>%
    simmer::branch(
      option = function() {
        disposition_code <- simmer::get_attribute(env, "disposition_code")
        ifelse(disposition_code %in% c(2, 3), 2, 1)
      },
      continue = c(TRUE, TRUE),
      simmer::trajectory("no_boarding"),
      simmer::trajectory("boarding") %>%
        simmer::timeout(function() simmer::get_attribute(env, "boarding_delay_min"))
    )
}

add_common_clinical_steps <- function(trj, env, config) {
  trj %>%
    simmer::seize("provider", 1) %>%
    simmer::timeout(config$provider_evaluation_min) %>%
    simmer::release("provider", 1) %>%
    simmer::timeout(function() simmer::get_attribute(env, "workup_duration_min")) %>%
    add_imaging_step(env = env) %>%
    add_consult_step(env = env)
}

build_area_specific_path <- function(resource_name, env, config) {
  simmer::trajectory(paste0(resource_name, "_path")) %>%
    simmer::seize(resource_name, 1) %>%
    add_common_clinical_steps(env = env, config = config) %>%
    simmer::release(resource_name, 1) %>%
    add_boarding_step(env = env)
}

build_patient_trajectory <- function(input_data, env, config) {
  core_path <- build_area_specific_path("core_ed_space", env, config)
  vertical_path <- build_area_specific_path("vertical_flex_space", env, config)
  rta_path <- build_area_specific_path("rta_space", env, config)
  
  simmer::trajectory("patient_path") %>%
    set_patient_attributes(input_data = input_data, env = env, config = config) %>%
    set_patient_durations(input_data = input_data, env = env, config = config) %>%
    simmer::seize("triage_rn", 1) %>%
    simmer::timeout(config$triage_service_min) %>%
    simmer::release("triage_rn", 1) %>%
    simmer::timeout(function() simmer::get_attribute(env, "front_end_delay_min")) %>%
    simmer::branch(
      option = function() {
        care_area_code <- simmer::get_attribute(env, "care_area_code")
        ifelse(care_area_code %in% 1:3, care_area_code, 2)
      },
      continue = c(TRUE, TRUE, TRUE),
      core_path,
      vertical_path,
      rta_path
    )
}
