scenario_baseline <- function(config) {
  config
}

scenario_add_core_spaces <- function(config) {
  config$core_ed_spaces <- config$core_ed_spaces + 5
  config
}

scenario_expand_rta <- function(config) {
  config$rta_spaces <- config$rta_spaces + 4
  config
}

scenario_faster_imaging <- function(config) {
  config$max_imaging_acquisition_min <- config$max_imaging_acquisition_min * 0.75
  config$max_imaging_interpretation_min <- config$max_imaging_interpretation_min * 0.75
  config
}

scenario_faster_consult <- function(config) {
  config$max_consult_adjustment_min <- config$max_consult_adjustment_min * 0.75
  config
}

get_scenarios <- function() {
  list(
    baseline = scenario_baseline,
    add_core_spaces = scenario_add_core_spaces,
    expand_rta = scenario_expand_rta,
    faster_imaging = scenario_faster_imaging,
    faster_consult = scenario_faster_consult
  )
}
