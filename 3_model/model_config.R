default_model_config <- function() {
  list(
    current_quarter = 2,
    start_day_of_week_num = 1,
    start_hour_of_day = 8,
    warmup_days = 7,
    analysis_days = 7,
    followup_days = 7,
    
    # fixed
    core_ed_spaces = 43,
    vertical_flex_spaces = 41,
    rta_spaces = 8,
    triage_rns = 2,
    
    # placeholder
    providers = 8,
    imaging_resources = 3,
    provider_evaluation_min = 10,
    triage_service_min = 5,
    
    default_front_end_delay_min = 30,
    default_workup_duration_min = 90,
    
    default_imaging_acquisition_min = 30,
    default_imaging_interpretation_min = 60,
    
    max_imaging_acquisition_min = 240,
    max_imaging_interpretation_min = 720,
    
    default_consult_los_adjustment_min = 120,
    fallback_weibull_shape = 1.5,
    
    consult_los_adjustment_median_min = 120,
    consult_los_adjustment_p90_min = 360,
    max_front_end_delay_min = 1440,
    max_workup_duration_min = 1440,
    max_consult_adjustment_min = 1440,
    max_boarding_delay_min = 2880,
    observation_boarding_median_min = 360,
    observation_boarding_p90_min = 900,
    admission_boarding_median_min = 600,
    admission_boarding_p90_min = 1800,
    random_seed = 42
  )
}

get_total_simulation_minutes <- function(config) {
  (config$warmup_days + config$analysis_days + config$followup_days) * 24 * 60
}

get_warmup_minutes <- function(config) {
  config$warmup_days * 24 * 60
}

get_analysis_start_minutes <- function(config) {
  config$warmup_days * 24 * 60
}

get_analysis_end_minutes <- function(config) {
  (config$warmup_days + config$analysis_days) * 24 * 60
}