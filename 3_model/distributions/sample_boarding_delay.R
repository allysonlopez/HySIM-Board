#
#disposition_code = 1 → discharge
#disposition_code = 2 → observation
#disposition_code = 3 → admit
#disposition_code = 4 → transfer


sample_post_disposition_delay <- function(disposition_code,
                                          complexity_code,
                                          config) {
  is_observation <- disposition_code == 2
  is_admission <- disposition_code == 3

  if (is_observation) {
    return(sample_from_weibull(
      median_min = config$observation_boarding_median_min,
      p90_min = config$observation_boarding_p90_min,
      max_allowed_min = config$max_boarding_delay_min
    ))
  }

  if (is_admission) {
    complexity_multiplier <- dplyr::case_when(
      complexity_code >= 6 ~ 1.3,
      complexity_code >= 5 ~ 1.15,
      TRUE ~ 1
    )

    return(sample_from_weibull(
      median_min = config$admission_boarding_median_min * complexity_multiplier,
      p90_min = config$admission_boarding_p90_min * complexity_multiplier,
      max_allowed_min = config$max_boarding_delay_min
    ))
  }

  0
}