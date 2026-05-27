sample_arrival_to_first_provider_delay <- function(first_seen_summary,
                                                    triage_priority,
                                                    config) {
  matching_row <- first_seen_summary %>%
    dplyr::filter(as.character(.data$triage_priority) == as.character(triage_priority)) %>%
    safe_first_row()

  if (is.null(matching_row)) {
    matching_row <- first_seen_summary %>%
      dplyr::filter(.data$triage_priority == "UNKNOWN") %>%
      safe_first_row()
  }

  if (is.null(matching_row)) {
    return(config$default_front_end_delay_min)
  }

  sample_from_weibull(
    median_min = matching_row$median_min,
    p90_min = matching_row$p90_min,
    max_allowed_min = config$max_front_end_delay_min
  )
}