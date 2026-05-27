consult_probability_columns <- c(
  "social_work_case_management_prob",
  "psych_behavioral_health_prob",
  "ancillary_support_prob",
  "cardiology_prob",
  "neuro_neurosurgery_prob",
  "orthopedics_prob",
  "surgery_specialty_prob",
  "medicine_specialty_prob",
  "other_specialty_prob",
  "radiology_ir_prob",
  "other_unknown_prob"
)

choose_consult_attributes <- function(consult_probability, triage_priority) {
  matching_row <- consult_probability %>%
    dplyr::filter(as.character(.data$triage_priority) == as.character(triage_priority)) %>%
    safe_first_row()
  
  if (is.null(matching_row)) {
    return(c(needs_consult = 0, consult_group_code = 0))
  }
  
  consult_probability_value <- as.numeric(matching_row$needs_consult_prob)
  if (is.na(consult_probability_value)) {
    consult_probability_value <- 0
  }
  consult_probability_value <- min(max(consult_probability_value, 0), 1)
  
  needs_consult <- stats::rbinom(1, size = 1, prob = consult_probability_value)
  
  if (needs_consult == 0) {
    return(c(needs_consult = 0, consult_group_code = 0))
  }
  
  group_probabilities <- normalize_probabilities(unlist(matching_row[consult_probability_columns], use.names = FALSE))
  consult_group_code <- sample(seq_along(group_probabilities), size = 1, prob = group_probabilities)
  
  c(needs_consult = 1, consult_group_code = consult_group_code)
}
