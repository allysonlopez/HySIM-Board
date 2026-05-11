sample_consult_decision <- function(consult_probability_data, patient_acuity) {
  
  matching_row <- consult_probability_data[
    as.character(consult_probability_data$triage_priority) == as.character(patient_acuity),
  ]
  
  if (nrow(matching_row) == 0) {
    return("none")
  }
  
  consult_prob <- matching_row$needs_consult_prob[1]
  
  gets_consult <- rbinom(
    n = 1,
    size = 1,
    prob = consult_prob
  )
  
  if (gets_consult == 0) {
    return("none")
  }
  
  group_columns <- names(matching_row)[grepl("_prob$", names(matching_row))]
  group_columns <- setdiff(group_columns, "needs_consult_prob")
  
  if (length(group_columns) == 0) {
    return("consult_unknown")
  }
  
  consult_group <- sample(
    x = group_columns,
    size = 1,
    replace = TRUE,
    prob = as.numeric(matching_row[1, group_columns])
  )
  
  consult_group <- gsub("_prob$", "", consult_group)
  
  return(consult_group)
}