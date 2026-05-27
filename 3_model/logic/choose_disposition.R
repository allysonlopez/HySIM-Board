# Disposition codes:
# 1 = discharge
# 2 = observation
# 3 = admit
# 4 = transfer
choose_disposition_code <- function(triage_priority, complexity_code, behavioral_health_flag) {
  if (behavioral_health_flag == 1) {
    return(sample(1:4, size = 1, prob = c(0.50, 0.20, 0.25, 0.05)))
  }
  
  if (is.na(triage_priority) || triage_priority == 0) {
    base_probabilities <- c(0.70, 0.08, 0.20, 0.02)
  } else if (triage_priority <= 1) {
    base_probabilities <- c(0.25, 0.05, 0.60, 0.10)
  } else if (triage_priority == 2) {
    base_probabilities <- c(0.45, 0.08, 0.43, 0.04)
  } else if (triage_priority == 3) {
    base_probabilities <- c(0.68, 0.08, 0.22, 0.02)
  } else {
    base_probabilities <- c(0.90, 0.04, 0.05, 0.01)
  }
  
  if (!is.na(complexity_code) && complexity_code >= 5) {
    base_probabilities[1] <- max(0.05, base_probabilities[1] - 0.15)
    base_probabilities[3] <- base_probabilities[3] + 0.15
  }
  
  sample(1:4, size = 1, prob = normalize_probabilities(base_probabilities))
}

decode_disposition <- function(disposition_code) {
  dplyr::case_when(
    disposition_code == 1 ~ "discharge",
    disposition_code == 2 ~ "observation",
    disposition_code == 3 ~ "admit",
    disposition_code == 4 ~ "transfer",
    TRUE ~ "unknown"
  )
}
