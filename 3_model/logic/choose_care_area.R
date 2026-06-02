# 3_model/logic/choose_care_area.R
#
# Care area codes:
# 1 = core_ed_space
# 2 = vertical_flex_space
# 3 = rta_space

choose_care_area_code <- function(triage_priority,
                                  complexity_code,
                                  behavioral_health_flag,
                                  disposition_code = NULL,
                                  arrival_mode_code) {
  # Conservative default
  if (is.na(triage_priority) || triage_priority == 0) {
    return(1)
  }
  
  if (is.na(complexity_code) || complexity_code == 0) {
    complexity_code <- 4
  }
  
  if (is.na(behavioral_health_flag)) {
    behavioral_health_flag <- 0
  }
  
  if (is.na(arrival_mode_code)) {
    arrival_mode_code <- 6
  }
  
  # Arrival mode codes:
  # 1 = self_presented
  # 2 = ground_ambulance
  # 3 = police_custody
  # 4 = hospital_transport
  # 5 = air_transport
  # 6 = other_unknown
  
  # Non-walk-in / transport arrivals -> core ED.
  if (arrival_mode_code %in% c(2, 3, 4, 5)) {
    return(1)
  }
  
  # Behavioral health patients -> core ED
  if (behavioral_health_flag == 1) {
    return(1)
  }
  
  # High-acuity patients -> core ED.
  if (triage_priority <= 2) {
    return(1)
  }
  
  # Critical patients -> core ED.
  if (complexity_code >= 6) {
    return(1)
  }
  
  # Self-presented ESI 5 patients -> RTA.
  if (arrival_mode_code == 1 && triage_priority == 5) {
    return(3)
  }
  
  # Self-presented ESI 4 patients -> vertical/flex.
  if (arrival_mode_code == 1 && triage_priority == 4) {
    return(2)
  }
  
  # Self-presented ESI 3 patients:
  # Low/moderate complexity -> vertical/flex
  if (
    arrival_mode_code == 1 &&
    triage_priority == 3 &&
    complexity_code <= 4
  ) {
    return(2)
  }
  
  # Everything else -> core ED.
  return(1)
}

decode_care_area <- function(care_area_code) {
  dplyr::case_when(
    care_area_code == 1 ~ "core_ed_space",
    care_area_code == 2 ~ "vertical_flex_space",
    care_area_code == 3 ~ "rta_space",
    TRUE ~ "unknown"
  )
}