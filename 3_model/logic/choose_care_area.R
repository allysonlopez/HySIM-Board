# Care area codes:
# 1 = core ED treatment/resuscitation space
# 2 = vertical/flex space
# 3 = RTA space
choose_care_area_code <- function(triage_priority, complexity_code, behavioral_health_flag) {
  if (is.na(triage_priority) || triage_priority == 0) {
    return(2)
  }

  if (behavioral_health_flag == 1) {
    return(2)
  }

  if (triage_priority <= 2) {
    return(1)
  }

  if (triage_priority == 3) {
    if (complexity_code >= 4) {
      return(1)
    }
    return(2)
  }

  if (triage_priority >= 4) {
    if (complexity_code <= 2) {
      return(3)
    }
    return(2)
  }

  2
}

decode_care_area <- function(care_area_code) {
  dplyr::case_when(
    care_area_code == 1 ~ "core_ed_space",
    care_area_code == 2 ~ "vertical_flex_space",
    care_area_code == 3 ~ "rta_space",
    TRUE ~ "unknown"
  )
}
