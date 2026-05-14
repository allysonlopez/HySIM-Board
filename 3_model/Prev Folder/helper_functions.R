# 3_model/helper_functions.R
# Purpose:
#   Store small helper functions used by multiple parts of the simulation.

#   Converts the current simulation time, measured in minutes, into:
#     1) day_of_week_num: simulated day of week from 1 to 7
#     2) hour_of_day: hour from 0 to 23
get_day_hour <- function(current_time_min) {
  day_of_week_num <- floor(current_time_min / 1440) %% 7 + 1
  hour_of_day <- floor((current_time_min %% 1440) / 60)
  
  list(day_of_week_num = day_of_week_num, hour_of_day = hour_of_day)
}


#   Finds the rows in a data table that match the current simulated quarter,
#   day of week, and hour of day.
filter_time_block <- function(data, current_time_min, current_quarter) {
  dh <- get_day_hour(current_time_min)
  
  rows <- data %>%
    filter(
      as.numeric(day_of_week_num) == dh$day_of_week_num,
      as.numeric(hour_of_day) == dh$hour_of_day,
      as.numeric(quarter) == as.numeric(current_quarter)
    )
  
  if (nrow(rows) == 0) rows <- data
  
  rows
}


#   Randomly selects one value from a vector, optionally using probabilities.
#   It removes missing values and protects the model from invalid probability
#   vectors.
safe_sample <- function(values, probs = NULL) {
  values <- values[!is.na(values)]
  
  if (length(values) == 0) return(NA)
  
  if (!is.null(probs)) {
    probs <- as.numeric(probs)
    probs[is.na(probs)] <- 0
    if (sum(probs) <= 0) probs <- NULL
  }
  
  sample(values, size = 1, replace = TRUE, prob = probs)
}
