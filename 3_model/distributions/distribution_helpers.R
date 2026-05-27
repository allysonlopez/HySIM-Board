estimate_weibull_parameters_from_quantiles <- function(median_min, p90_min) {
  median_min <- as.numeric(median_min)
  p90_min <- as.numeric(p90_min)
  
  invalid_input <- is.na(median_min) ||
    is.na(p90_min) ||
    median_min <= 0 ||
    p90_min <= median_min
  
  if (invalid_input) {
    fallback_scale <- ifelse(is.na(median_min) || median_min <= 0, 1, median_min)
    
    return(list(
      shape = 1.5,
      scale = fallback_scale
    ))
  }
  
  shape <- log(log(10) / log(2)) / log(p90_min / median_min)
  scale <- median_min / (log(2)^(1 / shape))
  
  list(
    shape = shape,
    scale = scale
  )
}

sample_from_weibull <- function(median_min, p90_min, max_allowed_min = Inf) {
  weibull_parameters <- estimate_weibull_parameters_from_quantiles(
    median_min = median_min,
    p90_min = p90_min
  )
  
  sampled_duration <- stats::rweibull(
    n = 1,
    shape = weibull_parameters$shape,
    scale = weibull_parameters$scale
  )
  
  max(1, min(sampled_duration, max_allowed_min))
}

safe_first_row <- function(data) {
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  data[1, ]
}

normalize_probabilities <- function(probabilities) {
  probabilities <- as.numeric(probabilities)
  probabilities[is.na(probabilities)] <- 0
  probabilities[probabilities < 0] <- 0
  
  probability_sum <- sum(probabilities)
  
  if (probability_sum <= 0) {
    return(rep(1 / length(probabilities), length(probabilities)))
  }
  
  probabilities / probability_sum
}