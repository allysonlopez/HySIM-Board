# how long does *imaging* take?

sample_imaging_duration <- function(imaging_duration_data, modality) {
  
  if (modality == "none") {
    return(0)
  }
  
  matching_row <- imaging_duration_data[
    imaging_duration_data$imaging_type == modality,
  ]
  
  if (nrow(matching_row) == 0) {
    return(0)
  }
  
  imaging_duration <- as.numeric(
    matching_row$total_imaging_median_min[1]
  )
  
  if (is.na(imaging_duration)) {
    return(0)
  }
  
  return(max(1, imaging_duration))
}