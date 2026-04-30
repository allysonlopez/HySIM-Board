register_resources <- function(env) {
  
  n_ed_beds <- 17
  #n_providers <- 
  #n_nurses <- 
  #imaging_capacity <- 
  
  env %>%
    add_resource("ed_bed", capacity = n_ed_beds, queue_size = Inf) #%>%
    #add_resource("provider", capacity = n_providers, queue_size = Inf) %>%
    #add_resource("nurse", capacity = n_nurses, queue_size = Inf) %>%
    #add_resource("imaging", capacity = imaging_capacity, queue_size = Inf)
}