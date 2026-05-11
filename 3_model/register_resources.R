# tells the simulation what resources are limited

register_resources <- function(env) {
  
  n_core_ed_spaces <- 43
  n_triage_rns <- 2
  
  env %>%
    add_resource("triage_rn", capacity = n_triage_rns, queue_size = Inf) %>%
    add_resource("core_ed_space", capacity = n_core_ed_spaces, queue_size = Inf)
}