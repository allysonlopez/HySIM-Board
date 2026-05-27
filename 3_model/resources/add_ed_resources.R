add_ed_resources <- function(env, config) {
  env %>%
    simmer::add_resource("triage_rn", capacity = config$triage_rns, queue_size = Inf) %>%
    simmer::add_resource("provider", capacity = config$providers, queue_size = Inf) %>%
    simmer::add_resource("core_ed_space", capacity = config$core_ed_spaces, queue_size = Inf) %>%
    simmer::add_resource("vertical_flex_space", capacity = config$vertical_flex_spaces, queue_size = Inf) %>%
    simmer::add_resource("rta_space", capacity = config$rta_spaces, queue_size = Inf) %>%
    simmer::add_resource("imaging_resource", capacity = config$imaging_resources, queue_size = Inf)
}
