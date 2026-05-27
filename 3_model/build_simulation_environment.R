build_simulation_environment <- function(config) {
  simmer::simmer("HySIM_ED", log_level = 0) %>%
    add_ed_resources(config = config)
}
