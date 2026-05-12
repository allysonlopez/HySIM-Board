# 3_model/register_resources.R
#   Define the ED spaces that patients can occupy during the simulation.

register_resources <- function(env) {
  env %>%
    add_resource("core_ed_space", capacity = 43, queue_size = Inf) %>%
    add_resource("vertical_flex_space", capacity = 41, queue_size = Inf) %>%
    add_resource("rapid_treatment_space", capacity = 8, queue_size = Inf)
}
