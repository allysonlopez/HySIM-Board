# 3_model/register_resources.R
#   Define the ED spaces that patients can occupy during the simulation.

register_resources <- function(env) {
  env %>%
    add_resource("ed_bed", capacity = 43, queue_size = Inf)
}