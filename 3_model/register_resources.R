# 3_model/register_resources.R

# add ED beds as a resource patients must use, fixed 43 bed amount
register_resources <- function(env) {
    env %>%
    add_resource(
      "ed_bed",
      capacity = 43,
      queue_size = Inf
    )
}