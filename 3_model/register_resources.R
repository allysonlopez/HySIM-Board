# 3_model/register_resources.R
# Define capacity constraints for the MVP model.

register_resources <- function(env) {
  env %>%
    add_resource("core_ed_space", capacity = 43, queue_size = Inf) %>%
    add_resource("vertical_flex_space", capacity = 41, queue_size = Inf) %>%
    add_resource("rapid_treatment_space", capacity = 8, queue_size = Inf) %>%
    # Proxy inpatient bed capacity used only for admitted/boarding patients.
    # This should be calibrated once a real inpatient-bed availability table is available.
    add_resource("inpatient_bed", capacity = 30, queue_size = Inf)
}
