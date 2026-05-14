# Defines major ED resources for the MVP model.
# These are grouped capacities, not individual rooms.

ed_resources <- tibble(
  resource_group = c(
    "core_ed_spaces",
    "triage_rn"
  ),
  capacity = c(
    43,
    2
  ),
  modeling_use = c(
    "Core ED treatment and resuscitation spaces",
    "Front-end triage nursing capacity"
  )
)

ed_resources