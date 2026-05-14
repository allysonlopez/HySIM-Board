source("2_prep/load_packages.R")
source("2_prep/load_data.R")
source("3_model/03_sample_patient_attributes.R")

# Test attribute sampling
sample_patient_attributes(
  quarter_current = 1,
  dow_current = 1,
  hour_current = 0
)
unique(case_mix$attribute_name)

case_mix %>%
  filter(
    quarter == 1,
    day_of_week_num == 1,
    hour_of_day == 0
  ) %>%
  count(attribute_name)

source("3_model/04_sample_state_durations.R")

sample_first_seen_duration("2")
sample_workup_duration("critical_care")
sample_workup_duration("high")

source("3_model/02_generate_arrivals.R")

sample_interarrival_time(
  quarter_current = 1,
  dow_current = 1,
  hour_current = 0
)


source("3_model/04_sample_state_durations.R")

sample_imaging_needed("2")
sample_imaging_modality("2")
sample_imaging_duration("CT")


glimpse(consult_prob)
source("3_model/04_sample_state_durations.R")

sample_consult_needed("2")
sample_consult_group("2")

needs_consult <- sample_consult_needed("2")

if (needs_consult == 1) {
  consult_group <- sample_consult_group("2")
} else {
  consult_group <- "none"
}


source("2_prep/load_packages.R")
source("2_prep/load_data.R")

source("3_model/03_sample_patient_attributes.R")
source("3_model/04_sample_state_durations.R")
source("3_model/05_patient_trajectory.R")

simulate_one_patient_path(
  quarter_current = 1,
  dow_current = 1,
  hour_current = 0
)


test_patients <- map_dfr(
  1:100,
  ~ simulate_one_patient_path(
    quarter_current = 1,
    dow_current = 1,
    hour_current = 0
  )
)

glimpse(test_patients)

test_patients %>%
  summarise(
    mean_total_duration = mean(total_duration),
    median_total_duration = median(total_duration),
    imaging_rate = mean(needs_imaging),
    consult_rate = mean(needs_consult)
  )


source("3_model/07_run_simulation.R")

sim_1000 <- run_simple_patient_simulation(
  n_patients = 1000,
  quarter_current = 1,
  dow_current = 1,
  hour_current = 0
)

glimpse(sim_1000)

sim_1000 %>%
  summarise(
    n = n(),
    mean_total_duration = mean(total_duration),
    median_total_duration = median(total_duration),
    imaging_rate = mean(needs_imaging),
    consult_rate = mean(needs_consult)
  )



source("3_model/02_generate_arrivals.R")

arrivals_day <- generate_arrival_schedule(
  sim_minutes = 24 * 60,
  quarter_current = 1,
  dow_current = 1,
  start_hour = 0
)

glimpse(arrivals_day)

arrivals_day %>%
  count(arrival_hour)


source("3_model/07_run_simulation.R")

sim_day <- run_one_day_simulation(
  quarter_current = 1,
  dow_current = 1
)

glimpse(sim_day)

sim_day %>%
  summarise(
    n_patients = n(),
    mean_total_duration = mean(total_duration),
    median_total_duration = median(total_duration),
    imaging_rate = mean(needs_imaging),
    consult_rate = mean(needs_consult)
  )

source("3_model/06_track_system_state.R")

state_day <- track_system_state(
  sim_results = sim_day,
  time_step = 1,
  max_time = 24 * 60
)

glimpse(state_day)

state_day %>%
  filter(time <= 10)

state_day %>%
  filter(time >= 15, time <= 25)

#At each minute, how many patients are waiting, in workup, in imaging, exited, 
#and total in ED.
#Minute 17: 0 patients
#Minute 18: 1 patient waiting to be first seen


source("3_model/01_initialize_system.R")

sim_warm <- run_simulation_with_warmup(
  quarter_current = 1,
  dow_current = 1
)

state_warm <- track_system_state(
  sim_results = sim_warm,
  time_step = 1,
  max_time = 24 * 60
)

state_warm %>%
  filter(time <= 10)

#time 0:
#21 waiting for first seen
#22 in workup
#1 in imaging
#44 total in ED
#The simulation uses a 6-hour warm-up period before the 12AM evaluation window. 
#This prevents the model from starting empty and allows patients to already be in 
#different care states at midnight. At each minute, the model records how many patients 
#are waiting to be seen, in workup, in imaging, exited, and still present in the ED.


source("3_model/06_track_system_state.R")
core_capacity_day <- track_core_room_capacity(
  sim_results = sim_warm,
  time_step = 1,
  max_time = 24 * 60,
  core_capacity = 43
)

core_capacity_day %>%
  summarise(
    mean_occupied = mean(occupied_core_rooms),
    max_occupied = max(occupied_core_rooms),
    minutes_over_capacity = sum(over_capacity),
    percent_time_over_capacity = mean(over_capacity)
  )

source("3_model/07_run_simulation.R")

sim_day_rooms <- run_one_day_simulation_with_rooms(
  quarter_current = 1,
  dow_current = 1,
  core_capacity = 43
)

sim_day_rooms %>%
  summarise(
    n_patients = n(),
    mean_room_wait = mean(room_wait_duration),
    median_room_wait = median(room_wait_duration),
    max_room_wait = max(room_wait_duration),
    percent_waited_for_room = mean(room_wait_duration > 0)
  )

state_rooms <- track_system_state(
  sim_results = sim_day_rooms,
  time_step = 1,
  max_time = 24 * 60
)

core_capacity_rooms <- track_core_room_capacity(
  sim_results = sim_day_rooms,
  time_step = 1,
  max_time = 24 * 60,
  core_capacity = 43
)

core_capacity_rooms %>%
  summarise(
    mean_occupied = mean(occupied_core_rooms),
    max_occupied = max(occupied_core_rooms),
    minutes_over_capacity = sum(over_capacity),
    percent_time_over_capacity = mean(over_capacity)
  )

sim_day_rooms %>%
  summarise(
    percent_waited_for_room = mean(room_wait_duration > 0)
  )