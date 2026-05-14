2_prep/
  ├── load_packages.R
│   Loads tidyverse, lubridate, simmer, etc.
│
├── load_data.R
│   Reads all CSV input files into R objects.
│
├── clean_inputs.R
│   Reserved for cleaning or renaming columns if needed.
│
└── check_distributions.R
Checks summaries and plots of input distributions.


3_model/
  ├── 01_initialize_system.R
│   Runs the warm-up period so the ED is not empty at 12AM.
│   Main function: run_simulation_with_warmup()
│
├── 02_generate_arrivals.R
│   Creates patient arrival times.
│   Main functions:
  │   - sample_interarrival_time()
│   - generate_arrival_schedule()
│
├── 03_sample_patient_attributes.R
│   Assigns patient characteristics.
│   Main functions:
  │   - sample_attribute()
│   - sample_patient_attributes()
│
├── 04_sample_state_durations.R
│   Samples wait times, workup times, imaging, and consults.
│   Main functions:
  │   - sample_first_seen_duration()
│   - sample_workup_duration()
│   - sample_imaging_needed()
│   - sample_imaging_modality()
│   - sample_imaging_duration()
│   - sample_consult_needed()
│   - sample_consult_group()
│
├── 05_patient_trajectory.R
│   Defines what one patient goes through.
│   Flow:
  │   Arrival → first seen → workup → imaging? → consult? → exit
│   Main function: simulate_one_patient_path()
│
├── 06_track_system_state.R
│   Tracks how many patients are in each state over time.
│   Main functions:
  │   - track_system_state()
│   - track_core_room_capacity()
│
├── 07_run_simulation.R
│   Runs the full day simulation.
│   Main functions:
  │   - run_simple_patient_simulation()
│   - run_one_day_simulation()
│
├── 08_register_resources.R
│   Stores resource assumptions.
│   Example:
  │   - 43 core ED spaces
│   - 2 triage RN resources
│
└── helper_functions.R
Reserved for small helper functions used across files.

4_analysis/
  Good place for plots and summaries.
Example outputs:
  - ED census over time
- patients by state over time
- arrivals by hour
- core room occupancy
