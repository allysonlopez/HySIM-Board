# DES ED Board: Discrete Event Simulation for ED Boarding Reduction

**Team Members:**
Olivia Willard, Allyson Lopez, Xavier Zhou, Andrew Kim

---

## Project Overview

Emergency Department (ED) boarding occurs when patients who have been admitted to the hospital remain in the ED while waiting for an inpatient bed. Boarding can contribute to longer wait times, increased crowding, and strain on hospital resources.

This project develops a **Discrete Event Simulation (DES)** model using the **simmer** package in R to better understand patient flow through the Emergency Department and identify factors that contribute to boarding and delays.

The simulation models:

* Patient arrivals
* Patient acuity and complexity
* Time to first provider
* Diagnostic workup
* Imaging utilization and duration
* Consult utilization and duration
* ED bed resource constraints

The model is driven by historical hospital data and is designed to reproduce key operational patterns observed in the Emergency Department.

---

## Project Structure

### `1_data/`

Contains all simulation input datasets.

| File                                                                         | Description                                      |
| ---------------------------------------------------------------------------- | ------------------------------------------------ |
| `01_interarrival_by_timeblock_hourly_cy2025.csv`                             | Patient arrival patterns by hour and day         |
| `02_case_mix_by_timeblock_cy2025.csv`                                        | Patient acuity and complexity distributions      |
| `03_arrival_to_first_seen_distribution_by_triage_cy2025.csv`                 | Summary statistics for first-provider wait times |
| `04_arrival_to_first_seen_empirical_deid_cy2025.csv`                         | Empirical first-provider wait times              |
| `05_generic_workup_duration_distribution_by_complexity_cy2025.csv`           | Summary workup durations                         |
| `06_generic_workup_duration_empirical_deid_cy2025.csv`                       | Empirical workup durations                       |
| `07_imaging_probability_and_modality_mix_by_acuity_historical_2018_2022.csv` | Imaging probabilities and modality mix           |
| `08_imaging_duration_distribution_by_modality_historical_2018_2022.csv`      | Imaging duration distributions                   |
| `09_consult_probability_and_group_mix_by_acuity_historical_2018_2022.csv`    | Consult probabilities by acuity                  |

---

### `2_prep/`

Contains functions used to load packages and simulation input data.

| File              | Description                     |
| ----------------- | ------------------------------- |
| `load_packages.R` | Loads required R packages       |
| `load_data.R`     | Loads simulation input datasets |

---

### `3_model/`

Contains the simulation model and supporting functions.

| File                      | Description                                      |
| ------------------------- | ------------------------------------------------ |
| `helper_functions.R`      | General helper functions                         |
| `generate_arrivals.R`     | Generates patient arrival times                  |
| `sample_patient_inputs.R` | Samples patient attributes and process durations |
| `register_resources.R`    | Defines ED resources                             |
| `patient_trajectory.R`    | Defines patient flow through the simulation      |
| `run_simulation.R`        | Main script used to run the simulation           |

---

### `4_analysis/`

Contains functions used to summarize and visualize simulation outputs.

| File                  | Description                                      |
| --------------------- | ------------------------------------------------ |
| `summarize_results.R` | Creates summary statistics and validation tables |
| `visualize_results.R` | Creates validation plots                         |

---

### `5_outputs/`

Contains output files generated after running the simulation.

Example outputs include:

* `validation_summary.csv`
* `mape_summary.csv`
* `triage_validation.csv`
* `complexity_validation.csv`
* `validation_summary_plot.png`
* `triage_validation_plot.png`
* `complexity_validation_plot.png`

---

## Requirements

This project was developed in R using:

* simmer
* dplyr
* tidyr
* ggplot2
* readr
* purrr

All required packages will be loaded automatically through:

```r
source("2_prep/load_packages.R")
```

---

## How to Run the Simulation

### Step 1

Open the project `.Rproj` file in RStudio.

### Step 2

Ensure all input datasets are located in the `1_data/` folder.

### Step 3

Run the simulation:

```r
source("3_model/run_simulation.R")
```

### Step 4

The simulation will:

* Generate patient arrivals
* Run the ED simulation
* Calculate summary statistics
* Create validation tables
* Generate validation plots
* Save outputs to the `5_outputs/` folder

Results will also be printed to the R console.

---

## Model Validation

Model performance was evaluated by comparing simulated outputs to historical hospital data.

Validation metrics include:

* Patients per Day
* First Provider Wait Time
* Workup Duration
* Imaging Duration
* Imaging Utilization
* Consult Utilization

Overall model accuracy is summarized using **Mean Absolute Percentage Error (MAPE)**.

The final model achieved an overall operational validation MAPE of approximately **6.6%**, indicating strong agreement between simulated and historical performance across the primary workflow metrics.

---

## Limitations

This project represents a Minimum Viable Product (MVP) simulation framework designed to reproduce major Emergency Department workflow processes.

While the model accurately reproduces patient arrivals, acuity distributions, complexity distributions, and key operational metrics, bed management logic is intentionally simplified and does not yet incorporate hospital-specific inpatient bed allocation policies. As a result, bed wait times and overall length of stay should be interpreted cautiously.

---

## Acknowledgements

This project was completed as part of the UCI Data Science Capstone course in collaboration with Dr. Graham Stephenson and Luis Gonzalez. The simulation framework was developed to support future research into Emergency Department operations, boarding reduction, and patient flow optimization.
