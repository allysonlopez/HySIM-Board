# HySIM ED LOS DES – Initial Derived Input Package

## What this is

This folder contains the first batch of de-identified, simulation-ready input files for the HySIM Emergency Department Length of Stay discrete-event simulation (DES) project.

These files are meant to support the **minimum viable model (MVP)** described in the project write-up. In that MVP, the goal is to build a simplified ED LOS simulation from arrival to ED care complete, with a small, explicit pathway and modular inputs rather than one giant all-in-one script.

This package is intended to give you enough to start wiring up the model while the remaining inputs are still being pulled.

## What this batch covers

This batch gives you a workable first pass on the following parts of the model:

- **Arrival cadence** — when patients appear in the model
- **Case mix** — what kind of patient arrives
- **Front-end delay** — arrival to first practitioner contact
- **Generic workup** — a first-pass proxy for the broad evaluation period
- **Imaging use and modality mix** — whether imaging happens and what type
- **Imaging timing** — acquisition, interpretation, and total imaging timing by modality

This is not yet the full data package. Additional tables are still pending.

## Privacy / de-identification

These files were prepared with a privacy-first approach.

### Summary files
Most files in this package are **aggregated summary tables**. These do **not** contain direct identifiers or encounter-level raw timestamps.

### Empirical files
The empirical files are **de-identified**. They may contain:

- `encounter_token`
- derived attributes needed for modeling
- `duration_min`

They do **not** contain:

- CSN
- MRN
- names
- DOB
- real event timestamps

If event ordering ever becomes necessary in a future extract, shifted timestamps can be generated internally, but they are **not included in this batch**.

## How to use this package

Use the files in the order below:

1. **Build the arrival generator**
   - Start with the interarrival table.
   - This controls **when** entities enter the model.

2. **Assign patient attributes**
   - Use the case-mix table after the arrival generator.
   - This controls **who** the arriving patient is.

3. **Parameterize front-end delay**
   - Use the arrival-to-first-seen summary table, or the empirical version if you want to fit or sample directly.

4. **Parameterize generic workup**
   - Use the generic workup duration table as the MVP stand-in for the broader clinical evaluation period.

5. **Add imaging logic**
   - Use the imaging probability/modality table to decide whether imaging occurs and what modality is assigned.
   - Use the imaging duration table to parameterize how long the imaging subprocess takes.

## Files included

### 01_interarrival_by_timeblock_hourly_cy2025.csv
**Purpose:** Hourly arrival cadence by year, quarter, day of week, and hour.

**Use in the sim:**  
Use this to drive entity generation — in other words, **when patients arrive into the ED model**.

**Key columns:**  
- `year`
- `quarter`
- `day_of_week_num`
- `day_of_week_label`
- `hour_of_day`
- `arrivals_n`
- `hours_observed`
- `arrival_rate_per_hour`
- `interarrival_n`
- `mean_interarrival_min`
- `median_interarrival_min`
- `p75_interarrival_min`
- `p90_interarrival_min`
- `p95_interarrival_min`

---

### 02_case_mix_by_timeblock_cy2025.csv
**Purpose:** Long-format case-mix probabilities by time block.

**Use in the sim:**  
Use this after arrival generation to assign patient attributes — in other words, **who arrived**.

**Attributes currently included:**  
- `arrival_mode`
- `acuity`
- `age_group`
- `behavioral_health_flag`
- `complexity_bucket`

**Still pending:**  
- `zone_class`

**Key columns:**  
- `year`
- `quarter`
- `day_of_week_num`
- `hour_of_day`
- `attribute_name`
- `attribute_value`
- `n_obs`
- `probability`

---

### 03_arrival_to_first_seen_distribution_by_triage_cy2025.csv
**Purpose:** Aggregated front-end delay from arrival to first practitioner contact.

**Use in the sim:**  
Use this to parameterize the **arrival-to-first-seen** component of the model.

**Modeling definition used here:**  
“First seen” = first timestamp in the ED Treatment Team table where role is one of:
- ED Resident
- ED Attending
- Nurse Practitioner
- Physician Assistant

**Key columns:**  
- `triage_priority`
- `n_obs`
- `mean_min`
- `median_min`
- `p75_min`
- `p90_min`
- `p95_min`

---

### 04_arrival_to_first_seen_empirical_deid_cy2025.csv
**Purpose:** De-identified encounter-level empirical arrival-to-first-seen durations.

**Use in the sim:**  
Use this if you want to:
- fit a distribution empirically, or
- sample directly from observed durations instead of relying only on the aggregated summary table

**Key columns:**  
- `encounter_token`
- `triage_priority`
- `duration_min`

---

### 05_generic_workup_duration_distribution_by_complexity_cy2025.csv
**Purpose:** Aggregated generic workup duration by complexity bucket.

**Use in the sim:**  
Use this as the MVP stand-in for the broader clinical workup period that is **not yet explicitly broken out** into all downstream subprocesses.

**Modeling definition used here:**  
Generic workup duration =
- **start:** first practitioner contact
- **end:** disposition stand-in

**Disposition stand-in logic used here:**  
- If an admit-type signal exists, use the earliest of:
  - `BedReqDtm`
  - `dtmFirstInpatient`
- Otherwise use:
  - `dtmEdComplete`

**Key columns:**  
- `complexity_bucket`
- `n_obs`
- `mean_min`
- `median_min`
- `p75_min`
- `p90_min`
- `p95_min`

---

### 06_generic_workup_duration_empirical_deid_cy2025.csv
**Purpose:** De-identified encounter-level empirical generic workup durations.

**Use in the sim:**  
Use this if you want empirical fitting or direct sampling instead of relying only on the summary table.

**Key columns:**  
- `encounter_token`
- `complexity_bucket`
- `duration_min`

---

### 07_imaging_probability_and_modality_mix_by_complexity_acuity_cy2025.csv
**Purpose:** Imaging-use probability and primary modality mix by complexity bucket and triage priority.

**Use in the sim:**  
Use this to decide:
- whether imaging happens
- and, if it does, whether the model should route the patient to:
  - XR
  - CT
  - MRI
  - US

**Important modeling note:**  
The source imaging data are order-level and may overlap. For this reason, imaging was sessionized first and then collapsed to one **primary modality per encounter** for case-mix use.

**Key columns:**  
- `complexity_bucket`
- `triage_priority`
- `n_obs`
- `n_imaging_obs`
- `needs_imaging_prob`
- `xr_prob`
- `ct_prob`
- `mri_prob`
- `us_prob`

---

### 08_imaging_duration_distribution_by_modality_cy2025.csv
**Purpose:** Aggregated imaging timing by modality.

**Use in the sim:**  
Use this after imaging is triggered to parameterize how long the imaging subprocess takes by modality.

**Metrics included:**  
- acquisition time
- interpretation time
- total imaging time

**Modalities included:**  
- XR
- CT
- MRI
- US

**Key columns:**  
- `imaging_type`
- `n_obs`
- `acquisition_mean_min`
- `acquisition_median_min`
- `acquisition_p90_min`
- `interpretation_mean_min`
- `interpretation_median_min`
- `interpretation_p90_min`
- `total_imaging_mean_min`
- `total_imaging_median_min`
- `total_imaging_p90_min`

## Modeling assumptions to keep in mind

This package reflects a **practical first-pass MVP build**, not the final fully nuanced model.

A few examples:

- `zone_class` is still missing and will need a reliable ED location / care-area source
- generic workup is currently a broad proxy rather than a perfectly isolated subprocess
- imaging was simplified into a single primary modality per encounter for case-mix use
- the package is currently restricted to **CY2025** to stay aligned with the available complexity source (`CY25_CPT`)

These choices were made intentionally to keep the MVP moving.

## If you want a different version

These extracts were built based on the current DES plan and how I would structure the model from the write-up. If you decide to change the modeling approach and need:

- a different grain
- a different proxy
- different bins or buckets
- a different duration definition
- a different routing logic input

just let me know and I can adjust the pulls.

## Suggested next steps for the student team

1. Load the summary files first and confirm the fields line up with your current simulation objects.
2. Build the baseline arrival generator from the interarrival table.
3. Wire in the case-mix assignment step.
4. Add front-end delay and generic workup durations.
5. Add imaging decision logic and modality-specific timing.
6. Keep notes on anything you want changed in the next data batch so the next pull can be targeted.

## Short version

You should be able to use this package to get a meaningful chunk of the MVP DES off the ground while the remaining data are still being pulled.