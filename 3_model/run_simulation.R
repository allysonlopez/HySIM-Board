# 3_model/run_simulation.R

source("2_prep/load_packages.R")
source("2_prep/load_data.R")

source("3_model/helper_functions.R")
source("3_model/generate_arrivals.R")
source("3_model/sample_patient_inputs.R")
source("3_model/register_resources.R")
source("3_model/patient_trajectory.R")

source("4_analysis/summarize_results.R")
source("4_analysis/visualize_results.R")
set.seed(123)

current_quarter <- 2
first_seen_scale <- 0.35

warmup_days <- 1
analysis_days <- 21

total_days <- warmup_days + analysis_days
warmup_time <- warmup_days * 24 * 60
analysis_end_time <- total_days * 24 * 60


# ----------------------------
# Build simulation environment
# ----------------------------

env <- simmer("ED") %>%
  register_resources()


# ----------------------------
# Generate arrivals
# ----------------------------

arrival_times <- create_arrival_times(
  interarrival_data = interarrival_data,
  current_quarter = current_quarter,
  sim_days = total_days
)

arrival_distribution <- make_interarrival_function(arrival_times)


# ----------------------------
# Build patient trajectory
# ----------------------------

patient_trajectory <- build_patient_trajectory(
  env = env,
  current_quarter = current_quarter,
  case_mix_data = case_mix_data,
  first_seen_empirical_data = first_seen_empirical_data,
  first_seen_summary_data = first_seen_summary_data,
  consult_probability_data = consult_probability_data,
  workup_empirical_data = workup_empirical_data,
  workup_summary_data = workup_summary_data,
  imaging_probability_data = imaging_probability_data,
  imaging_duration_data = imaging_duration_data,
  first_seen_scale = first_seen_scale
)


# ----------------------------
# Add patient generator
# ----------------------------

env <- env %>%
  add_generator(
    name_prefix = "patient",
    trajectory = patient_trajectory,
    distribution = arrival_distribution,
    mon = 2
  )


# ----------------------------
# Run simulation
# ----------------------------

env <- env %>%
  run(until = analysis_end_time)


# ----------------------------
# Get monitor outputs
# ----------------------------

arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)


# ----------------------------
# Remove warmup period
# ----------------------------

arrivals_analysis <- arrivals %>%
  filter(start_time >= warmup_time)

resources_analysis <- resources %>%
  filter(time >= warmup_time)

attributes_analysis <- attributes %>%
  filter(time >= warmup_time)


# ----------------------------
# Patients per day check
# ----------------------------

patients_per_day <- arrivals_analysis %>%
  mutate(day = floor((start_time - warmup_time) / 1440) + 1) %>%
  filter(day >= 1, day <= analysis_days) %>%
  count(day, name = "n_patients")

patients_per_day_summary <- patients_per_day %>%
  summarise(
    mean_patients_per_day = mean(n_patients),
    min_patients_per_day = min(n_patients),
    max_patients_per_day = max(n_patients)
  )


# ----------------------------
# Overall LOS and resource summaries
# ----------------------------

los_metrics <- calculate_los_metrics(arrivals_analysis)

resource_summary <- summarize_resources(resources_analysis)

acuity_summary <- summarize_attributes(attributes_analysis, "acuity")

complexity_summary <- summarize_attributes(attributes_analysis, "complexity_bucket")


# ----------------------------
# Bed wait / queue bottleneck
# ----------------------------

bed_wait_summary <- arrivals_analysis %>%
  mutate(
    total_los_min = end_time - start_time,
    process_time_min = activity_time,
    bed_wait_time_min = total_los_min - process_time_min
  ) %>%
  summarise(
    mean_bed_wait_min = mean(bed_wait_time_min, na.rm = TRUE),
    median_bed_wait_min = median(bed_wait_time_min, na.rm = TRUE),
    p75_bed_wait_min = quantile(bed_wait_time_min, 0.75, na.rm = TRUE),
    p90_bed_wait_min = quantile(bed_wait_time_min, 0.90, na.rm = TRUE),
    max_bed_wait_min = max(bed_wait_time_min, na.rm = TRUE)
  )


# ----------------------------
# Process-level summaries
# ----------------------------

process_summary <- attributes_analysis %>%
  filter(key %in% c(
    "first_seen_duration",
    "consult_duration",
    "workup_duration",
    "imaging_duration"
  )) %>%
  group_by(key) %>%
  summarise(
    n = n(),
    mean_min = mean(value, na.rm = TRUE),
    median_min = median(value, na.rm = TRUE),
    p75_min = quantile(value, 0.75, na.rm = TRUE),
    p90_min = quantile(value, 0.90, na.rm = TRUE),
    max_min = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_min))


consult_summary <- attributes_analysis %>%
  filter(key == "consult_duration") %>%
  summarise(
    total_patients = n(),
    n_consults = sum(value > 0, na.rm = TRUE),
    percent_consult = mean(value > 0, na.rm = TRUE),
    mean_consult_duration_min = mean(value[value > 0], na.rm = TRUE)
  )


imaging_summary <- attributes_analysis %>%
  filter(key == "imaging_duration") %>%
  summarise(
    total_patients = n(),
    n_imaging = sum(value > 0, na.rm = TRUE),
    percent_imaging = mean(value > 0, na.rm = TRUE),
    mean_imaging_duration_min = mean(value[value > 0], na.rm = TRUE)
  )


observed_process_baseline <- calculate_observed_process_baseline(
  first_seen_empirical_data,
  workup_empirical_data
)



# ----------------------------
# Historical vs Simulated Validation Tables
# ----------------------------

dir.create("5_outputs", showWarnings = FALSE)

# ----------------------------
# 1. Main validation metrics
# ----------------------------

# Historical patients per day
# arrivals_n is counted across observed hours, so:
# total arrivals / total observed days
historical_patients_per_day <- interarrival_data %>%
  filter(quarter == current_quarter) %>%
  summarise(
    historical_value = sum(arrivals_n, na.rm = TRUE) /
      (sum(hours_observed, na.rm = TRUE) / 24)
  ) %>%
  pull(historical_value)

# Historical first provider wait
historical_first_provider_wait <- first_seen_empirical_data %>%
  filter(triage_priority != "UNKNOWN") %>%
  summarise(
    historical_value = mean(duration_min, na.rm = TRUE)
  ) %>%
  pull(historical_value)

# Historical workup duration
historical_workup_duration <- workup_empirical_data %>%
  filter(complexity_bucket != "UNKNOWN") %>%
  summarise(
    historical_value = mean(duration_min, na.rm = TRUE)
  ) %>%
  pull(historical_value)

# Historical imaging duration, weighted by modality counts
historical_imaging_duration <- imaging_duration_data %>%
  summarise(
    historical_value = weighted.mean(
      total_imaging_mean_min,
      n_obs,
      na.rm = TRUE
    )
  ) %>%
  pull(historical_value)

# Historical imaging utilization, weighted by n_obs
historical_imaging_utilization <- imaging_probability_data %>%
  filter(triage_priority != "UNKNOWN") %>%
  summarise(
    historical_value = weighted.mean(
      needs_imaging_prob,
      n_obs,
      na.rm = TRUE
    ) * 100
  ) %>%
  pull(historical_value)

# Historical consult utilization, weighted by n_obs
historical_consult_utilization <- consult_probability_data %>%
  filter(triage_priority != "UNKNOWN") %>%
  summarise(
    historical_value = weighted.mean(
      needs_consult_prob,
      n_obs,
      na.rm = TRUE
    ) * 100
  ) %>%
  pull(historical_value)

# Simulated values
sim_first_provider_wait <- process_summary %>%
  filter(key == "first_seen_duration") %>%
  pull(mean_min)

sim_workup_duration <- process_summary %>%
  filter(key == "workup_duration") %>%
  pull(mean_min)

sim_imaging_duration <- imaging_summary$mean_imaging_duration_min

sim_imaging_utilization <- imaging_summary$percent_imaging * 100

sim_consult_utilization <- consult_summary$percent_consult * 100

validation_summary <- tibble(
  metric = c(
    "Patients per Day (patients/day)",
    "Mean First Provider Wait (min)",
    "Mean Workup Duration (min)",
    "Mean Imaging Duration (min)",
    "Imaging Utilization (%)",
    "Consult Utilization (%)"
  ),
  unit = c(
    "patients/day",
    "minutes",
    "minutes",
    "minutes",
    "percent",
    "percent"
  ),
  historical = c(
    historical_patients_per_day,
    historical_first_provider_wait,
    historical_workup_duration,
    historical_imaging_duration,
    historical_imaging_utilization,
    historical_consult_utilization
  ),
  simulated = c(
    patients_per_day_summary$mean_patients_per_day,
    sim_first_provider_wait,
    sim_workup_duration,
    sim_imaging_duration,
    sim_imaging_utilization,
    sim_consult_utilization
  )
) %>%
  mutate(
    historical = round(historical, 2),
    simulated = round(simulated, 2),
    percent_difference = round(
      100 * (simulated - historical) / historical,
      1
    )
  )

write_csv(
  validation_summary,
  "5_outputs/validation_summary.csv"
)

print(validation_summary)


# ----------------------------
# 2. Historical vs simulated triage mix
# ----------------------------

historical_triage <- case_mix_data %>%
  filter(
    quarter == current_quarter,
    attribute_name == "acuity",
    attribute_value != "UNKNOWN"
  ) %>%
  group_by(attribute_value) %>%
  summarise(
    historical = sum(n_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    historical_percent = 100 * historical / sum(historical)
  ) %>%
  rename(triage_priority = attribute_value)

simulated_triage <- attributes_analysis %>%
  filter(key == "acuity") %>%
  mutate(
    triage_priority = as.character(value)
  ) %>%
  filter(triage_priority != "UNKNOWN") %>%
  count(triage_priority, name = "simulated") %>%
  mutate(
    simulated_percent = 100 * simulated / sum(simulated)
  )

triage_validation <- historical_triage %>%
  full_join(
    simulated_triage,
    by = "triage_priority"
  ) %>%
  mutate(
    historical_percent = replace_na(historical_percent, 0),
    simulated_percent = replace_na(simulated_percent, 0),
    difference = simulated_percent - historical_percent
  ) %>%
  arrange(triage_priority)

write_csv(
  triage_validation,
  "5_outputs/triage_validation.csv"
)

print(triage_validation)


# ----------------------------
# 3. Historical vs simulated complexity mix
# ----------------------------

historical_complexity <- case_mix_data %>%
  filter(
    quarter == current_quarter,
    attribute_name == "complexity_bucket",
    attribute_value != "UNKNOWN"
  ) %>%
  group_by(attribute_value) %>%
  summarise(
    historical = sum(n_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    historical_percent = 100 * historical / sum(historical)
  ) %>%
  rename(complexity_bucket = attribute_value)

simulated_complexity <- attributes_analysis %>%
  filter(key == "complexity_bucket") %>%
  mutate(
    complexity_bucket = as.character(value)
  ) %>%
  filter(complexity_bucket != "UNKNOWN") %>%
  count(complexity_bucket, name = "simulated") %>%
  mutate(
    simulated_percent = 100 * simulated / sum(simulated)
  )

complexity_validation <- historical_complexity %>%
  full_join(
    simulated_complexity,
    by = "complexity_bucket"
  ) %>%
  mutate(
    historical_percent = replace_na(historical_percent, 0),
    simulated_percent = replace_na(simulated_percent, 0),
    difference = simulated_percent - historical_percent
  ) %>%
  arrange(complexity_bucket)

write_csv(
  complexity_validation,
  "5_outputs/complexity_validation.csv"
)

print(complexity_validation)


# ----------------------------
# Validation Visualizations
# ----------------------------
# ----------------------------
# Validation Visualizations - Blue Theme
# ----------------------------

main_blue <- "#1F4E79"
light_blue <- "#9ECAE1"
dark_blue <- "#08306B"

# Convert complexity numbers back to text labels if needed
complexity_validation <- complexity_validation %>%
  mutate(
    complexity_bucket = case_when(
      complexity_bucket == "1" ~ "minimal",
      complexity_bucket == "2" ~ "straightforward",
      complexity_bucket == "3" ~ "low",
      complexity_bucket == "4" ~ "moderate",
      complexity_bucket == "5" ~ "high",
      complexity_bucket == "6" ~ "critical_care",
      TRUE ~ complexity_bucket
    )
  )

validation_plot <- validation_summary %>%
  select(metric, historical, simulated) %>%
  pivot_longer(
    cols = c(historical, simulated),
    names_to = "source",
    values_to = "value"
  ) %>%
  mutate(
    source = recode(
      source,
      historical = "Historical",
      simulated = "Simulated"
    )
  ) %>%
  ggplot(aes(x = metric, y = value, fill = source)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Historical" = light_blue,
    "Simulated" = main_blue
  )) +
  labs(
    title = "Operational Validation Summary: Historical vs Simulated Metrics",
    x = NULL,
    y = "Operational Metric Value",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "white"
    ),
    panel.background = element_rect(
      fill = "white",
      color = "white"
    ),
    legend.background = element_rect(
      fill = "white"
    ),
    legend.box.background = element_rect(
      fill = "white"
    )
  )

ggsave(
  "5_outputs/validation_summary_plot.png",
  validation_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
  
)


triage_plot <- triage_validation %>%
  select(triage_priority, historical_percent, simulated_percent) %>%
  pivot_longer(
    cols = c(historical_percent, simulated_percent),
    names_to = "source",
    values_to = "percent"
  ) %>%
  mutate(
    source = recode(
      source,
      historical_percent = "Historical",
      simulated_percent = "Simulated"
    )
  ) %>%
  ggplot(aes(x = factor(triage_priority), y = percent, fill = source)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(
    "Historical" = light_blue,
    "Simulated" = main_blue
  )) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,20)
  )+
  labs(
    title = "Model Validation: Historical vs Simulated Triage Distribution",
    x = "Triage Level",
    y = "Percent of Patients",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "white"
    ),
    panel.background = element_rect(
      fill = "white",
      color = "white"
    ),
    legend.background = element_rect(
      fill = "white"
    ),
    legend.box.background = element_rect(
      fill = "white"
    )
  )

ggsave(
  "5_outputs/triage_validation_plot.png",
  triage_plot,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
  
)


complexity_plot <- complexity_validation %>%
  select(complexity_bucket, historical_percent, simulated_percent) %>%
  pivot_longer(
    cols = c(historical_percent, simulated_percent),
    names_to = "source",
    values_to = "percent"
  ) %>%
  mutate(
    source = recode(
      source,
      historical_percent = "Historical",
      simulated_percent = "Simulated"
    ),
    complexity_bucket = factor(
      complexity_bucket,
      levels = c(
        "minimal",
        "straightforward",
        "low",
        "moderate",
        "high",
        "critical_care"
      )
    )
  ) %>%
  ggplot(aes(x = complexity_bucket, y = percent, fill = source)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Historical" = light_blue,
    "Simulated" = main_blue
  )) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = seq(0,100,20)
  )+
  labs(
    title = "Model Validation: Historical vs Simulated Complexity Distribution",
    x = NULL,
    y = "Percent of Patients",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "white"
    ),
    panel.background = element_rect(
      fill = "white",
      color = "white"
    ),
    legend.background = element_rect(
      fill = "white"
    ),
    legend.box.background = element_rect(
      fill = "white"
    )
  )

ggsave(
  "5_outputs/complexity_validation_plot.png",
  complexity_plot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
  
)

# ----------------------------
# Simulated Wait Time Decomposition
# Shows bed wait as an MVP limitation
# ----------------------------

wait_time_components <- arrivals_analysis %>%
  mutate(
    total_los_min = end_time - start_time,
    active_process_time_min = activity_time,
    bed_wait_time_min = total_los_min - active_process_time_min
  ) %>%
  summarise(
    `Bed Wait / Queue Time` = mean(bed_wait_time_min, na.rm = TRUE),
    `Active Modeled Process Time` = mean(active_process_time_min, na.rm = TRUE),
    `Total Simulated LOS` = mean(total_los_min, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "component",
    values_to = "mean_minutes"
  )

wait_time_plot <- wait_time_components %>%
  mutate(
    component = factor(
      component,
      levels = c(
        "Total Simulated LOS",
        "Active Modeled Process Time",
        "Bed Wait / Queue Time"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = component,
      y = mean_minutes
    )
  ) +
  geom_col(
    fill = main_blue,
    width = 0.65
  ) +
  coord_flip() +
  labs(
    title = "Simulated Waiting Time Under MVP Capacity Assumptions",
    subtitle = "Bed wait reflects queueing from the 43-bed resource constraint and is not historically validated in the current data package",
    x = NULL,
    y = "Mean Time (Minutes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "white"
    ),
    panel.background = element_rect(
      fill = "white",
      color = "white"
    ),
    plot.title = element_text(
      face = "bold",
      color = dark_blue
    )
  )

ggsave(
  "5_outputs/wait_time_decomposition_plot.png",
  wait_time_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ----------------------------
# LOS Decomposition by Daily Patient Volume
# ----------------------------

los_decomposition_by_volume <- arrivals_analysis %>%
  mutate(
    day = floor((start_time - warmup_time) / 1440) + 1,
    total_los_min = end_time - start_time,
    active_process_time_min = activity_time,
    bed_wait_time_min = total_los_min - active_process_time_min
  ) %>%
  left_join(
    patients_per_day,
    by = "day"
  ) %>%
  mutate(
    patient_volume_group = case_when(
      n_patients < 170 ~ "Low volume\n(<170 patients/day)",
      n_patients >= 170 & n_patients <= 190 ~ "Expected volume\n(170-190 patients/day)",
      n_patients > 190 ~ "High volume\n(>190 patients/day)"
    )
  ) %>%
  group_by(patient_volume_group) %>%
  summarise(
    `Bed Wait / Queue Time` = mean(bed_wait_time_min, na.rm = TRUE),
    `Active Modeled Process Time` = mean(active_process_time_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      `Bed Wait / Queue Time`,
      `Active Modeled Process Time`
    ),
    names_to = "component",
    values_to = "mean_minutes"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c(
        "Active Modeled Process Time",
        "Bed Wait / Queue Time"
      )
    ),
    patient_volume_group = factor(
      patient_volume_group,
      levels = c(
        "Low volume\n(<170 patients/day)",
        "Expected volume\n(170-190 patients/day)",
        "High volume\n(>190 patients/day)"
      )
    )
  )

los_volume_plot <- los_decomposition_by_volume %>%
  ggplot(
    aes(
      x = patient_volume_group,
      y = mean_minutes,
      fill = component
    )
  ) +
  geom_col(
    width = 0.65
  ) +
  scale_fill_manual(
    values = c(
      "Active Modeled Process Time" = light_blue,
      "Bed Wait / Queue Time" = main_blue
    )
  ) +
  labs(
    title = "Simulated LOS by Daily Patient Volume",
    subtitle = "Stacked bars show how much of LOS comes from bed wait under the 43-bed MVP capacity constraint",
    x = NULL,
    y = "Mean Simulated Time (Minutes)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = "white"
    ),
    panel.background = element_rect(
      fill = "white",
      color = "white"
    ),
    legend.background = element_rect(
      fill = "white"
    ),
    legend.box.background = element_rect(
      fill = "white"
    ),
    plot.title = element_text(
      face = "bold",
      color = dark_blue
    ),
    legend.position = "bottom"
  )

ggsave(
  "5_outputs/los_decomposition_by_patient_volume.png",
  los_volume_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

los_breakdown <- tibble(
  component = c(
    "Bed Wait / Queue Time",
    "Modeled Care Processes"
  ),
  
  minutes = c(
    bed_wait_summary$mean_bed_wait_min,
    los_metrics$mean_los_minute -
      bed_wait_summary$mean_bed_wait_min
  )
)

los_breakdown_plot <- ggplot(
  los_breakdown,
  aes(
    x = "",
    y = minutes,
    fill = component
  )
)+
  
  geom_col(
    width=.5
  )+
  
  coord_flip()+
  
  scale_fill_manual(
    values=c(
      "Bed Wait / Queue Time"=main_blue,
      "Modeled Care Processes"=light_blue
    )
  )+
  
  geom_text(
    aes(
      label=
        paste0(
          round(
            100*
              minutes/
              sum(minutes),
            0
          ),
          "%"
        )
    ),
    position=
      position_stack(
        vjust=.5
      ),
    color="white",
    fontface="bold",
    size=5
  )+
  
  labs(
    title=
      "Average LOS Composition Under MVP Capacity Assumptions",
    
    subtitle=
      "Most simulated LOS is driven by bed waiting generated by the 43-bed MVP constraint",
    
    x=NULL,
    y="Average LOS (Minutes)"
  )+
  
  theme_minimal(
    base_size=14
  )+
  
  theme(
    plot.title=
      element_text(
        face="bold",
        color=dark_blue
      )
  )

# ----------------------------
# Bed Wait vs Patient Volume
# ----------------------------

bed_wait_volume <- arrivals_analysis %>%
  
  mutate(
    
    day =
      floor(
        (start_time - warmup_time)/1440
      ) + 1,
    
    total_los_min =
      end_time - start_time,
    
    bed_wait_min =
      total_los_min -
      activity_time
    
  ) %>%
  
  group_by(day) %>%
  
  summarise(
    
    mean_bed_wait =
      mean(
        bed_wait_min,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  ) %>%
  
  left_join(
    patients_per_day,
    by = "day"
  )

bed_wait_scatter <-
  
  ggplot(
    
    bed_wait_volume,
    
    aes(
      
      x = n_patients,
      
      y = mean_bed_wait
      
    )
    
  )+
  
  geom_point(
    
    size = 4,
    
    color = main_blue,
    
    alpha = .85
    
  )+
  
  geom_smooth(
    
    method = "lm",
    
    se = FALSE,
    
    linewidth = 1.2,
    
    color = dark_blue
    
  )+
  
  labs(
    
    title =
      "Patient Volume and Simulated Bed Waiting",
    
    subtitle =
      "Average daily bed waiting under the 43-bed MVP constraint",
    
    x =
      "Patients Per Day",
    
    y =
      "Average Bed Wait (Minutes)"
    
  )+
  
  theme_minimal(
    base_size = 14
  )+
  
  theme(
    
    plot.background =
      element_rect(
        fill="white",
        color="white"
      ),
    
    panel.background =
      element_rect(
        fill="white",
        color="white"
      ),
    
    plot.title =
      element_text(
        face="bold",
        color=dark_blue
      )
    
  )

ggsave(
  
  "5_outputs/bed_wait_vs_volume.png",
  
  bed_wait_scatter,
  
  width = 8,
  
  height = 6,
  
  dpi = 300,
  
  bg="white"
  
)

# ----------------------------
# LOS Component Distribution
# ----------------------------

los_components <- arrivals_analysis %>%
  
  mutate(
    
    total_los_hr =
      (end_time - start_time)/60,
    
    bed_wait_hr =
      ((end_time - start_time)
       - activity_time)/60,
    
    modeled_process_hr =
      activity_time/60
    
  ) %>%
  
  select(
    total_los_hr,
    bed_wait_hr,
    modeled_process_hr
  ) %>%
  
  pivot_longer(
    
    cols =
      c(
        bed_wait_hr,
        modeled_process_hr
      ),
    
    names_to =
      "component",
    
    values_to =
      "hours"
    
  ) %>%
  
  mutate(
    
    component =
      recode(
        
        component,
        
        bed_wait_hr =
          "Bed Wait / Queue",
        
        modeled_process_hr =
          "Modeled Care Processes"
        
      )
    
  )

los_distribution_plot <-
  
  ggplot(
    
    los_components,
    
    aes(
      
      x = hours,
      
      fill = component
      
    )
    
  )+
  
  geom_histogram(
    
    bins = 40,
    
    alpha = .85,
    
    position = "stack",
    
    color = "black"
    
  )+
  
  scale_fill_manual(
    
    values = c(
      
      "Bed Wait / Queue" =
        main_blue,
      
      "Modeled Care Processes" =
        light_blue
      
    )
    
  )+
  
  labs(
    
    title =
      "Distribution of Simulated Patient Time",
    
    subtitle =
      "Most patient LOS is driven by bed waiting under the MVP 43-bed assumption",
    
    x =
      "Time (Hours)",
    
    y =
      "Patients",
    
    fill =
      NULL
    
  )+
  
  theme_minimal(
    base_size=14
  )+
  
  theme(
    
    plot.title=
      element_text(
        face="bold",
        color=dark_blue
      ),
    
    plot.background=
      element_rect(
        fill="white",
        color="white"
      ),
    
    panel.background=
      element_rect(
        fill="white",
        color="white"
      ),
    
    legend.position=
      "bottom"
    
  )

ggsave(
  
  "5_outputs/los_component_distribution.png",
  
  los_distribution_plot,
  
  width=10,
  
  height=6,
  
  dpi=300,
  
  bg="white"
  
)

validation_plot_data <- validation_summary %>%
  mutate(
    metric_group = case_when(
      metric %in% c(
        "Patients per Day (patients/day)",
        "Mean First Provider Wait (min)"
      ) ~ "Flow Metrics",
      metric %in% c(
        "Mean Workup Duration (min)",
        "Mean Imaging Duration (min)"
      ) ~ "Process Duration Metrics",
      TRUE ~ "Resource Utilization Metrics"
    ),
    metric = case_when(
      metric == "Mean First Provider Wait (min)" ~ "Mean First Provider Wait*\n(min)",
      TRUE ~ metric
    ),
    percent_label = paste0(
      ifelse(percent_difference > 0, "+", ""),
      percent_difference,
      "%"
    )
  )

validation_plot <- validation_plot_data %>%
  select(metric_group, metric, historical, simulated, percent_label) %>%
  pivot_longer(
    cols = c(historical, simulated),
    names_to = "source",
    values_to = "value"
  ) %>%
  mutate(
    source = recode(
      source,
      historical = "Historical",
      simulated = "Simulated"
    ),
    metric = fct_rev(factor(metric))
  ) %>%
  ggplot(aes(x = metric, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    data = validation_plot_data %>%
      mutate(metric = fct_rev(factor(metric))),
    aes(
      x = metric,
      y = pmax(historical, simulated) + 25,
      label = percent_label
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold",
    color = dark_blue
  ) +
  coord_flip() +
  facet_grid(
    metric_group ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_fill_manual(values = c(
    "Historical" = light_blue,
    "Simulated" = main_blue
  )) +
  labs(
    title = "Operational Validation Summary: Historical vs Simulated Performance",
    subtitle = "Summary comparison across metrics with differing units; intended for high-level validation rather than distribution-level agreement.",
    x = NULL,
    y = "Metric Value",
    fill = NULL,
    caption = "*First provider wait is influenced by simplified bed-first workflow assumptions."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", color = dark_blue),
    plot.subtitle = element_text(size = 11),
    strip.text.y = element_text(face = "bold", color = dark_blue),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    legend.background = element_rect(fill = "white"),
    legend.box.background = element_rect(fill = "white")
  )
# ----------------------------
# Print outputs
# ----------------------------

cat("\n--- Patients Per Day by Analysis Day ---\n")
print(patients_per_day)

cat("\n--- Patients Per Day Summary ---\n")
print(patients_per_day_summary)

cat("\n--- Simulation LOS Metrics ---\n")
print(los_metrics)

cat("\n--- Resource Summary: ED Beds Only ---\n")
print(resource_summary)

cat("\n--- Bed Wait / Queue Bottleneck Summary ---\n")
print(bed_wait_summary)

cat("\n--- Acuity Mix ---\n")
print(acuity_summary)

cat("\n--- Complexity Mix ---\n")
print(complexity_summary)

cat("\n--- Flow Step Bottleneck Summary ---\n")
print(process_summary)

cat("\n--- Consult Summary ---\n")
print(consult_summary)

cat("\n--- Imaging Summary ---\n")
print(imaging_summary)

cat("\n--- Observed Baseline from Input Data ---\n")
print(observed_process_baseline)

save_simulation_plots(
  arrivals_analysis = arrivals_analysis,
  attributes_analysis = attributes_analysis,
  patients_per_day = patients_per_day,
  process_summary = process_summary,
  acuity_summary = acuity_summary,
  complexity_summary = complexity_summary
)