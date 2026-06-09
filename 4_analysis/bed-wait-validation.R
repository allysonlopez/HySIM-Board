library(tidyverse)

# =============================================================================
# Optional WR / bed-wait validation diagnostic
# =============================================================================
#
# Purpose:
#   Compare historical WR / bed-wait duration against the simulated bed queue.
#
# Interpretation:
#   If the simulated bed-wait value is far outside the historical WR distribution,
#   that suggests the simplified "bed-first" resource assumption is creating
#   queue instability.
#
#   This should be framed as a resource/capacity sensitivity finding, not as a
#   failed model.
#
# Inputs:
#   historical_wr_duration.csv
#     - one column containing WR duration in minutes
#
# Note:
#   Replace sim_bed_wait_mean with the team's actual simulated bed-wait mean if
#   available. The value 9000 is only a rough estimate from the figure sent over.
# =============================================================================


# -----------------------------------------------------------------------------
# Load historical WR duration data
# -----------------------------------------------------------------------------

wr <- read.csv("1_data/bed_wait_validation.csv")

#normalize the column name in case it came out wonky
names(wr)[1] <- "wr_duration"

# -----------------------------------------------------------------------------
# Replace this with the actual simulated bed-wait mean if available.
# 9000 is only a rough estimate from the current figure.
# -----------------------------------------------------------------------------

sim_bed_wait_mean <- 9424


# -----------------------------------------------------------------------------
# Historical summary stats
# -----------------------------------------------------------------------------

wr_summary <- wr %>%
  summarise(
    n = n(),
    mean_min = mean(wr_duration, na.rm = TRUE),
    median_min = median(wr_duration, na.rm = TRUE),
    p75_min = quantile(wr_duration, 0.75, na.rm = TRUE),
    p90_min = quantile(wr_duration, 0.90, na.rm = TRUE),
    p95_min = quantile(wr_duration, 0.95, na.rm = TRUE),
    p99_min = quantile(wr_duration, 0.99, na.rm = TRUE),
    max_min = max(wr_duration, na.rm = TRUE)
  )

print(wr_summary)


# -----------------------------------------------------------------------------
# Plot: historical WR / bed-wait distribution with simulated mean annotation
# -----------------------------------------------------------------------------
# The x-axis is capped at 900 minutes for readability.
# If the simulated mean is much larger than that, it will be annotated rather
# than drawn as a vertical line.
# -----------------------------------------------------------------------------

ggplot(wr, aes(x = wr_duration)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 30,
    boundary = 0,
    fill = "grey80",
    color = "white"
  ) +
  geom_density(linewidth = 1) +
  geom_vline(
    xintercept = wr_summary$median_min,
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = wr_summary$p90_min,
    linetype = "dotted",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = wr_summary$median_min,
    y = Inf,
    label = "Historical median",
    vjust = 3.0,
    hjust = -0.1,
    size = 4
  ) +
  annotate(
    "text",
    x = wr_summary$p90_min,
    y = Inf,
    label = "Historical p90",
    vjust = 4.5,
    hjust = -0.1,
    size = 4
  ) +
  annotate(
    "text",
    x = 850,
    y = Inf,
    label = paste0(
      "Simulated mean bed wait ~ ",
      round(sim_bed_wait_mean),
      " min\n(outside plot range)"
    ),
    vjust = 1.5,
    hjust = 1,
    size = 4
  ) +
  coord_cartesian(xlim = c(0, 900)) +
  labs(
    title = "Historical WR / Bed-Wait Distribution",
    subtitle = "Simulated bed-wait mean is outside the plotted historical range",
    x = "WR / bed-wait time (minutes)",
    y = "Density",
    caption = "Dashed line = historical median. Dotted line = historical p90. Simulated mean should be replaced with actual model output if available."
  ) +
  theme_minimal(base_size = 14)

# =============================================================================
# Optional: stable-range comparison using full simulated bed-wait values
# =============================================================================
#
# Use this only if you have patient-level simulated bed-wait values.
#
# Why trim at historical p90?
#   The goal is not to hide the queue blow-up... we just want to separate:
#
#   1) how the simulation behaves while it is still inside a historically
#      plausible WR / bed-wait range; and
#
#   2) how often the simulation enters an overloaded queue state.
#
# Interpretation:
#   - The trimmed mean/median describes the stable-range simulated queue.
#   - The percent above historical p90 is the queue-instability signal.
# =============================================================================

# sim_results <- read.csv("your_simulation_output.csv")
# Replace bed_wait_min with the actual simulated bed-wait / queue-time column.

# hist_p90 <- as.numeric(wr_summary$p90_min)
#
# sim_queue_summary <- sim_results %>%
#   summarise(
#     n_sim = n(),
#     raw_mean_bed_wait_min = mean(bed_wait_min, na.rm = TRUE),
#     raw_median_bed_wait_min = median(bed_wait_min, na.rm = TRUE),
#     hist_p90_cutoff_min = hist_p90,
#     pct_above_hist_p90 = mean(bed_wait_min > hist_p90, na.rm = TRUE),
#     mean_bed_wait_trimmed_to_hist_p90 = mean(
#       bed_wait_min[bed_wait_min <= hist_p90],
#       na.rm = TRUE
#     ),
#     median_bed_wait_trimmed_to_hist_p90 = median(
#       bed_wait_min[bed_wait_min <= hist_p90],
#       na.rm = TRUE
#     ),
#     n_remaining_after_trim = sum(bed_wait_min <= hist_p90, na.rm = TRUE)
#   )
#
# print(sim_queue_summary)