#!/usr/bin/env Rscript
# ============================================================================
# PHASE 06 - EVENT MORPHOLOGY ANALYSIS - NOVEMBER 2025
# ============================================================================
# Extract pluvial/drought events: magnitude, duration, recovery
# Compare GRACE events vs model ensemble distributions
# Author: Ashraf Rateb
# Date: 2025-11-19
# ============================================================================

library(data.table)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

cat("============================================================================\n")
cat("PHASE 06: EVENT MORPHOLOGY ANALYSIS\n")
cat("============================================================================\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Event detection thresholds (percentile-based)
UPPER_PROB <- 0.90    # Upper threshold: 90th percentile for pluvials
LOWER_PROB <- 0.10    # Lower threshold: 10th percentile for droughts
NEUTRAL_BAND <- 0.05  # Recovery threshold: return to ±0.05 standardized units
MIN_LENGTH <- 2       # Minimum event duration (months)

cat("Configuration:\n")
cat("  Upper threshold (pluvials):", UPPER_PROB * 100, "th percentile\n")
cat("  Lower threshold (droughts):", LOWER_PROB * 100, "th percentile\n")
cat("  Neutral band (recovery):", NEUTRAL_BAND, "standardized units\n")
cat("  Minimum event length:", MIN_LENGTH, "months\n\n")

# Setup parallel processing
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)
cat("Using", n_cores, "cores for parallel processing\n\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Classify memory timescale into categories
#'
#' @param tau Memory timescale (e-folding time) in months
#' @return character classification
#'
classify_memory_timescale <- function(tau) {
  if (is.na(tau)) return(NA_character_)

  if (tau < 12) {
    "Short (< 1 year)"
  } else if (tau < 24) {
    "Seasonal (1-2 years)"
  } else if (tau < 60) {
    "Interannual (2-5 years)"
  } else if (tau < 120) {
    "Multi-year (5-10 years)"
  } else {
    "Decadal (> 10 years)"
  }
}

#' Compute persistence metrics for positive and negative anomaly regimes
#'
#' @param x Numeric vector of time series values
#' @return list with pluvial and drought regime persistence metrics
#'
compute_regime_persistence <- function(x) {

  # Remove NAs
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)

  # Initialize return structure
  result <- list(
    pluvial_ar1 = NA_real_,
    pluvial_p_lf = NA_real_,
    pluvial_tau = NA_real_,
    pluvial_tau_class = NA_character_,
    pluvial_n_obs = 0L,
    drought_ar1 = NA_real_,
    drought_p_lf = NA_real_,
    drought_tau = NA_real_,
    drought_tau_class = NA_character_,
    drought_n_obs = 0L,
    success = FALSE
  )

  if (n < 24) return(result)  # Need at least 2 years

  # Extract positive anomalies (pluvial regime)
  pluvial_vals <- x_clean[x_clean > 0]
  n_pluvial <- length(pluvial_vals)

  # Extract negative anomalies (drought regime)
  drought_vals <- x_clean[x_clean < 0]
  n_drought <- length(drought_vals)

  # Compute pluvial regime persistence
  if (n_pluvial >= 24) {  # At least 2 years of positive anomalies
    pluvial_ar1 <- tryCatch({
      acf_result <- acf(pluvial_vals, lag.max = 1, plot = FALSE, na.action = na.pass)
      acf_result$acf[2]  # lag-1 autocorrelation
    }, error = function(e) NA_real_)

    pluvial_p_lf <- sd(pluvial_vals, na.rm = TRUE)

    pluvial_tau <- ifelse(!is.na(pluvial_ar1) && pluvial_ar1 > 0 && pluvial_ar1 < 1,
                          -1 / log(pluvial_ar1),
                          NA_real_)

    pluvial_tau_class <- classify_memory_timescale(pluvial_tau)

    result$pluvial_ar1 <- pluvial_ar1
    result$pluvial_p_lf <- pluvial_p_lf
    result$pluvial_tau <- pluvial_tau
    result$pluvial_tau_class <- pluvial_tau_class
    result$pluvial_n_obs <- n_pluvial
  }

  # Compute drought regime persistence
  if (n_drought >= 24) {  # At least 2 years of negative anomalies
    # Use absolute values for drought persistence
    drought_abs <- abs(drought_vals)

    drought_ar1 <- tryCatch({
      acf_result <- acf(drought_abs, lag.max = 1, plot = FALSE, na.action = na.pass)
      acf_result$acf[2]  # lag-1 autocorrelation
    }, error = function(e) NA_real_)

    drought_p_lf <- sd(drought_abs, na.rm = TRUE)

    drought_tau <- ifelse(!is.na(drought_ar1) && drought_ar1 > 0 && drought_ar1 < 1,
                          -1 / log(drought_ar1),
                          NA_real_)

    drought_tau_class <- classify_memory_timescale(drought_tau)

    result$drought_ar1 <- drought_ar1
    result$drought_p_lf <- drought_p_lf
    result$drought_tau <- drought_tau
    result$drought_tau_class <- drought_tau_class
    result$drought_n_obs <- n_drought
  }

  result$success <- !is.na(result$pluvial_ar1) || !is.na(result$drought_ar1)

  return(result)
}

# ============================================================================
# EVENT EXTRACTION FUNCTION
# ============================================================================

#' Extract pluvial and drought events from a time series
#'
#' Uses percentile-based thresholds and formal event morphology metrics:
#' - Pluvial Height (H): max positive TWS anomaly during wet regime
#' - Drought Depth (D): max negative TWS anomaly during dry regime
#' - Duration: consecutive months within regime
#' - Intensity: cumulative absolute anomaly over duration
#' - Recovery: time to return to neutral band after extreme
#'
#' @param x Numeric vector of time series values
#' @param time Date vector (same length as x)
#' @param upper_prob Upper percentile threshold for pluvials (default 0.90)
#' @param lower_prob Lower percentile threshold for droughts (default 0.10)
#' @param neutral_band Neutral band for recovery (default 0.05 standardized units)
#' @param min_length Minimum event duration in months (default 2)
#' @return data.table with one row per event
#'
extract_events <- function(x, time, upper_prob = 0.90, lower_prob = 0.10,
                          neutral_band = 0.05, min_length = 2) {

  # Remove NAs
  valid_idx <- !is.na(x)
  if (sum(valid_idx) < 10) {
    return(data.table(
      type = character(),
      t_start = as.Date(character()),
      t_end = as.Date(character()),
      duration_months = integer(),
      pluvial_height = numeric(),
      drought_depth = numeric(),
      intensity = numeric(),
      recovery_months = numeric(),
      upper_thr = numeric(),
      lower_thr = numeric(),
      neutral_band = numeric()
    ))
  }

  x_clean <- x[valid_idx]
  time_clean <- time[valid_idx]
  n <- length(x_clean)

  # Standardize for neutral band calculation
  x_mean <- mean(x_clean, na.rm = TRUE)
  x_sd <- sd(x_clean, na.rm = TRUE)

  if (is.na(x_sd) || x_sd == 0) {
    return(data.table(
      type = character(),
      t_start = as.Date(character()),
      t_end = as.Date(character()),
      duration_months = integer(),
      pluvial_height = numeric(),
      drought_depth = numeric(),
      intensity = numeric(),
      recovery_months = numeric(),
      upper_thr = numeric(),
      lower_thr = numeric(),
      neutral_band = numeric()
    ))
  }

  x_standardized <- (x_clean - x_mean) / x_sd

  # Define percentile-based thresholds
  upper_thr <- as.numeric(quantile(x_clean, probs = upper_prob, na.rm = TRUE))
  lower_thr <- as.numeric(quantile(x_clean, probs = lower_prob, na.rm = TRUE))

  # Identify pluvial periods (x >= upper_thr)
  pluvial_idx <- which(x_clean >= upper_thr)

  # Identify drought periods (x <= lower_thr)
  drought_idx <- which(x_clean <= lower_thr)

  events_list <- list()

  # Extract pluvial events
  if (length(pluvial_idx) > 0) {
    # Group by breaks in contiguity (robust for all run lengths)
    grp <- cumsum(c(1, diff(pluvial_idx) > 1))
    pluvial_groups <- split(pluvial_idx, grp)

    for (g in seq_along(pluvial_groups)) {
      event_idx <- pluvial_groups[[g]]

      # Skip events shorter than minimum length
      if (length(event_idx) < min_length) next

      t_start <- time_clean[event_idx[1]]
      t_end <- time_clean[event_idx[length(event_idx)]]
      duration_months <- as.integer(length(event_idx))

      # Pluvial Height: maximum positive anomaly during event
      pluvial_height <- max(x_clean[event_idx])

      # Intensity: cumulative absolute anomaly
      intensity <- sum(abs(x_clean[event_idx]))

      # Find recovery time (return to neutral band in standardized units)
      recovery_months <- NA_real_
      if (event_idx[length(event_idx)] < n) {
        post_event_idx <- (event_idx[length(event_idx)] + 1):n
        # Check when |standardized anomaly| <= neutral_band
        recovery_idx <- which(abs(x_standardized[post_event_idx]) <= neutral_band)
        if (length(recovery_idx) > 0) {
          # Calculate months between event end and recovery
          t_recovery <- time_clean[post_event_idx[recovery_idx[1]]]
          recovery_months <- as.numeric(difftime(t_recovery, t_end, units = "days")) / 30.44
        }
      }

      events_list[[length(events_list) + 1]] <- data.table(
        type = "pluvial",
        t_start = t_start,
        t_end = t_end,
        duration_months = duration_months,
        pluvial_height = pluvial_height,
        drought_depth = NA_real_,
        intensity = intensity,
        recovery_months = recovery_months,
        upper_thr = upper_thr,
        lower_thr = lower_thr,
        neutral_band = neutral_band
      )
    }
  }

  # Extract drought events
  if (length(drought_idx) > 0) {
    # Group by breaks in contiguity (robust for all run lengths)
    grp <- cumsum(c(1, diff(drought_idx) > 1))
    drought_groups <- split(drought_idx, grp)

    for (g in seq_along(drought_groups)) {
      event_idx <- drought_groups[[g]]

      # Skip events shorter than minimum length
      if (length(event_idx) < min_length) next

      t_start <- time_clean[event_idx[1]]
      t_end <- time_clean[event_idx[length(event_idx)]]
      duration_months <- as.integer(length(event_idx))

      # Drought Depth: maximum negative anomaly (most negative value)
      drought_depth <- min(x_clean[event_idx])

      # Intensity: cumulative absolute anomaly
      intensity <- sum(abs(x_clean[event_idx]))

      # Find recovery time (return to neutral band in standardized units)
      recovery_months <- NA_real_
      if (event_idx[length(event_idx)] < n) {
        post_event_idx <- (event_idx[length(event_idx)] + 1):n
        # Check when |standardized anomaly| <= neutral_band
        recovery_idx <- which(abs(x_standardized[post_event_idx]) <= neutral_band)
        if (length(recovery_idx) > 0) {
          # Calculate months between event end and recovery
          t_recovery <- time_clean[post_event_idx[recovery_idx[1]]]
          recovery_months <- as.numeric(difftime(t_recovery, t_end, units = "days")) / 30.44
        }
      }

      events_list[[length(events_list) + 1]] <- data.table(
        type = "drought",
        t_start = t_start,
        t_end = t_end,
        duration_months = duration_months,
        pluvial_height = NA_real_,
        drought_depth = drought_depth,
        intensity = intensity,
        recovery_months = recovery_months,
        upper_thr = upper_thr,
        lower_thr = lower_thr,
        neutral_band = neutral_band
      )
    }
  }

  if (length(events_list) == 0) {
    return(data.table(
      type = character(),
      t_start = as.Date(character()),
      t_end = as.Date(character()),
      duration_months = integer(),
      pluvial_height = numeric(),
      drought_depth = numeric(),
      intensity = numeric(),
      recovery_months = numeric(),
      upper_thr = numeric(),
      lower_thr = numeric(),
      neutral_band = numeric()
    ))
  }

  return(rbindlist(events_list))
}

# ============================================================================
# TEST EVENT EXTRACTION WITH SYNTHETIC DATA
# ============================================================================

cat("Testing event extraction with synthetic data...\n")

# Create synthetic test case
set.seed(42)
n_test <- 120
time_test <- seq.Date(as.Date("2000-01-01"), by = "month", length.out = n_test)
x_test <- rnorm(n_test, mean = 0, sd = 1)

# Add a pluvial event (months 20-35)
x_test[20:35] <- x_test[20:35] + 2.5

# Add a drought event (months 60-80)
x_test[60:80] <- x_test[60:80] - 2.0

test_events <- extract_events(x_test, time_test,
                              upper_prob = UPPER_PROB,
                              lower_prob = LOWER_PROB,
                              neutral_band = NEUTRAL_BAND,
                              min_length = MIN_LENGTH)

cat("  Synthetic test results:\n")
cat("    Events found:", nrow(test_events), "\n")
if (nrow(test_events) > 0) {
  cat("    Event types:\n")
  print(table(test_events$type))
  cat("    Sample pluvial event:\n")
  if (any(test_events$type == "pluvial")) {
    print(test_events[type == "pluvial"][1, .(type, t_start, t_end, duration_months, pluvial_height, intensity, recovery_months)])
  }
  cat("    Sample drought event:\n")
  if (any(test_events$type == "drought")) {
    print(test_events[type == "drought"][1, .(type, t_start, t_end, duration_months, drought_depth, intensity, recovery_months)])
  }
}
cat("✓ Event extraction test complete\n\n")

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading data...\n")

# Load FILTERED data from Phase 1
cat("  Loading Processed_Filtered_Nov2025.rds (from Phase 1)...\n")
filtered_data <- readRDS("data/Processed_Filtered_Nov2025.rds")

# Load raw data for GRACE and attrs
cat("  Loading Enhanced_GGFO_MMLEs_Nov2025.rds (for GRACE + attrs)...\n")
raw_data <- readRDS("data/Enhanced_GGFO_MMLEs_Nov2025.rds")

# Extract data
attrs <- raw_data$attrs
gfo_dtrend <- raw_data$gfo_dtrend
dates_grace <- raw_data$dates_grace

G_CESM_filtered <- filtered_data$G_CESM_filtered
dates_cesm <- filtered_data$date_cesm
G_IPSL_combined <- filtered_data$G_IPSL_combined
dates_ipsl <- filtered_data$date_ipsl

cat("  CESM2 filtered dimensions:", dim(G_CESM_filtered), "\n")
cat("  IPSL combined filtered dimensions:", dim(G_IPSL_combined), "\n")
cat("  Basins:", nrow(attrs), "\n")
cat("  GRACE period:", range(dates_grace), "\n\n")

# ============================================================================
# EXTRACT GRACE EVENTS
# ============================================================================

cat("Extracting GRACE events...\n")

GRACE_median <- gfo_dtrend$median  # [time × basins]

grace_events_list <- list()

pb_grace <- txtProgressBar(min = 0, max = nrow(attrs), style = 3)

for (i in 1:nrow(attrs)) {
  basin_id <- attrs$ID[i]
  basin_name <- attrs$name[i]

  # Extract GRACE time series for this basin
  grace_series <- GRACE_median[[basin_name]]

  # Extract events
  events <- extract_events(grace_series, dates_grace,
                          upper_prob = UPPER_PROB,
                          lower_prob = LOWER_PROB,
                          neutral_band = NEUTRAL_BAND,
                          min_length = MIN_LENGTH)

  if (nrow(events) > 0) {
    events[, basin := i]
    events[, basin_id := basin_id]
    events[, basin_name := basin_name]
    grace_events_list[[i]] <- events
  }

  setTxtProgressBar(pb_grace, i)
}

close(pb_grace)

events_grace <- rbindlist(grace_events_list, fill = TRUE)

cat("\n✓ GRACE event extraction complete\n")
cat("  Total events:", nrow(events_grace), "\n")
cat("  Pluvials:", sum(events_grace$type == "pluvial"), "\n")
cat("  Droughts:", sum(events_grace$type == "drought"), "\n\n")

# ============================================================================
# COMPUTE REGIME PERSISTENCE FOR GRACE
# ============================================================================

cat("Computing regime-based persistence (positive/negative anomalies) for GRACE...\n")

grace_regime_persistence_list <- list()

pb_grace_regime <- txtProgressBar(min = 0, max = nrow(attrs), style = 3)

for (i in 1:nrow(attrs)) {
  basin_id <- attrs$ID[i]
  basin_name <- attrs$name[i]

  # Extract GRACE time series for this basin
  grace_series <- GRACE_median[[basin_name]]

  # Compute regime persistence
  regime_metrics <- compute_regime_persistence(grace_series)

  grace_regime_persistence_list[[i]] <- data.table(
    basin = i,
    basin_id = basin_id,
    basin_name = basin_name,
    pluvial_ar1_grace = regime_metrics$pluvial_ar1,
    pluvial_p_lf_grace = regime_metrics$pluvial_p_lf,
    pluvial_tau_grace = regime_metrics$pluvial_tau,
    pluvial_tau_class_grace = regime_metrics$pluvial_tau_class,
    pluvial_n_obs_grace = regime_metrics$pluvial_n_obs,
    drought_ar1_grace = regime_metrics$drought_ar1,
    drought_p_lf_grace = regime_metrics$drought_p_lf,
    drought_tau_grace = regime_metrics$drought_tau,
    drought_tau_class_grace = regime_metrics$drought_tau_class,
    drought_n_obs_grace = regime_metrics$drought_n_obs
  )

  setTxtProgressBar(pb_grace_regime, i)
}

close(pb_grace_regime)

grace_regime_persistence <- rbindlist(grace_regime_persistence_list)

cat("\n✓ GRACE regime persistence complete\n")
cat("  Basins processed:", nrow(grace_regime_persistence), "\n")
cat("  Basins with pluvial regime metrics:", sum(!is.na(grace_regime_persistence$pluvial_ar1_grace)), "\n")
cat("  Basins with drought regime metrics:", sum(!is.na(grace_regime_persistence$drought_ar1_grace)), "\n\n")

# ============================================================================
# EXTRACT MODEL EVENTS (CESM2) WITH 23-YEAR WINDOWING
# ============================================================================

cat("============================================================================\n")
cat("CESM2 EVENT EXTRACTION\n")
cat("============================================================================\n\n")

# Use 273-month windows to match GRACE observation period
WINDOW_LENGTH <- 273  # months (23 years, matching GRACE)

cat("Extracting events from 23-year windows (273 months) to match GRACE...\n")
cat("  80 members × 184 basins × ~1900 windows per member\n")
cat("  Processing basins in parallel...\n\n")

n_basins <- dim(G_CESM_filtered)[1]
n_members_cesm <- dim(G_CESM_filtered)[2]
n_times_cesm <- dim(G_CESM_filtered)[3]

# Export function and parameters to cluster
clusterExport(cl, c("extract_events", "UPPER_PROB", "LOWER_PROB", "NEUTRAL_BAND", "MIN_LENGTH", "WINDOW_LENGTH"))

# Parallel processing across basins
cesm_events_results <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  basin_events <- list()

  # Extract basin data [members × time]
  basin_data <- G_CESM_filtered[basin, , ]

  for (member in 1:n_members_cesm) {
    member_series <- basin_data[member, ]

    # Find valid indices (non-NA)
    valid_idx <- which(!is.na(member_series))

    # Skip if insufficient valid data for even one window
    if (length(valid_idx) < WINDOW_LENGTH) {
      next
    }

    # Determine max number of windows
    max_start <- length(valid_idx) - WINDOW_LENGTH + 1

    # Extract events from all possible 273-month windows
    for (w in 1:max_start) {
      start_idx <- valid_idx[w]
      end_idx <- valid_idx[w + WINDOW_LENGTH - 1]

      # Extract window data and dates
      window_data <- member_series[start_idx:end_idx]
      window_dates <- dates_cesm[start_idx:end_idx]

      # Extract events from this window
      events <- extract_events(window_data, window_dates,
                              upper_prob = UPPER_PROB,
                              lower_prob = LOWER_PROB,
                              neutral_band = NEUTRAL_BAND,
                              min_length = MIN_LENGTH)

      if (nrow(events) > 0) {
        events[, basin := basin]
        events[, basin_id := attrs$ID[basin]]
        events[, basin_name := attrs$name[basin]]
        events[, member := member]
        events[, window := w]
        events[, model := "CESM2"]
        basin_events[[length(basin_events) + 1]] <- events
      }
    }
  }

  if (length(basin_events) > 0) {
    return(rbindlist(basin_events, fill = TRUE))
  } else {
    return(NULL)
  }
}

# Combine results
cesm_events_results <- cesm_events_results[!sapply(cesm_events_results, is.null)]
events_cesm <- rbindlist(cesm_events_results, fill = TRUE)

cat("\n✓ CESM2 event extraction complete\n")
cat("  Total events:", nrow(events_cesm), "\n")
cat("  Pluvials:", sum(events_cesm$type == "pluvial"), "\n")
cat("  Droughts:", sum(events_cesm$type == "drought"), "\n\n")

# ============================================================================
# COMPUTE REGIME PERSISTENCE FOR CESM2 WINDOWS
# ============================================================================

cat("Computing regime-based persistence for CESM2 windows...\n")
cat("  Processing basins in parallel...\n\n")

# Export compute_regime_persistence function to cluster
clusterExport(cl, c("compute_regime_persistence", "classify_memory_timescale"))

# Parallel processing across basins
cesm_regime_results <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  basin_regime_list <- list()

  # Extract basin data [members × time]
  basin_data <- G_CESM_filtered[basin, , ]

  for (member in 1:n_members_cesm) {
    member_series <- basin_data[member, ]

    # Find valid indices (non-NA)
    valid_idx <- which(!is.na(member_series))

    # Skip if insufficient valid data for even one window
    if (length(valid_idx) < WINDOW_LENGTH) {
      next
    }

    # Determine max number of windows
    max_start <- length(valid_idx) - WINDOW_LENGTH + 1

    # Compute regime persistence for all 273-month windows
    for (w in 1:max_start) {
      start_idx <- valid_idx[w]
      end_idx <- valid_idx[w + WINDOW_LENGTH - 1]

      # Extract window data
      window_data <- member_series[start_idx:end_idx]

      # Compute regime persistence
      regime_metrics <- compute_regime_persistence(window_data)

      if (regime_metrics$success) {
        basin_regime_list[[length(basin_regime_list) + 1]] <- data.table(
          basin = basin,
          basin_id = attrs$ID[basin],
          basin_name = attrs$name[basin],
          member = member,
          window = w,
          pluvial_ar1 = regime_metrics$pluvial_ar1,
          pluvial_p_lf = regime_metrics$pluvial_p_lf,
          pluvial_tau = regime_metrics$pluvial_tau,
          pluvial_tau_class = regime_metrics$pluvial_tau_class,
          pluvial_n_obs = regime_metrics$pluvial_n_obs,
          drought_ar1 = regime_metrics$drought_ar1,
          drought_p_lf = regime_metrics$drought_p_lf,
          drought_tau = regime_metrics$drought_tau,
          drought_tau_class = regime_metrics$drought_tau_class,
          drought_n_obs = regime_metrics$drought_n_obs
        )
      }
    }
  }

  if (length(basin_regime_list) > 0) {
    return(rbindlist(basin_regime_list))
  } else {
    return(NULL)
  }
}

# Combine results
cesm_regime_results <- cesm_regime_results[!sapply(cesm_regime_results, is.null)]
cesm_regime_persistence <- rbindlist(cesm_regime_results, fill = TRUE)

cat("\n✓ CESM2 regime persistence complete\n")
cat("  Total windows with regime metrics:", nrow(cesm_regime_persistence), "\n")
cat("  Windows with pluvial regime metrics:", sum(!is.na(cesm_regime_persistence$pluvial_ar1)), "\n")
cat("  Windows with drought regime metrics:", sum(!is.na(cesm_regime_persistence$drought_ar1)), "\n\n")

# ============================================================================
# EXTRACT MODEL EVENTS (IPSL) WITH 23-YEAR WINDOWING
# ============================================================================

cat("============================================================================\n")
cat("IPSL EVENT EXTRACTION\n")
cat("============================================================================\n\n")

cat("Extracting events from 23-year windows (273 months) to match GRACE...\n")
cat("  18 members × 184 basins × ~1200 windows per member\n")
cat("  Processing basins in parallel...\n\n")

n_members_ipsl <- dim(G_IPSL_combined)[2]
n_times_ipsl <- dim(G_IPSL_combined)[3]

# Parallel processing across basins (variables already exported)
ipsl_events_results <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  basin_events <- list()

  # Extract basin data [members × time]
  basin_data <- G_IPSL_combined[basin, , ]

  for (member in 1:n_members_ipsl) {
    member_series <- basin_data[member, ]

    # Find valid indices (non-NA)
    valid_idx <- which(!is.na(member_series))

    # Skip if insufficient valid data for even one window
    if (length(valid_idx) < WINDOW_LENGTH) {
      next
    }

    # Determine max number of windows
    max_start <- length(valid_idx) - WINDOW_LENGTH + 1

    # Extract events from all possible 273-month windows
    for (w in 1:max_start) {
      start_idx <- valid_idx[w]
      end_idx <- valid_idx[w + WINDOW_LENGTH - 1]

      # Extract window data and dates
      window_data <- member_series[start_idx:end_idx]
      window_dates <- dates_ipsl[start_idx:end_idx]

      # Extract events from this window
      events <- extract_events(window_data, window_dates,
                              upper_prob = UPPER_PROB,
                              lower_prob = LOWER_PROB,
                              neutral_band = NEUTRAL_BAND,
                              min_length = MIN_LENGTH)

      if (nrow(events) > 0) {
        events[, basin := basin]
        events[, basin_id := attrs$ID[basin]]
        events[, basin_name := attrs$name[basin]]
        events[, member := member]
        events[, window := w]
        events[, model := "IPSL"]
        basin_events[[length(basin_events) + 1]] <- events
      }
    }
  }

  if (length(basin_events) > 0) {
    return(rbindlist(basin_events, fill = TRUE))
  } else {
    return(NULL)
  }
}

# Combine results
ipsl_events_results <- ipsl_events_results[!sapply(ipsl_events_results, is.null)]
events_ipsl <- rbindlist(ipsl_events_results, fill = TRUE)

cat("\n✓ IPSL event extraction complete\n")
cat("  Total events:", nrow(events_ipsl), "\n")
cat("  Pluvials:", sum(events_ipsl$type == "pluvial"), "\n")
cat("  Droughts:", sum(events_ipsl$type == "drought"), "\n\n")

# ============================================================================
# COMPUTE REGIME PERSISTENCE FOR IPSL WINDOWS
# ============================================================================

cat("Computing regime-based persistence for IPSL windows...\n")
cat("  Processing basins in parallel...\n\n")

# Parallel processing across basins (functions already exported)
ipsl_regime_results <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  basin_regime_list <- list()

  # Extract basin data [members × time]
  basin_data <- G_IPSL_combined[basin, , ]

  for (member in 1:n_members_ipsl) {
    member_series <- basin_data[member, ]

    # Find valid indices (non-NA)
    valid_idx <- which(!is.na(member_series))

    # Skip if insufficient valid data for even one window
    if (length(valid_idx) < WINDOW_LENGTH) {
      next
    }

    # Determine max number of windows
    max_start <- length(valid_idx) - WINDOW_LENGTH + 1

    # Compute regime persistence for all 273-month windows
    for (w in 1:max_start) {
      start_idx <- valid_idx[w]
      end_idx <- valid_idx[w + WINDOW_LENGTH - 1]

      # Extract window data
      window_data <- member_series[start_idx:end_idx]

      # Compute regime persistence
      regime_metrics <- compute_regime_persistence(window_data)

      if (regime_metrics$success) {
        basin_regime_list[[length(basin_regime_list) + 1]] <- data.table(
          basin = basin,
          basin_id = attrs$ID[basin],
          basin_name = attrs$name[basin],
          member = member,
          window = w,
          pluvial_ar1 = regime_metrics$pluvial_ar1,
          pluvial_p_lf = regime_metrics$pluvial_p_lf,
          pluvial_tau = regime_metrics$pluvial_tau,
          pluvial_tau_class = regime_metrics$pluvial_tau_class,
          pluvial_n_obs = regime_metrics$pluvial_n_obs,
          drought_ar1 = regime_metrics$drought_ar1,
          drought_p_lf = regime_metrics$drought_p_lf,
          drought_tau = regime_metrics$drought_tau,
          drought_tau_class = regime_metrics$drought_tau_class,
          drought_n_obs = regime_metrics$drought_n_obs
        )
      }
    }
  }

  if (length(basin_regime_list) > 0) {
    return(rbindlist(basin_regime_list))
  } else {
    return(NULL)
  }
}

# Combine results
ipsl_regime_results <- ipsl_regime_results[!sapply(ipsl_regime_results, is.null)]
ipsl_regime_persistence <- rbindlist(ipsl_regime_results, fill = TRUE)

cat("\n✓ IPSL regime persistence complete\n")
cat("  Total windows with regime metrics:", nrow(ipsl_regime_persistence), "\n")
cat("  Windows with pluvial regime metrics:", sum(!is.na(ipsl_regime_persistence$pluvial_ar1)), "\n")
cat("  Windows with drought regime metrics:", sum(!is.na(ipsl_regime_persistence$drought_ar1)), "\n\n")

# ============================================================================
# COMBINE MODEL EVENTS
# ============================================================================

cat("Combining CESM2 and IPSL events...\n")

events_models <- rbind(events_cesm, events_ipsl, fill = TRUE)

cat("  Total model events:", nrow(events_models), "\n\n")

# ============================================================================
# COMPUTE EVENT SUMMARIES BY BASIN
# ============================================================================

cat("Computing basin-level event summaries...\n")

# For each basin, compute:
# - Max pluvial height (H_max)
# - Max drought depth (D_max = most negative)
# - Max intensity (cumulative)
# - Mean/median duration
# - Mean/median recovery time

# GRACE summaries
grace_summary <- events_grace[, .(
  n_events = as.integer(.N),
  n_pluvials = as.integer(sum(type == "pluvial")),
  n_droughts = as.integer(sum(type == "drought")),
  H_max_grace = ifelse(any(type == "pluvial"), max(pluvial_height, na.rm = TRUE), NA_real_),
  D_max_grace = ifelse(any(type == "drought"), min(drought_depth, na.rm = TRUE), NA_real_),
  I_max_pluvial_grace = ifelse(any(type == "pluvial"), max(intensity[type == "pluvial"], na.rm = TRUE), NA_real_),
  I_max_drought_grace = ifelse(any(type == "drought"), max(intensity[type == "drought"], na.rm = TRUE), NA_real_),
  mean_duration_grace = as.numeric(mean(duration_months, na.rm = TRUE)),
  median_duration_grace = as.numeric(median(duration_months, na.rm = TRUE)),
  mean_recovery_grace = as.numeric(mean(recovery_months, na.rm = TRUE)),
  median_recovery_grace = as.numeric(median(recovery_months, na.rm = TRUE))
), by = .(basin, basin_id, basin_name)]

# Model summaries (CESM2) - per member
cesm_summary <- events_cesm[, .(
  H_max = ifelse(any(type == "pluvial"), max(pluvial_height, na.rm = TRUE), NA_real_),
  D_max = ifelse(any(type == "drought"), min(drought_depth, na.rm = TRUE), NA_real_),
  I_max_pluvial = ifelse(any(type == "pluvial"), max(intensity[type == "pluvial"], na.rm = TRUE), NA_real_),
  I_max_drought = ifelse(any(type == "drought"), max(intensity[type == "drought"], na.rm = TRUE), NA_real_),
  mean_duration = as.numeric(mean(duration_months, na.rm = TRUE)),
  mean_recovery = as.numeric(mean(recovery_months, na.rm = TRUE))
), by = .(basin, basin_id, basin_name, member)]

# Compute percentiles across all members
cesm_percentiles <- cesm_summary[, .(
  H_max_p05_cesm = quantile(H_max, 0.05, na.rm = TRUE),
  H_max_p50_cesm = quantile(H_max, 0.50, na.rm = TRUE),
  H_max_p95_cesm = quantile(H_max, 0.95, na.rm = TRUE),
  D_max_p05_cesm = quantile(D_max, 0.05, na.rm = TRUE),
  D_max_p50_cesm = quantile(D_max, 0.50, na.rm = TRUE),
  D_max_p95_cesm = quantile(D_max, 0.95, na.rm = TRUE),
  I_pluvial_p05_cesm = quantile(I_max_pluvial, 0.05, na.rm = TRUE),
  I_pluvial_p50_cesm = quantile(I_max_pluvial, 0.50, na.rm = TRUE),
  I_pluvial_p95_cesm = quantile(I_max_pluvial, 0.95, na.rm = TRUE),
  I_drought_p05_cesm = quantile(I_max_drought, 0.05, na.rm = TRUE),
  I_drought_p50_cesm = quantile(I_max_drought, 0.50, na.rm = TRUE),
  I_drought_p95_cesm = quantile(I_max_drought, 0.95, na.rm = TRUE),
  duration_mean_cesm = mean(mean_duration, na.rm = TRUE),
  duration_sd_cesm = sd(mean_duration, na.rm = TRUE),
  recovery_mean_cesm = mean(mean_recovery, na.rm = TRUE),
  recovery_sd_cesm = sd(mean_recovery, na.rm = TRUE)
), by = .(basin, basin_id, basin_name)]

# Model summaries (IPSL) - per member
ipsl_summary <- events_ipsl[, .(
  H_max = ifelse(any(type == "pluvial"), max(pluvial_height, na.rm = TRUE), NA_real_),
  D_max = ifelse(any(type == "drought"), min(drought_depth, na.rm = TRUE), NA_real_),
  I_max_pluvial = ifelse(any(type == "pluvial"), max(intensity[type == "pluvial"], na.rm = TRUE), NA_real_),
  I_max_drought = ifelse(any(type == "drought"), max(intensity[type == "drought"], na.rm = TRUE), NA_real_),
  mean_duration = as.numeric(mean(duration_months, na.rm = TRUE)),
  mean_recovery = as.numeric(mean(recovery_months, na.rm = TRUE))
), by = .(basin, basin_id, basin_name, member)]

# Compute percentiles across all members
ipsl_percentiles <- ipsl_summary[, .(
  H_max_p05_ipsl = quantile(H_max, 0.05, na.rm = TRUE),
  H_max_p50_ipsl = quantile(H_max, 0.50, na.rm = TRUE),
  H_max_p95_ipsl = quantile(H_max, 0.95, na.rm = TRUE),
  D_max_p05_ipsl = quantile(D_max, 0.05, na.rm = TRUE),
  D_max_p50_ipsl = quantile(D_max, 0.50, na.rm = TRUE),
  D_max_p95_ipsl = quantile(D_max, 0.95, na.rm = TRUE),
  I_pluvial_p05_ipsl = quantile(I_max_pluvial, 0.05, na.rm = TRUE),
  I_pluvial_p50_ipsl = quantile(I_max_pluvial, 0.50, na.rm = TRUE),
  I_pluvial_p95_ipsl = quantile(I_max_pluvial, 0.95, na.rm = TRUE),
  I_drought_p05_ipsl = quantile(I_max_drought, 0.05, na.rm = TRUE),
  I_drought_p50_ipsl = quantile(I_max_drought, 0.50, na.rm = TRUE),
  I_drought_p95_ipsl = quantile(I_max_drought, 0.95, na.rm = TRUE),
  duration_mean_ipsl = mean(mean_duration, na.rm = TRUE),
  duration_sd_ipsl = sd(mean_duration, na.rm = TRUE),
  recovery_mean_ipsl = mean(mean_recovery, na.rm = TRUE),
  recovery_sd_ipsl = sd(mean_recovery, na.rm = TRUE)
), by = .(basin, basin_id, basin_name)]

# Merge GRACE with model percentiles
event_summary <- merge(grace_summary, cesm_percentiles, by = c("basin", "basin_id", "basin_name"), all = TRUE)
event_summary <- merge(event_summary, ipsl_percentiles, by = c("basin", "basin_id", "basin_name"), all = TRUE)

# Compute percentile ranks for GRACE events in model distributions
# For each basin, where do GRACE metrics fall in the model distributions?
cat("Computing percentile ranks for GRACE metrics...\n")

for (i in 1:nrow(event_summary)) {
  basin_i <- event_summary$basin[i]

  # CESM2 percentile ranks - Pluvial Height
  if (!is.na(event_summary$H_max_grace[i])) {
    cesm_H_vals <- cesm_summary[basin == basin_i, H_max]
    if (length(cesm_H_vals) > 0 && !all(is.na(cesm_H_vals))) {
      event_summary$percentile_H_cesm[i] <- mean(cesm_H_vals <= event_summary$H_max_grace[i], na.rm = TRUE) * 100
    } else {
      event_summary$percentile_H_cesm[i] <- NA_real_
    }
  } else {
    event_summary$percentile_H_cesm[i] <- NA_real_
  }

  # CESM2 percentile ranks - Drought Depth
  # Use absolute values so higher percentile = more extreme (consistent with pluvials)
  if (!is.na(event_summary$D_max_grace[i])) {
    cesm_D_vals <- cesm_summary[basin == basin_i, D_max]
    if (length(cesm_D_vals) > 0 && !all(is.na(cesm_D_vals))) {
      event_summary$percentile_D_cesm[i] <- mean(abs(cesm_D_vals) <= abs(event_summary$D_max_grace[i]), na.rm = TRUE) * 100
    } else {
      event_summary$percentile_D_cesm[i] <- NA_real_
    }
  } else {
    event_summary$percentile_D_cesm[i] <- NA_real_
  }

  # CESM2 percentile ranks - Pluvial Intensity
  if (!is.na(event_summary$I_max_pluvial_grace[i])) {
    cesm_I_pluvial_vals <- cesm_summary[basin == basin_i, I_max_pluvial]
    if (length(cesm_I_pluvial_vals) > 0 && !all(is.na(cesm_I_pluvial_vals))) {
      event_summary$percentile_I_pluvial_cesm[i] <- mean(cesm_I_pluvial_vals <= event_summary$I_max_pluvial_grace[i], na.rm = TRUE) * 100
    } else {
      event_summary$percentile_I_pluvial_cesm[i] <- NA_real_
    }
  } else {
    event_summary$percentile_I_pluvial_cesm[i] <- NA_real_
  }

  # CESM2 percentile ranks - Drought Intensity
  if (!is.na(event_summary$I_max_drought_grace[i])) {
    cesm_I_drought_vals <- cesm_summary[basin == basin_i, I_max_drought]
    if (length(cesm_I_drought_vals) > 0 && !all(is.na(cesm_I_drought_vals))) {
      event_summary$percentile_I_drought_cesm[i] <- mean(cesm_I_drought_vals <= event_summary$I_max_drought_grace[i], na.rm = TRUE) * 100
    } else {
      event_summary$percentile_I_drought_cesm[i] <- NA_real_
    }
  } else {
    event_summary$percentile_I_drought_cesm[i] <- NA_real_
  }

  # IPSL percentile ranks - Pluvial Height
  if (!is.na(event_summary$H_max_grace[i])) {
    ipsl_H_vals <- ipsl_summary[basin == basin_i, H_max]
    if (length(ipsl_H_vals) > 0 && !all(is.na(ipsl_H_vals))) {
      event_summary$percentile_H_ipsl[i] <- mean(ipsl_H_vals <= event_summary$H_max_grace[i], na.rm = TRUE) * 100
    } else {
      event_summary$percentile_H_ipsl[i] <- NA_real_
    }
  } else {
    event_summary$percentile_H_ipsl[i] <- NA_real_
  }

  # IPSL percentile ranks - Drought Depth
  # Use absolute values so higher percentile = more extreme (consistent with pluvials)
  if (!is.na(event_summary$D_max_grace[i])) {
    ipsl_D_vals <- ipsl_summary[basin == basin_i, D_max]
    if (length(ipsl_D_vals) > 0 && !all(is.na(ipsl_D_vals))) {
      event_summary$percentile_D_ipsl[i] <- mean(abs(ipsl_D_vals) <= abs(event_summary$D_max_grace[i]), na.rm = TRUE) * 100
    } else {
      event_summary$percentile_D_ipsl[i] <- NA_real_
    }
  } else {
    event_summary$percentile_D_ipsl[i] <- NA_real_
  }

  # IPSL percentile ranks - Pluvial Intensity
  if (!is.na(event_summary$I_max_pluvial_grace[i])) {
    ipsl_I_pluvial_vals <- ipsl_summary[basin == basin_i, I_max_pluvial]
    if (length(ipsl_I_pluvial_vals) > 0 && !all(is.na(ipsl_I_pluvial_vals))) {
      event_summary$percentile_I_pluvial_ipsl[i] <- mean(ipsl_I_pluvial_vals <= event_summary$I_max_pluvial_grace[i], na.rm = TRUE) * 100
    } else {
      event_summary$percentile_I_pluvial_ipsl[i] <- NA_real_
    }
  } else {
    event_summary$percentile_I_pluvial_ipsl[i] <- NA_real_
  }

  # IPSL percentile ranks - Drought Intensity
  if (!is.na(event_summary$I_max_drought_grace[i])) {
    ipsl_I_drought_vals <- ipsl_summary[basin == basin_i, I_max_drought]
    if (length(ipsl_I_drought_vals) > 0 && !all(is.na(ipsl_I_drought_vals))) {
      event_summary$percentile_I_drought_ipsl[i] <- mean(ipsl_I_drought_vals <= event_summary$I_max_drought_grace[i], na.rm = TRUE) * 100
    } else {
      event_summary$percentile_I_drought_ipsl[i] <- NA_real_
    }
  } else {
    event_summary$percentile_I_drought_ipsl[i] <- NA_real_
  }
}

cat("✓ Basin-level summaries complete\n")
cat("  Basins with summaries:", nrow(event_summary), "\n\n")

# ============================================================================
# COMPUTE REGIME PERSISTENCE SUMMARIES BY BASIN
# ============================================================================

cat("Computing basin-level regime persistence summaries...\n")

# CESM2 regime persistence distributions
cesm_regime_summary <- cesm_regime_persistence[, .(
  # Pluvial regime statistics
  pluvial_ar1_p05_cesm = quantile(pluvial_ar1, 0.05, na.rm = TRUE),
  pluvial_ar1_p50_cesm = quantile(pluvial_ar1, 0.50, na.rm = TRUE),
  pluvial_ar1_p95_cesm = quantile(pluvial_ar1, 0.95, na.rm = TRUE),
  pluvial_ar1_mean_cesm = mean(pluvial_ar1, na.rm = TRUE),
  pluvial_p_lf_p05_cesm = quantile(pluvial_p_lf, 0.05, na.rm = TRUE),
  pluvial_p_lf_p50_cesm = quantile(pluvial_p_lf, 0.50, na.rm = TRUE),
  pluvial_p_lf_p95_cesm = quantile(pluvial_p_lf, 0.95, na.rm = TRUE),
  pluvial_p_lf_mean_cesm = mean(pluvial_p_lf, na.rm = TRUE),
  pluvial_tau_p05_cesm = quantile(pluvial_tau, 0.05, na.rm = TRUE),
  pluvial_tau_p50_cesm = quantile(pluvial_tau, 0.50, na.rm = TRUE),
  pluvial_tau_p95_cesm = quantile(pluvial_tau, 0.95, na.rm = TRUE),
  pluvial_tau_mean_cesm = mean(pluvial_tau, na.rm = TRUE),

  # Drought regime statistics
  drought_ar1_p05_cesm = quantile(drought_ar1, 0.05, na.rm = TRUE),
  drought_ar1_p50_cesm = quantile(drought_ar1, 0.50, na.rm = TRUE),
  drought_ar1_p95_cesm = quantile(drought_ar1, 0.95, na.rm = TRUE),
  drought_ar1_mean_cesm = mean(drought_ar1, na.rm = TRUE),
  drought_p_lf_p05_cesm = quantile(drought_p_lf, 0.05, na.rm = TRUE),
  drought_p_lf_p50_cesm = quantile(drought_p_lf, 0.50, na.rm = TRUE),
  drought_p_lf_p95_cesm = quantile(drought_p_lf, 0.95, na.rm = TRUE),
  drought_p_lf_mean_cesm = mean(drought_p_lf, na.rm = TRUE),
  drought_tau_p05_cesm = quantile(drought_tau, 0.05, na.rm = TRUE),
  drought_tau_p50_cesm = quantile(drought_tau, 0.50, na.rm = TRUE),
  drought_tau_p95_cesm = quantile(drought_tau, 0.95, na.rm = TRUE),
  drought_tau_mean_cesm = mean(drought_tau, na.rm = TRUE),

  n_windows_cesm = .N
), by = basin]

# IPSL regime persistence distributions
ipsl_regime_summary <- ipsl_regime_persistence[, .(
  # Pluvial regime statistics
  pluvial_ar1_p05_ipsl = quantile(pluvial_ar1, 0.05, na.rm = TRUE),
  pluvial_ar1_p50_ipsl = quantile(pluvial_ar1, 0.50, na.rm = TRUE),
  pluvial_ar1_p95_ipsl = quantile(pluvial_ar1, 0.95, na.rm = TRUE),
  pluvial_ar1_mean_ipsl = mean(pluvial_ar1, na.rm = TRUE),
  pluvial_p_lf_p05_ipsl = quantile(pluvial_p_lf, 0.05, na.rm = TRUE),
  pluvial_p_lf_p50_ipsl = quantile(pluvial_p_lf, 0.50, na.rm = TRUE),
  pluvial_p_lf_p95_ipsl = quantile(pluvial_p_lf, 0.95, na.rm = TRUE),
  pluvial_p_lf_mean_ipsl = mean(pluvial_p_lf, na.rm = TRUE),
  pluvial_tau_p05_ipsl = quantile(pluvial_tau, 0.05, na.rm = TRUE),
  pluvial_tau_p50_ipsl = quantile(pluvial_tau, 0.50, na.rm = TRUE),
  pluvial_tau_p95_ipsl = quantile(pluvial_tau, 0.95, na.rm = TRUE),
  pluvial_tau_mean_ipsl = mean(pluvial_tau, na.rm = TRUE),

  # Drought regime statistics
  drought_ar1_p05_ipsl = quantile(drought_ar1, 0.05, na.rm = TRUE),
  drought_ar1_p50_ipsl = quantile(drought_ar1, 0.50, na.rm = TRUE),
  drought_ar1_p95_ipsl = quantile(drought_ar1, 0.95, na.rm = TRUE),
  drought_ar1_mean_ipsl = mean(drought_ar1, na.rm = TRUE),
  drought_p_lf_p05_ipsl = quantile(drought_p_lf, 0.05, na.rm = TRUE),
  drought_p_lf_p50_ipsl = quantile(drought_p_lf, 0.50, na.rm = TRUE),
  drought_p_lf_p95_ipsl = quantile(drought_p_lf, 0.95, na.rm = TRUE),
  drought_p_lf_mean_ipsl = mean(drought_p_lf, na.rm = TRUE),
  drought_tau_p05_ipsl = quantile(drought_tau, 0.05, na.rm = TRUE),
  drought_tau_p50_ipsl = quantile(drought_tau, 0.50, na.rm = TRUE),
  drought_tau_p95_ipsl = quantile(drought_tau, 0.95, na.rm = TRUE),
  drought_tau_mean_ipsl = mean(drought_tau, na.rm = TRUE),

  n_windows_ipsl = .N
), by = basin]

# Merge GRACE regime persistence with model distributions
regime_persistence_summary <- merge(grace_regime_persistence, cesm_regime_summary, by = "basin", all = TRUE)
regime_persistence_summary <- merge(regime_persistence_summary, ipsl_regime_summary, by = "basin", all = TRUE)

# Compute percentile ranks for GRACE regime metrics in model distributions
cat("Computing percentile ranks for GRACE regime metrics...\n")

for (i in 1:nrow(regime_persistence_summary)) {
  basin_i <- regime_persistence_summary$basin[i]

  # CESM2 percentiles - Pluvial AR(1)
  if (!is.na(regime_persistence_summary$pluvial_ar1_grace[i])) {
    cesm_pluvial_ar1_vals <- cesm_regime_persistence[basin == basin_i, pluvial_ar1]
    if (length(cesm_pluvial_ar1_vals) > 0 && !all(is.na(cesm_pluvial_ar1_vals))) {
      regime_persistence_summary$pluvial_ar1_percentile_cesm[i] <- mean(cesm_pluvial_ar1_vals <= regime_persistence_summary$pluvial_ar1_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$pluvial_ar1_percentile_cesm[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$pluvial_ar1_percentile_cesm[i] <- NA_real_
  }

  # CESM2 percentiles - Pluvial P_LF
  if (!is.na(regime_persistence_summary$pluvial_p_lf_grace[i])) {
    cesm_pluvial_p_lf_vals <- cesm_regime_persistence[basin == basin_i, pluvial_p_lf]
    if (length(cesm_pluvial_p_lf_vals) > 0 && !all(is.na(cesm_pluvial_p_lf_vals))) {
      regime_persistence_summary$pluvial_p_lf_percentile_cesm[i] <- mean(cesm_pluvial_p_lf_vals <= regime_persistence_summary$pluvial_p_lf_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$pluvial_p_lf_percentile_cesm[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$pluvial_p_lf_percentile_cesm[i] <- NA_real_
  }

  # CESM2 percentiles - Pluvial tau
  if (!is.na(regime_persistence_summary$pluvial_tau_grace[i])) {
    cesm_pluvial_tau_vals <- cesm_regime_persistence[basin == basin_i, pluvial_tau]
    if (length(cesm_pluvial_tau_vals) > 0 && !all(is.na(cesm_pluvial_tau_vals))) {
      regime_persistence_summary$pluvial_tau_percentile_cesm[i] <- mean(cesm_pluvial_tau_vals <= regime_persistence_summary$pluvial_tau_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$pluvial_tau_percentile_cesm[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$pluvial_tau_percentile_cesm[i] <- NA_real_
  }

  # CESM2 percentiles - Drought AR(1)
  if (!is.na(regime_persistence_summary$drought_ar1_grace[i])) {
    cesm_drought_ar1_vals <- cesm_regime_persistence[basin == basin_i, drought_ar1]
    if (length(cesm_drought_ar1_vals) > 0 && !all(is.na(cesm_drought_ar1_vals))) {
      regime_persistence_summary$drought_ar1_percentile_cesm[i] <- mean(cesm_drought_ar1_vals <= regime_persistence_summary$drought_ar1_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$drought_ar1_percentile_cesm[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$drought_ar1_percentile_cesm[i] <- NA_real_
  }

  # CESM2 percentiles - Drought P_LF
  if (!is.na(regime_persistence_summary$drought_p_lf_grace[i])) {
    cesm_drought_p_lf_vals <- cesm_regime_persistence[basin == basin_i, drought_p_lf]
    if (length(cesm_drought_p_lf_vals) > 0 && !all(is.na(cesm_drought_p_lf_vals))) {
      regime_persistence_summary$drought_p_lf_percentile_cesm[i] <- mean(cesm_drought_p_lf_vals <= regime_persistence_summary$drought_p_lf_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$drought_p_lf_percentile_cesm[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$drought_p_lf_percentile_cesm[i] <- NA_real_
  }

  # CESM2 percentiles - Drought tau
  if (!is.na(regime_persistence_summary$drought_tau_grace[i])) {
    cesm_drought_tau_vals <- cesm_regime_persistence[basin == basin_i, drought_tau]
    if (length(cesm_drought_tau_vals) > 0 && !all(is.na(cesm_drought_tau_vals))) {
      regime_persistence_summary$drought_tau_percentile_cesm[i] <- mean(cesm_drought_tau_vals <= regime_persistence_summary$drought_tau_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$drought_tau_percentile_cesm[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$drought_tau_percentile_cesm[i] <- NA_real_
  }

  # IPSL percentiles (same pattern for IPSL)
  # Pluvial AR(1)
  if (!is.na(regime_persistence_summary$pluvial_ar1_grace[i])) {
    ipsl_pluvial_ar1_vals <- ipsl_regime_persistence[basin == basin_i, pluvial_ar1]
    if (length(ipsl_pluvial_ar1_vals) > 0 && !all(is.na(ipsl_pluvial_ar1_vals))) {
      regime_persistence_summary$pluvial_ar1_percentile_ipsl[i] <- mean(ipsl_pluvial_ar1_vals <= regime_persistence_summary$pluvial_ar1_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$pluvial_ar1_percentile_ipsl[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$pluvial_ar1_percentile_ipsl[i] <- NA_real_
  }

  # Pluvial P_LF
  if (!is.na(regime_persistence_summary$pluvial_p_lf_grace[i])) {
    ipsl_pluvial_p_lf_vals <- ipsl_regime_persistence[basin == basin_i, pluvial_p_lf]
    if (length(ipsl_pluvial_p_lf_vals) > 0 && !all(is.na(ipsl_pluvial_p_lf_vals))) {
      regime_persistence_summary$pluvial_p_lf_percentile_ipsl[i] <- mean(ipsl_pluvial_p_lf_vals <= regime_persistence_summary$pluvial_p_lf_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$pluvial_p_lf_percentile_ipsl[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$pluvial_p_lf_percentile_ipsl[i] <- NA_real_
  }

  # Pluvial tau
  if (!is.na(regime_persistence_summary$pluvial_tau_grace[i])) {
    ipsl_pluvial_tau_vals <- ipsl_regime_persistence[basin == basin_i, pluvial_tau]
    if (length(ipsl_pluvial_tau_vals) > 0 && !all(is.na(ipsl_pluvial_tau_vals))) {
      regime_persistence_summary$pluvial_tau_percentile_ipsl[i] <- mean(ipsl_pluvial_tau_vals <= regime_persistence_summary$pluvial_tau_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$pluvial_tau_percentile_ipsl[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$pluvial_tau_percentile_ipsl[i] <- NA_real_
  }

  # Drought AR(1)
  if (!is.na(regime_persistence_summary$drought_ar1_grace[i])) {
    ipsl_drought_ar1_vals <- ipsl_regime_persistence[basin == basin_i, drought_ar1]
    if (length(ipsl_drought_ar1_vals) > 0 && !all(is.na(ipsl_drought_ar1_vals))) {
      regime_persistence_summary$drought_ar1_percentile_ipsl[i] <- mean(ipsl_drought_ar1_vals <= regime_persistence_summary$drought_ar1_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$drought_ar1_percentile_ipsl[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$drought_ar1_percentile_ipsl[i] <- NA_real_
  }

  # Drought P_LF
  if (!is.na(regime_persistence_summary$drought_p_lf_grace[i])) {
    ipsl_drought_p_lf_vals <- ipsl_regime_persistence[basin == basin_i, drought_p_lf]
    if (length(ipsl_drought_p_lf_vals) > 0 && !all(is.na(ipsl_drought_p_lf_vals))) {
      regime_persistence_summary$drought_p_lf_percentile_ipsl[i] <- mean(ipsl_drought_p_lf_vals <= regime_persistence_summary$drought_p_lf_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$drought_p_lf_percentile_ipsl[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$drought_p_lf_percentile_ipsl[i] <- NA_real_
  }

  # Drought tau
  if (!is.na(regime_persistence_summary$drought_tau_grace[i])) {
    ipsl_drought_tau_vals <- ipsl_regime_persistence[basin == basin_i, drought_tau]
    if (length(ipsl_drought_tau_vals) > 0 && !all(is.na(ipsl_drought_tau_vals))) {
      regime_persistence_summary$drought_tau_percentile_ipsl[i] <- mean(ipsl_drought_tau_vals <= regime_persistence_summary$drought_tau_grace[i], na.rm = TRUE) * 100
    } else {
      regime_persistence_summary$drought_tau_percentile_ipsl[i] <- NA_real_
    }
  } else {
    regime_persistence_summary$drought_tau_percentile_ipsl[i] <- NA_real_
  }
}

# Add tau class for median CESM2 and IPSL values
regime_persistence_summary[, pluvial_tau_class_p50_cesm := sapply(pluvial_tau_p50_cesm, classify_memory_timescale)]
regime_persistence_summary[, drought_tau_class_p50_cesm := sapply(drought_tau_p50_cesm, classify_memory_timescale)]
regime_persistence_summary[, pluvial_tau_class_p50_ipsl := sapply(pluvial_tau_p50_ipsl, classify_memory_timescale)]
regime_persistence_summary[, drought_tau_class_p50_ipsl := sapply(drought_tau_p50_ipsl, classify_memory_timescale)]

# Add basin attributes
attrs_dt <- data.table(attrs)
attrs_dt[, basin_id := ID]
regime_persistence_summary <- merge(regime_persistence_summary, attrs_dt[, .(basin_id, name, area)],
                                    by = "basin_id", all.x = TRUE, suffixes = c("", "_attr"))

cat("✓ Regime persistence summaries complete\n")
cat("  Basins with regime summaries:", nrow(regime_persistence_summary), "\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

# Save event catalogs
saveRDS(events_grace, "outputs/phase06_events_grace.rds")
fwrite(events_grace, "outputs/phase06_events_grace.csv")

saveRDS(events_models, "outputs/phase06_events_models.rds")
fwrite(events_models, "outputs/phase06_events_models.csv")

# Save summary
saveRDS(event_summary, "outputs/phase06_event_summary.rds")
fwrite(event_summary, "outputs/phase06_event_summary.csv")

# Save regime persistence results
saveRDS(grace_regime_persistence, "outputs/phase06_grace_regime_persistence.rds")
fwrite(grace_regime_persistence, "outputs/phase06_grace_regime_persistence.csv")

saveRDS(cesm_regime_persistence, "outputs/phase06_cesm_regime_persistence.rds")
fwrite(cesm_regime_persistence, "outputs/phase06_cesm_regime_persistence.csv")

saveRDS(ipsl_regime_persistence, "outputs/phase06_ipsl_regime_persistence.rds")
fwrite(ipsl_regime_persistence, "outputs/phase06_ipsl_regime_persistence.csv")

saveRDS(regime_persistence_summary, "outputs/phase06_regime_persistence_summary.rds")
fwrite(regime_persistence_summary, "outputs/phase06_regime_persistence_summary.csv")

cat("  ✓ outputs/phase06_events_grace.{rds,csv}\n")
cat("  ✓ outputs/phase06_events_models.{rds,csv}\n")
cat("  ✓ outputs/phase06_event_summary.{rds,csv}\n")
cat("  ✓ outputs/phase06_grace_regime_persistence.{rds,csv}\n")
cat("  ✓ outputs/phase06_cesm_regime_persistence.{rds,csv}\n")
cat("  ✓ outputs/phase06_ipsl_regime_persistence.{rds,csv}\n")
cat("  ✓ outputs/phase06_regime_persistence_summary.{rds,csv}\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("============================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================================================\n\n")

cat("GRACE events:\n")
cat("  Total events:", nrow(events_grace), "\n")
cat("  Pluvials:", sum(events_grace$type == "pluvial"), "\n")
cat("  Droughts:", sum(events_grace$type == "drought"), "\n")
cat("  Mean duration:", round(mean(events_grace$duration_months, na.rm = TRUE), 1), "months\n")
cat("  Mean recovery:", round(mean(events_grace$recovery_months, na.rm = TRUE), 1), "months\n")
cat("  Mean pluvial height:", round(mean(events_grace$pluvial_height, na.rm = TRUE), 1), "mm\n")
cat("  Mean drought depth:", round(mean(events_grace$drought_depth, na.rm = TRUE), 1), "mm\n")
cat("  Mean intensity:", round(mean(events_grace$intensity, na.rm = TRUE), 1), "mm·months\n\n")

cat("CESM2 events:\n")
cat("  Total events:", nrow(events_cesm), "\n")
cat("  Pluvials:", sum(events_cesm$type == "pluvial"), "\n")
cat("  Droughts:", sum(events_cesm$type == "drought"), "\n")
cat("  Mean duration:", round(mean(events_cesm$duration_months, na.rm = TRUE), 1), "months\n")
cat("  Mean recovery:", round(mean(events_cesm$recovery_months, na.rm = TRUE), 1), "months\n")
cat("  Mean pluvial height:", round(mean(events_cesm$pluvial_height, na.rm = TRUE), 1), "mm\n")
cat("  Mean drought depth:", round(mean(events_cesm$drought_depth, na.rm = TRUE), 1), "mm\n")
cat("  Mean intensity:", round(mean(events_cesm$intensity, na.rm = TRUE), 1), "mm·months\n\n")

cat("IPSL events:\n")
cat("  Total events:", nrow(events_ipsl), "\n")
cat("  Pluvials:", sum(events_ipsl$type == "pluvial"), "\n")
cat("  Droughts:", sum(events_ipsl$type == "drought"), "\n")
cat("  Mean duration:", round(mean(events_ipsl$duration_months, na.rm = TRUE), 1), "months\n")
cat("  Mean recovery:", round(mean(events_ipsl$recovery_months, na.rm = TRUE), 1), "months\n")
cat("  Mean pluvial height:", round(mean(events_ipsl$pluvial_height, na.rm = TRUE), 1), "mm\n")
cat("  Mean drought depth:", round(mean(events_ipsl$drought_depth, na.rm = TRUE), 1), "mm\n")
cat("  Mean intensity:", round(mean(events_ipsl$intensity, na.rm = TRUE), 1), "mm·months\n\n")

cat("Event coverage by basin:\n")
cat("  Basins with GRACE events:", sum(event_summary$n_events > 0, na.rm = TRUE), "\n")
cat("  Basins where H_max_grace > CESM2 p95:", sum(event_summary$H_max_grace > event_summary$H_max_p95_cesm, na.rm = TRUE), "\n")
cat("  Basins where D_max_grace < CESM2 p05:", sum(event_summary$D_max_grace < event_summary$D_max_p05_cesm, na.rm = TRUE), "\n")
cat("  Basins where H_max_grace > IPSL p95:", sum(event_summary$H_max_grace > event_summary$H_max_p95_ipsl, na.rm = TRUE), "\n")
cat("  Basins where D_max_grace < IPSL p05:", sum(event_summary$D_max_grace < event_summary$D_max_p05_ipsl, na.rm = TRUE), "\n\n")

cat("Percentile ranks (median across basins):\n")
cat("  Pluvial Height:\n")
cat("    GRACE H_max in CESM2:", round(median(event_summary$percentile_H_cesm, na.rm = TRUE), 1), "th percentile\n")
cat("    GRACE H_max in IPSL:", round(median(event_summary$percentile_H_ipsl, na.rm = TRUE), 1), "th percentile\n")
cat("  Drought Depth:\n")
cat("    GRACE D_max in CESM2:", round(median(event_summary$percentile_D_cesm, na.rm = TRUE), 1), "th percentile\n")
cat("    GRACE D_max in IPSL:", round(median(event_summary$percentile_D_ipsl, na.rm = TRUE), 1), "th percentile\n")
cat("  Pluvial Intensity:\n")
cat("    GRACE I_pluvial in CESM2:", round(median(event_summary$percentile_I_pluvial_cesm, na.rm = TRUE), 1), "th percentile\n")
cat("    GRACE I_pluvial in IPSL:", round(median(event_summary$percentile_I_pluvial_ipsl, na.rm = TRUE), 1), "th percentile\n")
cat("  Drought Intensity:\n")
cat("    GRACE I_drought in CESM2:", round(median(event_summary$percentile_I_drought_cesm, na.rm = TRUE), 1), "th percentile\n")
cat("    GRACE I_drought in IPSL:", round(median(event_summary$percentile_I_drought_ipsl, na.rm = TRUE), 1), "th percentile\n\n")

cat("Regime-based persistence:\n")
cat("  GRACE basins:\n")
cat("    Pluvial regime - median AR(1):", round(median(regime_persistence_summary$pluvial_ar1_grace, na.rm = TRUE), 3), "\n")
cat("    Pluvial regime - median tau:", round(median(regime_persistence_summary$pluvial_tau_grace, na.rm = TRUE), 1), "months\n")
cat("    Drought regime - median AR(1):", round(median(regime_persistence_summary$drought_ar1_grace, na.rm = TRUE), 3), "\n")
cat("    Drought regime - median tau:", round(median(regime_persistence_summary$drought_tau_grace, na.rm = TRUE), 1), "months\n\n")

cat("  CESM2 windows:\n")
cat("    Pluvial regime - median AR(1) (p50):", round(median(regime_persistence_summary$pluvial_ar1_p50_cesm, na.rm = TRUE), 3), "\n")
cat("    Pluvial regime - median tau (p50):", round(median(regime_persistence_summary$pluvial_tau_p50_cesm, na.rm = TRUE), 1), "months\n")
cat("    Drought regime - median AR(1) (p50):", round(median(regime_persistence_summary$drought_ar1_p50_cesm, na.rm = TRUE), 3), "\n")
cat("    Drought regime - median tau (p50):", round(median(regime_persistence_summary$drought_tau_p50_cesm, na.rm = TRUE), 1), "months\n\n")

cat("  IPSL windows:\n")
cat("    Pluvial regime - median AR(1) (p50):", round(median(regime_persistence_summary$pluvial_ar1_p50_ipsl, na.rm = TRUE), 3), "\n")
cat("    Pluvial regime - median tau (p50):", round(median(regime_persistence_summary$pluvial_tau_p50_ipsl, na.rm = TRUE), 1), "months\n")
cat("    Drought regime - median AR(1) (p50):", round(median(regime_persistence_summary$drought_ar1_p50_ipsl, na.rm = TRUE), 3), "\n")
cat("    Drought regime - median tau (p50):", round(median(regime_persistence_summary$drought_tau_p50_ipsl, na.rm = TRUE), 1), "months\n\n")

cat("  Percentile ranks (median across basins):\n")
cat("    GRACE pluvial AR(1) in CESM2:", round(median(regime_persistence_summary$pluvial_ar1_percentile_cesm, na.rm = TRUE), 1), "th percentile\n")
cat("    GRACE pluvial tau in CESM2:", round(median(regime_persistence_summary$pluvial_tau_percentile_cesm, na.rm = TRUE), 1), "th percentile\n")
cat("    GRACE drought AR(1) in CESM2:", round(median(regime_persistence_summary$drought_ar1_percentile_cesm, na.rm = TRUE), 1), "th percentile\n")
cat("    GRACE drought tau in CESM2:", round(median(regime_persistence_summary$drought_tau_percentile_cesm, na.rm = TRUE), 1), "th percentile\n\n")

cat("  Tau classification distribution (CESM2 windows):\n")
cesm_pluvial_tau_class_dist <- table(cesm_regime_persistence$pluvial_tau_class)
cesm_drought_tau_class_dist <- table(cesm_regime_persistence$drought_tau_class)
cat("    Pluvial regime:\n")
print(cesm_pluvial_tau_class_dist)
cat("    Drought regime:\n")
print(cesm_drought_tau_class_dist)
cat("\n")

cat("  Tau classification distribution (GRACE basins):\n")
grace_pluvial_tau_class_dist <- table(grace_regime_persistence$pluvial_tau_class_grace)
grace_drought_tau_class_dist <- table(grace_regime_persistence$drought_tau_class_grace)
cat("    Pluvial regime:\n")
print(grace_pluvial_tau_class_dist)
cat("    Drought regime:\n")
print(grace_drought_tau_class_dist)
cat("\n")

# Cleanup parallel cluster
stopCluster(cl)
cat("✓ Parallel cluster stopped\n\n")

cat("============================================================================\n")
cat("PHASE 06 COMPLETE\n")
cat("============================================================================\n\n")
