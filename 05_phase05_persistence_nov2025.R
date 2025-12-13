#!/usr/bin/env Rscript
# ============================================================================
# PHASE 05: PERSISTENCE AND LOW-FREQUENCY AMPLITUDE (23-YEAR WINDOWS)
# ============================================================================
# Compute persistence metrics (AR(1), P_LF) for GRACE and model ensembles
# using 23-year windows on filtered data (NO additional Butterworth filtering)
# Author: Ashraf Rateb
# Date: 2025-11-22
# ============================================================================

library(tidyverse)
library(data.table)
library(parallel)
library(foreach)
library(doParallel)

# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Setup parallel processing
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

cat("============================================================================\n")
cat("PHASE 05: PERSISTENCE AND LOW-FREQUENCY AMPLITUDE (23-YEAR WINDOWS)\n")
cat("============================================================================\n\n")

cat("Using", n_cores, "cores for parallel processing\n\n")

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading data...\n")

# Load FILTERED data from Phase 1 (already deforced + STL filtered)
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
cat("  Basins:", nrow(attrs), "\n\n")

cat("✓ Using filtered data from Phase 1 (already deforced + STL filtered)\n")
cat("✓ NO Butterworth filtering - data already filtered\n\n")

# ============================================================================
# WINDOW CONFIGURATION (same as Phase 03)
# ============================================================================

WINDOW_SIZE <- 273  # 273 months (22.75 years - GRACE data length)
OVERLAP <- 0        # No overlap

cat("Window configuration:\n")
cat("  Window size: 273 months (22.75 years - GRACE data length)\n")
cat("  Overlap: 0 months (non-overlapping)\n\n")

# ============================================================================
# PERSISTENCE FUNCTIONS
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

#' Compute AR(1) coefficient and P_LF for a time series window
#'
#' @param x Numeric vector (detrended, filtered time series)
#' @return List with ar1, p_lf, variance
#'
compute_persistence_window <- function(x) {

  # Remove NA values
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)

  # Check minimum length
  if (n < 120) {  # Require at least 10 years
    return(list(
      ar1 = NA_real_,
      p_lf = NA_real_,
      variance = NA_real_,
      n_obs = n,
      success = FALSE
    ))
  }

  # Compute variance
  var_x <- var(x_clean, na.rm = TRUE)

  # Compute AR(1) coefficient using lag-1 autocorrelation
  # AR(1): x_t = ar1 * x_{t-1} + epsilon_t
  ar1 <- tryCatch({
    # Use acf for lag-1 autocorrelation
    acf_result <- acf(x_clean, lag.max = 1, plot = FALSE, na.action = na.pass)
    acf_result$acf[2]  # lag-1 autocorrelation
  }, error = function(e) NA_real_)

  # Compute P_LF: low-frequency amplitude
  # Since data is already filtered, compute amplitude directly
  # P_LF = standard deviation of the filtered signal (represents LF amplitude)
  p_lf <- sd(x_clean, na.rm = TRUE)

  list(
    ar1 = ar1,
    p_lf = p_lf,
    variance = var_x,
    n_obs = n,
    success = !is.na(ar1) && !is.na(p_lf)
  )
}

# Export functions to cluster
clusterExport(cl, c("compute_persistence_window", "compute_regime_persistence", "classify_memory_timescale"))

# ============================================================================
# GRACE PERSISTENCE (single 23-year window)
# ============================================================================

cat("Computing GRACE persistence metrics...\n")

grace_persistence_list <- list()

for (i in 1:nrow(attrs)) {
  basin_id <- attrs$ID[i]
  basin_name <- attrs$name[i]

  # Extract GRACE time series (filtered)
  grace_series <- gfo_dtrend$median[[basin_name]]

  # Check if we have at least 23 years
  if (length(grace_series) >= WINDOW_SIZE) {
    # Use the full 23-year window (or first 23 years if longer)
    window_data <- grace_series[1:WINDOW_SIZE]

    pm <- compute_persistence_window(window_data)

    grace_persistence_list[[i]] <- data.table(
      basin = i,
      basin_id = basin_id,
      basin_name = basin_name,
      ar1_grace = pm$ar1,
      p_lf_grace = pm$p_lf,
      variance_grace = pm$variance,
      n_obs_grace = pm$n_obs,
      success = pm$success
    )
  } else {
    # Not enough data
    grace_persistence_list[[i]] <- data.table(
      basin = i,
      basin_id = basin_id,
      basin_name = basin_name,
      ar1_grace = NA_real_,
      p_lf_grace = NA_real_,
      variance_grace = NA_real_,
      n_obs_grace = length(grace_series),
      success = FALSE
    )
  }
}

grace_persistence <- rbindlist(grace_persistence_list)

# Compute memory timescale for GRACE
grace_persistence[, tau_grace := ifelse(ar1_grace > 0 & ar1_grace < 1,
                                         -1 / log(ar1_grace),
                                         NA_real_)]

# Classify memory timescale
grace_persistence[, tau_class_grace := sapply(tau_grace, classify_memory_timescale)]

cat("✓ GRACE persistence computed for", sum(grace_persistence$success), "/", nrow(grace_persistence), "basins\n")
cat("✓ Memory timescales computed and classified\n\n")

# ============================================================================
# GRACE REGIME PERSISTENCE (positive/negative anomalies)
# ============================================================================

cat("Computing GRACE regime persistence (pluvial/drought regimes)...\n")

grace_regime_list <- list()

for (i in 1:nrow(attrs)) {
  basin_id <- attrs$ID[i]
  basin_name <- attrs$name[i]

  # Extract GRACE time series (filtered)
  grace_series <- gfo_dtrend$median[[basin_name]]

  if (length(grace_series) >= WINDOW_SIZE) {
    # Use the full 23-year window
    window_data <- grace_series[1:WINDOW_SIZE]

    rm <- compute_regime_persistence(window_data)

    grace_regime_list[[i]] <- data.table(
      basin = i,
      basin_id = basin_id,
      basin_name = basin_name,
      pluvial_ar1_grace = rm$pluvial_ar1,
      pluvial_p_lf_grace = rm$pluvial_p_lf,
      pluvial_tau_grace = rm$pluvial_tau,
      pluvial_tau_class_grace = rm$pluvial_tau_class,
      pluvial_n_obs_grace = rm$pluvial_n_obs,
      drought_ar1_grace = rm$drought_ar1,
      drought_p_lf_grace = rm$drought_p_lf,
      drought_tau_grace = rm$drought_tau,
      drought_tau_class_grace = rm$drought_tau_class,
      drought_n_obs_grace = rm$drought_n_obs
    )
  } else {
    # Not enough data
    grace_regime_list[[i]] <- data.table(
      basin = i,
      basin_id = basin_id,
      basin_name = basin_name,
      pluvial_ar1_grace = NA_real_,
      pluvial_p_lf_grace = NA_real_,
      pluvial_tau_grace = NA_real_,
      pluvial_tau_class_grace = NA_character_,
      pluvial_n_obs_grace = 0L,
      drought_ar1_grace = NA_real_,
      drought_p_lf_grace = NA_real_,
      drought_tau_grace = NA_real_,
      drought_tau_class_grace = NA_character_,
      drought_n_obs_grace = 0L
    )
  }
}

grace_regime_persistence <- rbindlist(grace_regime_list)

cat("✓ GRACE regime persistence computed\n")
cat("  Basins with pluvial metrics:", sum(!is.na(grace_regime_persistence$pluvial_ar1_grace)), "/", nrow(grace_regime_persistence), "\n")
cat("  Basins with drought metrics:", sum(!is.na(grace_regime_persistence$drought_ar1_grace)), "/", nrow(grace_regime_persistence), "\n\n")

# ============================================================================
# CESM2 PERSISTENCE (23-year windows across all members)
# ============================================================================

cat("============================================================================\n")
cat("CESM2 PERSISTENCE ANALYSIS (23-YEAR WINDOWS)\n")
cat("============================================================================\n\n")

n_basins <- dim(G_CESM_filtered)[1]
n_members_cesm <- dim(G_CESM_filtered)[2]
n_times_cesm <- dim(G_CESM_filtered)[3]

# Calculate number of windows per member
n_windows_per_member <- floor((n_times_cesm - WINDOW_SIZE) / (WINDOW_SIZE - OVERLAP)) + 1
total_windows_cesm <- n_basins * n_members_cesm * n_windows_per_member

cat("  Basins:", n_basins, "\n")
cat("  Members:", n_members_cesm, "\n")
cat("  Time steps:", n_times_cesm, "months (", n_times_cesm/12, "years)\n")
cat("  Windows per member:", n_windows_per_member, "\n")
cat("  Total windows:", total_windows_cesm, "\n\n")

cat("Processing basins in parallel...\n")

results_cesm <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  tryCatch({
    # Skip if GRACE persistence failed
    if (!grace_persistence$success[basin]) {
      return(NULL)
    }

    # Extract basin data [members × time]
    basin_data <- G_CESM_filtered[basin, , ]

    all_persistence_list <- list()

    for (member in 1:n_members_cesm) {
      member_series <- basin_data[member, ]

      # Create windows
      for (w in 1:n_windows_per_member) {
        start_idx <- (w - 1) * (WINDOW_SIZE - OVERLAP) + 1
        end_idx <- start_idx + WINDOW_SIZE - 1

        if (end_idx <= length(member_series)) {
          window_data <- member_series[start_idx:end_idx]

          pm <- compute_persistence_window(window_data)

          if (pm$success) {
            all_persistence_list[[length(all_persistence_list) + 1]] <- data.table(
              basin = basin,
              member = member,
              window = w,
              ar1 = pm$ar1,
              p_lf = pm$p_lf,
              variance = pm$variance,
              n_obs = pm$n_obs
            )
          }
        }
      }
    }

    if (length(all_persistence_list) > 0) {
      rbindlist(all_persistence_list)
    } else {
      NULL
    }

  }, error = function(e) {
    NULL
  })
}

# Combine results
cesm_persistence_list <- results_cesm[!sapply(results_cesm, is.null)]

if (length(cesm_persistence_list) == 0) {
  stop("No CESM2 persistence windows generated.")
}

cesm_persistence <- rbindlist(cesm_persistence_list, fill = TRUE)

# Compute memory timescale for all CESM2 windows
cesm_persistence[, tau := ifelse(ar1 > 0 & ar1 < 1,
                                  -1 / log(ar1),
                                  NA_real_)]

# Classify memory timescale
cesm_persistence[, tau_class := sapply(tau, classify_memory_timescale)]

cat("\n✓ CESM2 persistence computed\n")
cat("  Total windows:", nrow(cesm_persistence), "\n")
cat("  Basins:", length(unique(cesm_persistence$basin)), "\n")
cat("  Members:", length(unique(cesm_persistence$member)), "\n")
cat("✓ Memory timescales computed and classified for all windows\n\n")

# ============================================================================
# CESM2 REGIME PERSISTENCE (positive/negative anomalies in windows)
# ============================================================================

cat("Computing CESM2 regime persistence (pluvial/drought regimes)...\n")
cat("  Processing basins in parallel...\n\n")

results_cesm_regime <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  tryCatch({
    # Skip if GRACE persistence failed
    if (!grace_persistence$success[basin]) {
      return(NULL)
    }

    # Extract basin data [members × time]
    basin_data <- G_CESM_filtered[basin, , ]

    all_regime_list <- list()

    for (member in 1:n_members_cesm) {
      member_series <- basin_data[member, ]

      # Create windows
      for (w in 1:n_windows_per_member) {
        start_idx <- (w - 1) * (WINDOW_SIZE - OVERLAP) + 1
        end_idx <- start_idx + WINDOW_SIZE - 1

        if (end_idx <= length(member_series)) {
          window_data <- member_series[start_idx:end_idx]

          rm <- compute_regime_persistence(window_data)

          if (rm$success) {
            all_regime_list[[length(all_regime_list) + 1]] <- data.table(
              basin = basin,
              member = member,
              window = w,
              pluvial_ar1 = rm$pluvial_ar1,
              pluvial_p_lf = rm$pluvial_p_lf,
              pluvial_tau = rm$pluvial_tau,
              pluvial_tau_class = rm$pluvial_tau_class,
              pluvial_n_obs = rm$pluvial_n_obs,
              drought_ar1 = rm$drought_ar1,
              drought_p_lf = rm$drought_p_lf,
              drought_tau = rm$drought_tau,
              drought_tau_class = rm$drought_tau_class,
              drought_n_obs = rm$drought_n_obs
            )
          }
        }
      }
    }

    if (length(all_regime_list) > 0) {
      rbindlist(all_regime_list)
    } else {
      NULL
    }

  }, error = function(e) {
    NULL
  })
}

# Combine results
cesm_regime_list <- results_cesm_regime[!sapply(results_cesm_regime, is.null)]

if (length(cesm_regime_list) > 0) {
  cesm_regime_persistence <- rbindlist(cesm_regime_list, fill = TRUE)

  cat("\n✓ CESM2 regime persistence computed\n")
  cat("  Total windows with regime metrics:", nrow(cesm_regime_persistence), "\n")
  cat("  Windows with pluvial regime:", sum(!is.na(cesm_regime_persistence$pluvial_ar1)), "\n")
  cat("  Windows with drought regime:", sum(!is.na(cesm_regime_persistence$drought_ar1)), "\n\n")
} else {
  cat("\n! No CESM2 regime persistence windows generated\n\n")
  cesm_regime_persistence <- data.table()
}

# ============================================================================
# COMPUTE GRACE POSITION IN CESM2 DISTRIBUTION
# ============================================================================

cat("Computing GRACE position within CESM2 distributions...\n")

# For each basin, compute distribution statistics
cesm_distributions <- cesm_persistence[,
  .(ar1_p05_cesm = quantile(ar1, 0.05, na.rm = TRUE),
    ar1_p50_cesm = quantile(ar1, 0.50, na.rm = TRUE),
    ar1_p95_cesm = quantile(ar1, 0.95, na.rm = TRUE),
    ar1_mean_cesm = mean(ar1, na.rm = TRUE),
    ar1_sd_cesm = sd(ar1, na.rm = TRUE),

    p_lf_p05_cesm = quantile(p_lf, 0.05, na.rm = TRUE),
    p_lf_p50_cesm = quantile(p_lf, 0.50, na.rm = TRUE),
    p_lf_p95_cesm = quantile(p_lf, 0.95, na.rm = TRUE),
    p_lf_mean_cesm = mean(p_lf, na.rm = TRUE),
    p_lf_sd_cesm = sd(p_lf, na.rm = TRUE),

    tau_p05_cesm = quantile(tau, 0.05, na.rm = TRUE),
    tau_p50_cesm = quantile(tau, 0.50, na.rm = TRUE),
    tau_p95_cesm = quantile(tau, 0.95, na.rm = TRUE),
    tau_mean_cesm = mean(tau, na.rm = TRUE),
    tau_sd_cesm = sd(tau, na.rm = TRUE),

    n_windows_cesm = .N),
  by = basin]

# Merge GRACE values and compute percentiles
persistence_summary_cesm <- merge(
  grace_persistence,
  cesm_distributions,
  by = "basin",
  all.x = TRUE
)

# Compute percentiles
persistence_summary_cesm <- persistence_summary_cesm[, .(
  basin, basin_id, basin_name,
  ar1_grace, p_lf_grace, variance_grace, n_obs_grace,
  ar1_p05_cesm, ar1_p50_cesm, ar1_p95_cesm, ar1_mean_cesm, ar1_sd_cesm,
  p_lf_p05_cesm, p_lf_p50_cesm, p_lf_p95_cesm, p_lf_mean_cesm, p_lf_sd_cesm,
  n_windows_cesm
)]

# Compute percentiles by comparing GRACE with window distribution
for (i in 1:nrow(persistence_summary_cesm)) {
  b <- persistence_summary_cesm$basin[i]

  if (!is.na(persistence_summary_cesm$ar1_grace[i])) {
    basin_windows <- cesm_persistence[basin == b]

    persistence_summary_cesm$ar1_percentile_cesm[i] <- mean(
      basin_windows$ar1 < persistence_summary_cesm$ar1_grace[i],
      na.rm = TRUE
    )

    persistence_summary_cesm$p_lf_percentile_cesm[i] <- mean(
      basin_windows$p_lf < persistence_summary_cesm$p_lf_grace[i],
      na.rm = TRUE
    )
  }
}

# Compute memory timescales (e-folding time) for GRACE and CESM2
# τ = -1 / log(AR1) in months
persistence_summary_cesm[, tau_grace := ifelse(ar1_grace > 0 & ar1_grace < 1,
                                                -1 / log(ar1_grace),
                                                NA_real_)]
persistence_summary_cesm[, tau_p05_cesm := ifelse(ar1_p05_cesm > 0 & ar1_p05_cesm < 1,
                                                   -1 / log(ar1_p05_cesm),
                                                   NA_real_)]
persistence_summary_cesm[, tau_p50_cesm := ifelse(ar1_p50_cesm > 0 & ar1_p50_cesm < 1,
                                                   -1 / log(ar1_p50_cesm),
                                                   NA_real_)]
persistence_summary_cesm[, tau_p95_cesm := ifelse(ar1_p95_cesm > 0 & ar1_p95_cesm < 1,
                                                   -1 / log(ar1_p95_cesm),
                                                   NA_real_)]
persistence_summary_cesm[, tau_mean_cesm := ifelse(ar1_mean_cesm > 0 & ar1_mean_cesm < 1,
                                                    -1 / log(ar1_mean_cesm),
                                                    NA_real_)]

# Classify memory timescales
persistence_summary_cesm[, tau_class_grace := sapply(tau_grace, classify_memory_timescale)]
persistence_summary_cesm[, tau_class_p50_cesm := sapply(tau_p50_cesm, classify_memory_timescale)]

cat("✓ CESM2 distributions computed for", nrow(persistence_summary_cesm), "basins\n")
cat("✓ Memory timescales computed and classified (e-folding time in months)\n\n")

# ============================================================================
# IPSL PERSISTENCE (23-year windows across all members)
# ============================================================================

cat("============================================================================\n")
cat("IPSL PERSISTENCE ANALYSIS (23-YEAR WINDOWS)\n")
cat("============================================================================\n\n")

n_members_ipsl <- dim(G_IPSL_combined)[2]
n_times_ipsl <- dim(G_IPSL_combined)[3]

n_windows_per_member_ipsl <- floor((n_times_ipsl - WINDOW_SIZE) / (WINDOW_SIZE - OVERLAP)) + 1
total_windows_ipsl <- n_basins * n_members_ipsl * n_windows_per_member_ipsl

cat("  Basins:", n_basins, "\n")
cat("  Members:", n_members_ipsl, "\n")
cat("  Time steps:", n_times_ipsl, "months (", n_times_ipsl/12, "years)\n")
cat("  Windows per member:", n_windows_per_member_ipsl, "\n")
cat("  Total windows:", total_windows_ipsl, "\n\n")

cat("Processing basins in parallel...\n")

results_ipsl <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  tryCatch({
    if (!grace_persistence$success[basin]) {
      return(NULL)
    }

    basin_data <- G_IPSL_combined[basin, , ]
    all_persistence_list <- list()

    for (member in 1:n_members_ipsl) {
      member_series <- basin_data[member, ]

      for (w in 1:n_windows_per_member_ipsl) {
        start_idx <- (w - 1) * (WINDOW_SIZE - OVERLAP) + 1
        end_idx <- start_idx + WINDOW_SIZE - 1

        if (end_idx <= length(member_series)) {
          window_data <- member_series[start_idx:end_idx]

          pm <- compute_persistence_window(window_data)

          if (pm$success) {
            all_persistence_list[[length(all_persistence_list) + 1]] <- data.table(
              basin = basin,
              member = member,
              window = w,
              ar1 = pm$ar1,
              p_lf = pm$p_lf,
              variance = pm$variance,
              n_obs = pm$n_obs
            )
          }
        }
      }
    }

    if (length(all_persistence_list) > 0) {
      rbindlist(all_persistence_list)
    } else {
      NULL
    }

  }, error = function(e) {
    NULL
  })
}

# Combine results
ipsl_persistence_list <- results_ipsl[!sapply(results_ipsl, is.null)]

if (length(ipsl_persistence_list) == 0) {
  stop("No IPSL persistence windows generated.")
}

ipsl_persistence <- rbindlist(ipsl_persistence_list, fill = TRUE)

# Compute memory timescale for all IPSL windows
ipsl_persistence[, tau := ifelse(ar1 > 0 & ar1 < 1,
                                  -1 / log(ar1),
                                  NA_real_)]

# Classify memory timescale
ipsl_persistence[, tau_class := sapply(tau, classify_memory_timescale)]

cat("\n✓ IPSL persistence computed\n")
cat("  Total windows:", nrow(ipsl_persistence), "\n")
cat("  Basins:", length(unique(ipsl_persistence$basin)), "\n")
cat("  Members:", length(unique(ipsl_persistence$member)), "\n")
cat("✓ Memory timescales computed and classified for all windows\n\n")

# ============================================================================
# IPSL REGIME PERSISTENCE (positive/negative anomalies in windows)
# ============================================================================

cat("Computing IPSL regime persistence (pluvial/drought regimes)...\n")
cat("  Processing basins in parallel...\n\n")

results_ipsl_regime <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  tryCatch({
    # Skip if GRACE persistence failed
    if (!grace_persistence$success[basin]) {
      return(NULL)
    }

    # Extract basin data [members × time]
    basin_data <- G_IPSL_combined[basin, , ]

    all_regime_list <- list()

    for (member in 1:n_members_ipsl) {
      member_series <- basin_data[member, ]

      # Create windows
      for (w in 1:n_windows_per_member_ipsl) {
        start_idx <- (w - 1) * (WINDOW_SIZE - OVERLAP) + 1
        end_idx <- start_idx + WINDOW_SIZE - 1

        if (end_idx <= length(member_series)) {
          window_data <- member_series[start_idx:end_idx]

          rm <- compute_regime_persistence(window_data)

          if (rm$success) {
            all_regime_list[[length(all_regime_list) + 1]] <- data.table(
              basin = basin,
              member = member,
              window = w,
              pluvial_ar1 = rm$pluvial_ar1,
              pluvial_p_lf = rm$pluvial_p_lf,
              pluvial_tau = rm$pluvial_tau,
              pluvial_tau_class = rm$pluvial_tau_class,
              pluvial_n_obs = rm$pluvial_n_obs,
              drought_ar1 = rm$drought_ar1,
              drought_p_lf = rm$drought_p_lf,
              drought_tau = rm$drought_tau,
              drought_tau_class = rm$drought_tau_class,
              drought_n_obs = rm$drought_n_obs
            )
          }
        }
      }
    }

    if (length(all_regime_list) > 0) {
      rbindlist(all_regime_list)
    } else {
      NULL
    }

  }, error = function(e) {
    NULL
  })
}

# Combine results
ipsl_regime_list <- results_ipsl_regime[!sapply(results_ipsl_regime, is.null)]

if (length(ipsl_regime_list) > 0) {
  ipsl_regime_persistence <- rbindlist(ipsl_regime_list, fill = TRUE)

  cat("\n✓ IPSL regime persistence computed\n")
  cat("  Total windows with regime metrics:", nrow(ipsl_regime_persistence), "\n")
  cat("  Windows with pluvial regime:", sum(!is.na(ipsl_regime_persistence$pluvial_ar1)), "\n")
  cat("  Windows with drought regime:", sum(!is.na(ipsl_regime_persistence$drought_ar1)), "\n\n")
} else {
  cat("\n! No IPSL regime persistence windows generated\n\n")
  ipsl_regime_persistence <- data.table()
}

# ============================================================================
# COMPUTE GRACE POSITION IN IPSL DISTRIBUTION
# ============================================================================

cat("Computing GRACE position within IPSL distributions...\n")

ipsl_distributions <- ipsl_persistence[,
  .(ar1_p05_ipsl = quantile(ar1, 0.05, na.rm = TRUE),
    ar1_p50_ipsl = quantile(ar1, 0.50, na.rm = TRUE),
    ar1_p95_ipsl = quantile(ar1, 0.95, na.rm = TRUE),
    ar1_mean_ipsl = mean(ar1, na.rm = TRUE),
    ar1_sd_ipsl = sd(ar1, na.rm = TRUE),

    p_lf_p05_ipsl = quantile(p_lf, 0.05, na.rm = TRUE),
    p_lf_p50_ipsl = quantile(p_lf, 0.50, na.rm = TRUE),
    p_lf_p95_ipsl = quantile(p_lf, 0.95, na.rm = TRUE),
    p_lf_mean_ipsl = mean(p_lf, na.rm = TRUE),
    p_lf_sd_ipsl = sd(p_lf, na.rm = TRUE),

    tau_p05_ipsl = quantile(tau, 0.05, na.rm = TRUE),
    tau_p50_ipsl = quantile(tau, 0.50, na.rm = TRUE),
    tau_p95_ipsl = quantile(tau, 0.95, na.rm = TRUE),
    tau_mean_ipsl = mean(tau, na.rm = TRUE),
    tau_sd_ipsl = sd(tau, na.rm = TRUE),

    n_windows_ipsl = .N),
  by = basin]

# Merge with CESM summary
persistence_summary <- merge(
  persistence_summary_cesm,
  ipsl_distributions,
  by = "basin",
  all = TRUE
)

# Compute IPSL percentiles
for (i in 1:nrow(persistence_summary)) {
  b <- persistence_summary$basin[i]

  if (!is.na(persistence_summary$ar1_grace[i])) {
    basin_windows <- ipsl_persistence[basin == b]

    if (nrow(basin_windows) > 0) {
      persistence_summary$ar1_percentile_ipsl[i] <- mean(
        basin_windows$ar1 < persistence_summary$ar1_grace[i],
        na.rm = TRUE
      )

      persistence_summary$p_lf_percentile_ipsl[i] <- mean(
        basin_windows$p_lf < persistence_summary$p_lf_grace[i],
        na.rm = TRUE
      )
    }
  }
}

# Compute memory timescales (e-folding time) for IPSL
# τ = -1 / log(AR1) in months
persistence_summary[, tau_p05_ipsl := ifelse(ar1_p05_ipsl > 0 & ar1_p05_ipsl < 1,
                                              -1 / log(ar1_p05_ipsl),
                                              NA_real_)]
persistence_summary[, tau_p50_ipsl := ifelse(ar1_p50_ipsl > 0 & ar1_p50_ipsl < 1,
                                              -1 / log(ar1_p50_ipsl),
                                              NA_real_)]
persistence_summary[, tau_p95_ipsl := ifelse(ar1_p95_ipsl > 0 & ar1_p95_ipsl < 1,
                                              -1 / log(ar1_p95_ipsl),
                                              NA_real_)]
persistence_summary[, tau_mean_ipsl := ifelse(ar1_mean_ipsl > 0 & ar1_mean_ipsl < 1,
                                               -1 / log(ar1_mean_ipsl),
                                               NA_real_)]

# Classify memory timescales for IPSL
persistence_summary[, tau_class_p50_ipsl := sapply(tau_p50_ipsl, classify_memory_timescale)]

# Merge basin attributes
attrs_dt <- data.table(attrs)
attrs_dt[, basin_id := ID]
persistence_summary <- merge(persistence_summary, attrs_dt, by = "basin_id", all.x = TRUE, suffixes = c("", "_attr"))

cat("✓ IPSL distributions computed\n")
cat("✓ Memory timescales computed and classified for both models\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

# Basin-level summary (like Phase 03 dispersion_summary)
saveRDS(persistence_summary, "outputs/phase05_persistence_summary.rds")
fwrite(persistence_summary, "outputs/phase05_persistence_summary.csv")

# Window-level data
saveRDS(cesm_persistence, "outputs/phase05_cesm_persistence.rds")
fwrite(cesm_persistence, "outputs/phase05_cesm_persistence.csv")

saveRDS(ipsl_persistence, "outputs/phase05_ipsl_persistence.rds")
fwrite(ipsl_persistence, "outputs/phase05_ipsl_persistence.csv")

# GRACE persistence
saveRDS(grace_persistence, "outputs/phase05_grace_persistence.rds")
fwrite(grace_persistence, "outputs/phase05_grace_persistence.csv")

# Regime persistence (pluvial/drought)
saveRDS(grace_regime_persistence, "outputs/phase05_grace_regime_persistence.rds")
fwrite(grace_regime_persistence, "outputs/phase05_grace_regime_persistence.csv")

if (nrow(cesm_regime_persistence) > 0) {
  saveRDS(cesm_regime_persistence, "outputs/phase05_cesm_regime_persistence.rds")
  fwrite(cesm_regime_persistence, "outputs/phase05_cesm_regime_persistence.csv")
}

if (nrow(ipsl_regime_persistence) > 0) {
  saveRDS(ipsl_regime_persistence, "outputs/phase05_ipsl_regime_persistence.rds")
  fwrite(ipsl_regime_persistence, "outputs/phase05_ipsl_regime_persistence.csv")
}

cat("  ✓ outputs/phase05_persistence_summary.{rds,csv} (basin-level)\n")
cat("  ✓ outputs/phase05_grace_persistence.{rds,csv}\n")
cat("  ✓ outputs/phase05_cesm_persistence.{rds,csv} (window-level)\n")
cat("  ✓ outputs/phase05_ipsl_persistence.{rds,csv} (window-level)\n")
cat("  ✓ outputs/phase05_grace_regime_persistence.{rds,csv} (pluvial/drought regimes)\n")
if (nrow(cesm_regime_persistence) > 0) {
  cat("  ✓ outputs/phase05_cesm_regime_persistence.{rds,csv} (regime window-level)\n")
}
if (nrow(ipsl_regime_persistence) > 0) {
  cat("  ✓ outputs/phase05_ipsl_regime_persistence.{rds,csv} (regime window-level)\n")
}
cat("\n")

# Cleanup
stopCluster(cl)

cat("============================================================================\n")
cat("PHASE 05 COMPLETE\n")
cat("============================================================================\n\n")
