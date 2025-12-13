#!/usr/bin/env Rscript
# ============================================================================
# PHASE 05: PERSISTENCE AND LOW-FREQUENCY AMPLITUDE
# ============================================================================
# Compute persistence metrics (autocorrelation time τ) and low-frequency
# amplitude A_LF for GRACE and model ensembles using filtered data
# Author: Ashraf Rateb
# Date: 2025-11-19
# ============================================================================

library(tidyverse)
library(data.table)
library(parallel)
library(foreach)
library(doParallel)

# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

cat("============================================================================\n")
cat("PHASE 05: PERSISTENCE AND LOW-FREQUENCY AMPLITUDE\n")
cat("============================================================================\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Maximum lag for autocorrelation (months)
# For monthly data: 60 months = 5 years
MAX_LAG_MONTHS <- 60

# Minimum length requirement for stable ACF estimation
MIN_LENGTH_MONTHS <- 120  # 10 years minimum

# Setup parallel processing
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

cat("Configuration:\n")
cat("  Max lag for autocorrelation:", MAX_LAG_MONTHS, "months (", MAX_LAG_MONTHS/12, "years)\n")
cat("  Minimum series length:", MIN_LENGTH_MONTHS, "months (", MIN_LENGTH_MONTHS/12, "years)\n")
cat("  Using", n_cores, "cores for parallel processing\n\n")

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
cat("  Basins:", nrow(attrs), "\n\n")

# ============================================================================
# PERSISTENCE FUNCTIONS
# ============================================================================

#' Compute persistence metrics for a time series
#'
#' @param x Numeric vector (time series)
#' @param max_lag Maximum lag for autocorrelation
#' @param min_length Minimum required length
#' @return List with rho_k, tau, A_LF, effective_length, and additional metrics
#'
compute_persistence_metrics <- function(x, max_lag = 60, min_length = 120) {

  # Remove NA values
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)

  # Check minimum length
  if (n < min_length || n < max_lag + 10) {
    return(list(
      rho_k = rep(NA_real_, max_lag),
      tau = NA_real_,
      tau_robust = NA_real_,
      A_LF = NA_real_,
      variance = NA_real_,
      effective_n = NA_real_,
      lag1_acf = NA_real_,
      decorrelation_time = NA_real_,
      success = FALSE
    ))
  }

  # Check for zero variance
  if (sd(x_clean) < 1e-10) {
    return(list(
      rho_k = rep(0, max_lag),
      tau = 1.0,
      tau_robust = 1.0,
      A_LF = 0.0,
      variance = 0.0,
      effective_n = n,
      lag1_acf = 0.0,
      decorrelation_time = 1.0,
      success = TRUE
    ))
  }

  # Compute autocorrelation function
  acf_result <- tryCatch({
    acf(x_clean, lag.max = max_lag, type = "correlation", plot = FALSE, na.action = na.pass)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(acf_result)) {
    return(list(
      rho_k = rep(NA_real_, max_lag),
      tau = NA_real_,
      tau_robust = NA_real_,
      A_LF = NA_real_,
      variance = NA_real_,
      effective_n = NA_real_,
      lag1_acf = NA_real_,
      decorrelation_time = NA_real_,
      success = FALSE
    ))
  }

  # Extract autocorrelations (excluding lag 0)
  rho_k <- as.numeric(acf_result$acf[-1, 1, 1])

  # Integrated autocorrelation time
  # τ = 1 + 2 * Σ_{k=1}^{K_max} ρ(k)
  tau <- 1 + 2 * sum(rho_k, na.rm = TRUE)

  # Robust tau: truncate sum when ACF becomes insignificant or negative
  # Find first crossing of 0 or use max_lag
  rho_positive <- rho_k > 0
  if (any(!rho_positive)) {
    cutoff <- which(!rho_positive)[1] - 1
    if (cutoff > 0) {
      tau_robust <- 1 + 2 * sum(rho_k[1:cutoff], na.rm = TRUE)
    } else {
      tau_robust <- 1.0
    }
  } else {
    tau_robust <- tau
  }

  # Lag-1 autocorrelation (important for AR(1) processes)
  lag1_acf <- rho_k[1]

  # Decorrelation time: e-folding scale (lag where ACF drops to 1/e)
  # Approximate: τ_decorr ≈ -1/log(|ρ_1|) for AR(1)
  if (!is.na(lag1_acf) && abs(lag1_acf) > 0.01) {
    decorrelation_time <- -1 / log(abs(lag1_acf))
  } else {
    decorrelation_time <- 1.0
  }

  # Effective sample size accounting for autocorrelation
  # N_eff ≈ N / τ
  effective_n <- n / max(tau_robust, 1.0)

  # Low-frequency amplitude (standard deviation)
  A_LF <- sd(x_clean, na.rm = TRUE)
  variance <- var(x_clean, na.rm = TRUE)

  return(list(
    rho_k = rho_k,
    tau = tau,
    tau_robust = tau_robust,
    A_LF = A_LF,
    variance = variance,
    effective_n = effective_n,
    lag1_acf = lag1_acf,
    decorrelation_time = decorrelation_time,
    success = TRUE
  ))
}

# ============================================================================
# GRACE PERSISTENCE METRICS
# ============================================================================

cat("Computing GRACE persistence metrics...\n")

grace_persistence_list <- list()

pb_grace <- txtProgressBar(min = 0, max = nrow(attrs), style = 3)

for (i in 1:nrow(attrs)) {
  basin_id <- attrs$ID[i]
  basin_name <- attrs$name[i]

  # Extract GRACE time series (median)
  grace_series <- gfo_dtrend$median[[basin_name]]

  # Compute persistence metrics
  pm <- compute_persistence_metrics(grace_series, max_lag = MAX_LAG_MONTHS)

  grace_persistence_list[[i]] <- data.table(
    basin = i,
    basin_id = basin_id,
    basin_name = basin_name,
    tau_grace = pm$tau,
    tau_robust_grace = pm$tau_robust,
    A_LF_grace = pm$A_LF,
    variance_grace = pm$variance,
    lag1_acf_grace = pm$lag1_acf,
    decorrelation_time_grace = pm$decorrelation_time,
    effective_n_grace = pm$effective_n,
    success = pm$success
  )

  setTxtProgressBar(pb_grace, i)
}

close(pb_grace)

grace_persistence <- rbindlist(grace_persistence_list)

cat("\n✓ GRACE persistence analysis complete\n")
cat("  Successful analyses:", sum(grace_persistence$success), "/", nrow(grace_persistence), "\n\n")

# Export functions to cluster (once for both CESM2 and IPSL)
clusterExport(cl, c("compute_persistence_metrics", "MAX_LAG_MONTHS", "MIN_LENGTH_MONTHS"))

# ============================================================================
# CESM2 PERSISTENCE METRICS (PER MEMBER) WITH 23-YEAR WINDOWING
# ============================================================================

cat("============================================================================\n")
cat("CESM2 PERSISTENCE METRICS\n")
cat("============================================================================\n\n")

# Use 273-month windows to match GRACE observation period
WINDOW_LENGTH <- 273  # months (23 years, matching GRACE)

cat("Analyzing 23-year windows (273 months) to match GRACE observation period...\n")
cat("  80 members × 184 basins × ~1900 windows per member\n")
cat("  Processing basins in parallel...\n\n")

n_basins <- dim(G_CESM_filtered)[1]
n_members_cesm <- dim(G_CESM_filtered)[2]
n_times_cesm <- dim(G_CESM_filtered)[3]

# Export window length to cluster
clusterExport(cl, c("WINDOW_LENGTH"))

# Parallel processing across basins (variables exported via clusterExport above)
cesm_results <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  basin_list <- list()

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

    # Extract all possible 273-month windows
    for (w in 1:max_start) {
      start_idx <- valid_idx[w]
      end_idx <- valid_idx[w + WINDOW_LENGTH - 1]

      # Extract window data
      window_data <- member_series[start_idx:end_idx]

      # Compute persistence metrics on this window
      pm <- compute_persistence_metrics(window_data, max_lag = MAX_LAG_MONTHS, min_length = MIN_LENGTH_MONTHS)

      basin_list[[length(basin_list) + 1]] <- data.table(
        basin = basin,
        basin_id = attrs$ID[basin],
        basin_name = attrs$name[basin],
        member = member,
        window = w,
        tau = pm$tau,
        tau_robust = pm$tau_robust,
        A_LF = pm$A_LF,
        variance = pm$variance,
        lag1_acf = pm$lag1_acf,
        decorrelation_time = pm$decorrelation_time,
        effective_n = pm$effective_n,
        success = pm$success
      )
    }
  }

  rbindlist(basin_list)
}

cesm_persistence <- rbindlist(cesm_results)

# Attach basin attributes
attrs_dt <- data.table(attrs)
attrs_dt[, basin_id := ID]

cesm_persistence <- merge(cesm_persistence, attrs_dt, by = "basin_id", all.x = TRUE, suffixes = c("", "_attr"))

cat("\n✓ CESM2 persistence analysis complete\n")
cat("  Total records:", nrow(cesm_persistence), "\n")
cat("  Successful analyses:", sum(cesm_persistence$success, na.rm = TRUE), "\n\n")

# ============================================================================
# IPSL PERSISTENCE METRICS (PER MEMBER) WITH 23-YEAR WINDOWING
# ============================================================================

cat("============================================================================\n")
cat("IPSL PERSISTENCE METRICS\n")
cat("============================================================================\n\n")

cat("Analyzing 23-year windows (273 months) to match GRACE observation period...\n")
cat("  18 members × 184 basins × ~1200 windows per member\n")
cat("  Processing basins in parallel...\n\n")

n_members_ipsl <- dim(G_IPSL_combined)[2]
n_times_ipsl <- dim(G_IPSL_combined)[3]

# Parallel processing across basins (variables exported via clusterExport above)
ipsl_results <- foreach(basin = 1:n_basins, .packages = c("data.table")) %dopar% {

  basin_list <- list()

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

    # Extract all possible 273-month windows
    for (w in 1:max_start) {
      start_idx <- valid_idx[w]
      end_idx <- valid_idx[w + WINDOW_LENGTH - 1]

      # Extract window data
      window_data <- member_series[start_idx:end_idx]

      # Compute persistence metrics on this window
      pm <- compute_persistence_metrics(window_data, max_lag = MAX_LAG_MONTHS, min_length = MIN_LENGTH_MONTHS)

      basin_list[[length(basin_list) + 1]] <- data.table(
        basin = basin,
        basin_id = attrs$ID[basin],
        basin_name = attrs$name[basin],
        member = member,
        window = w,
        tau = pm$tau,
        tau_robust = pm$tau_robust,
        A_LF = pm$A_LF,
        variance = pm$variance,
        lag1_acf = pm$lag1_acf,
        decorrelation_time = pm$decorrelation_time,
        effective_n = pm$effective_n,
        success = pm$success
      )
    }
  }

  rbindlist(basin_list)
}

ipsl_persistence <- rbindlist(ipsl_results)

# Attach basin attributes
ipsl_persistence <- merge(ipsl_persistence, attrs_dt, by = "basin_id", all.x = TRUE, suffixes = c("", "_attr"))

cat("\n✓ IPSL persistence analysis complete\n")
cat("  Total records:", nrow(ipsl_persistence), "\n")
cat("  Successful analyses:", sum(ipsl_persistence$success, na.rm = TRUE), "\n\n")

# ============================================================================
# COMPUTE SUMMARY STATISTICS
# ============================================================================

cat("Computing summary statistics...\n")
cat("  Aggregating across all windows and members per basin...\n")

# CESM2 percentiles per basin (across ALL windows and members)
# Each basin now has: 80 members × ~1900 windows = ~152,000 persistence estimates
cesm_summary <- cesm_persistence[success == TRUE, .(
  tau_p05_cesm = quantile(tau, 0.05, na.rm = TRUE),
  tau_p50_cesm = quantile(tau, 0.50, na.rm = TRUE),
  tau_p95_cesm = quantile(tau, 0.95, na.rm = TRUE),
  tau_robust_p05_cesm = quantile(tau_robust, 0.05, na.rm = TRUE),
  tau_robust_p50_cesm = quantile(tau_robust, 0.50, na.rm = TRUE),
  tau_robust_p95_cesm = quantile(tau_robust, 0.95, na.rm = TRUE),
  A_LF_p05_cesm = quantile(A_LF, 0.05, na.rm = TRUE),
  A_LF_p50_cesm = quantile(A_LF, 0.50, na.rm = TRUE),
  A_LF_p95_cesm = quantile(A_LF, 0.95, na.rm = TRUE),
  lag1_acf_p05_cesm = quantile(lag1_acf, 0.05, na.rm = TRUE),
  lag1_acf_p50_cesm = quantile(lag1_acf, 0.50, na.rm = TRUE),
  lag1_acf_p95_cesm = quantile(lag1_acf, 0.95, na.rm = TRUE),
  n_windows_cesm = .N,
  n_members_cesm = length(unique(member))
), by = .(basin_id, basin_name)]

# IPSL percentiles per basin (across ALL windows and members)
# Each basin now has: 18 members × ~1200 windows = ~21,600 persistence estimates
ipsl_summary <- ipsl_persistence[success == TRUE, .(
  tau_p05_ipsl = quantile(tau, 0.05, na.rm = TRUE),
  tau_p50_ipsl = quantile(tau, 0.50, na.rm = TRUE),
  tau_p95_ipsl = quantile(tau, 0.95, na.rm = TRUE),
  tau_robust_p05_ipsl = quantile(tau_robust, 0.05, na.rm = TRUE),
  tau_robust_p50_ipsl = quantile(tau_robust, 0.50, na.rm = TRUE),
  tau_robust_p95_ipsl = quantile(tau_robust, 0.95, na.rm = TRUE),
  A_LF_p05_ipsl = quantile(A_LF, 0.05, na.rm = TRUE),
  A_LF_p50_ipsl = quantile(A_LF, 0.50, na.rm = TRUE),
  A_LF_p95_ipsl = quantile(A_LF, 0.95, na.rm = TRUE),
  lag1_acf_p05_ipsl = quantile(lag1_acf, 0.05, na.rm = TRUE),
  lag1_acf_p50_ipsl = quantile(lag1_acf, 0.50, na.rm = TRUE),
  lag1_acf_p95_ipsl = quantile(lag1_acf, 0.95, na.rm = TRUE),
  n_windows_ipsl = .N,
  n_members_ipsl = length(unique(member))
), by = .(basin_id, basin_name)]

# Merge with GRACE
persistence_summary <- merge(
  grace_persistence[, .(basin_id, basin_name, tau_grace, tau_robust_grace, A_LF_grace,
                        lag1_acf_grace, decorrelation_time_grace, effective_n_grace)],
  cesm_summary, by = c("basin_id", "basin_name"), all = TRUE)

persistence_summary <- merge(persistence_summary, ipsl_summary,
                             by = c("basin_id", "basin_name"), all = TRUE)

# Add basin attributes
persistence_summary <- merge(persistence_summary, attrs_dt,
                             by = "basin_id", all.x = TRUE, suffixes = c("", "_attr"))

cat("✓ Summary statistics computed\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

# Individual member results
saveRDS(cesm_persistence, "outputs/phase05_cesm_persistence.rds")
fwrite(cesm_persistence, "outputs/phase05_cesm_persistence.csv")

saveRDS(ipsl_persistence, "outputs/phase05_ipsl_persistence.rds")
fwrite(ipsl_persistence, "outputs/phase05_ipsl_persistence.csv")

# GRACE persistence
saveRDS(grace_persistence, "outputs/phase05_grace_persistence.rds")
fwrite(grace_persistence, "outputs/phase05_grace_persistence.csv")

# Summary statistics
saveRDS(persistence_summary, "outputs/phase05_persistence_summary.rds")
fwrite(persistence_summary, "outputs/phase05_persistence_summary.csv")

cat("  ✓ outputs/phase05_grace_persistence.{rds,csv}\n")
cat("  ✓ outputs/phase05_cesm_persistence.{rds,csv}\n")
cat("  ✓ outputs/phase05_ipsl_persistence.{rds,csv}\n")
cat("  ✓ outputs/phase05_persistence_summary.{rds,csv}\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("============================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================================================\n\n")

cat("GRACE PERSISTENCE:\n")
cat("  Mean τ:", round(mean(grace_persistence$tau_grace, na.rm = TRUE), 2), "months\n")
cat("  Mean A_LF:", round(mean(grace_persistence$A_LF_grace, na.rm = TRUE), 2), "mm\n\n")

cat("CESM2 PERSISTENCE:\n")
cat("  Total records:", nrow(cesm_persistence), "\n")
cat("  Mean τ:", round(mean(cesm_persistence$tau, na.rm = TRUE), 2), "months\n")
cat("  Mean A_LF:", round(mean(cesm_persistence$A_LF, na.rm = TRUE), 2), "mm\n\n")

cat("IPSL PERSISTENCE:\n")
cat("  Total records:", nrow(ipsl_persistence), "\n")
cat("  Mean τ:", round(mean(ipsl_persistence$tau, na.rm = TRUE), 2), "months\n")
cat("  Mean A_LF:", round(mean(ipsl_persistence$A_LF, na.rm = TRUE), 2), "mm\n\n")

# Check coverage (only include basins with valid model envelopes in denominator)
cat("COVERAGE ANALYSIS:\n")

# τ coverage - only count basins with valid GRACE and model data
valid_tau_cesm <- !is.na(persistence_summary$tau_grace) &
                  !is.na(persistence_summary$tau_p05_cesm) &
                  !is.na(persistence_summary$tau_p95_cesm)
tau_covered_cesm <- sum(persistence_summary$tau_grace[valid_tau_cesm] >= persistence_summary$tau_p05_cesm[valid_tau_cesm] &
                        persistence_summary$tau_grace[valid_tau_cesm] <= persistence_summary$tau_p95_cesm[valid_tau_cesm])
cat("  Basins where GRACE τ within CESM2 [5%-95%]:", tau_covered_cesm, "/", sum(valid_tau_cesm), "\n")

# A_LF coverage - only count basins with valid GRACE and model data
valid_A_LF_cesm <- !is.na(persistence_summary$A_LF_grace) &
                   !is.na(persistence_summary$A_LF_p05_cesm) &
                   !is.na(persistence_summary$A_LF_p95_cesm)
A_LF_covered_cesm <- sum(persistence_summary$A_LF_grace[valid_A_LF_cesm] >= persistence_summary$A_LF_p05_cesm[valid_A_LF_cesm] &
                         persistence_summary$A_LF_grace[valid_A_LF_cesm] <= persistence_summary$A_LF_p95_cesm[valid_A_LF_cesm])
cat("  Basins where GRACE A_LF within CESM2 [5%-95%]:", A_LF_covered_cesm, "/", sum(valid_A_LF_cesm), "\n\n")

# Cleanup parallel cluster
stopCluster(cl)
cat("✓ Parallel cluster stopped\n\n")

cat("============================================================================\n")
cat("PHASE 05 COMPLETE\n")
cat("============================================================================\n\n")
