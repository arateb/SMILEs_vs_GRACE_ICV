# ============================================================================
# PHASE 08 - METRIC SPACE & COMPATIBILITY INDEX - NOVEMBER 2025
# ============================================================================
# Synthesize all metrics into reduced metric vector M_b and compatibility index C_b
# Uses Mahalanobis distance to assess if GRACE falls within model ensemble spread
# Author: Ashraf Rateb
# Date: 2025-11-19
# ============================================================================

library(data.table)
library(tidyverse)

# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

cat("============================================================================\n")
cat("PHASE 08: METRIC SPACE & COMPATIBILITY INDEX\n")
cat("============================================================================\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Regularization parameter for nearly singular covariance matrices
RIDGE_LAMBDA <- 1e-6

cat("Configuration:\n")
cat("  Ridge regularization (λ):", RIDGE_LAMBDA, "\n")
cat("  Comprehensive metric vector: M_b = 10 metrics\n")
cat("    1. p_lf: low-frequency power (Phase 05)\n")
cat("    2. tau: memory timescale (Phase 05)\n")
cat("    3. H_max: pluvial height (Phase 06)\n")
cat("    4. D_max: drought depth (Phase 06)\n")
cat("    5. pluvial_ar1: pluvial regime persistence (Phase 06)\n")
cat("    6. drought_ar1: drought regime persistence (Phase 06)\n")
cat("    7. mean_duration: mean event duration (Phase 06)\n")
cat("    8. dominant_power_1: strongest wavelet power (Phase 04)\n")
cat("    9. enso_power: ENSO core power (Phase 04)\n")
cat("    10. qd_power: quasi-decadal power (Phase 04)\n\n")

# ============================================================================
# HELPER FUNCTION: COMPUTE COMPATIBILITY FOR ONE MODEL
# ============================================================================

#' Compute Mahalanobis distance and compatibility index for one model ensemble
#'
#' @param M_grace Numeric vector (10 metrics) for GRACE
#' @param M_model Matrix (n_members × 10) for model ensemble
#' @param ridge_lambda Ridge regularization parameter
#' @return List with d_mahal, C_b, compat_class, n_members, model_medians
compute_compatibility_single_model <- function(M_grace, M_model, ridge_lambda = 1e-6) {

  # Remove rows with any NA
  complete_rows <- complete.cases(M_model)
  M_model_clean <- M_model[complete_rows, , drop = FALSE]

  if (nrow(M_model_clean) < 10) {
    # Not enough data to estimate covariance
    return(list(
      n_members = nrow(M_model),
      model_medians = rep(NA_real_, 10),
      d_mahal = NA_real_,
      C_b = NA_real_,
      compat_class = NA_character_
    ))
  }

  # Compute mean vector and covariance matrix
  mu_model <- colMeans(M_model_clean, na.rm = TRUE)
  Sigma_model <- cov(M_model_clean, use = "complete.obs")

  # Regularization: Add ridge term to diagonal for numerical stability
  Sigma_model_reg <- Sigma_model + diag(ridge_lambda, nrow = ncol(Sigma_model))

  # Compute Mahalanobis distance for GRACE
  if (any(is.na(M_grace))) {
    d_grace <- NA_real_
    C_b <- NA_real_
  } else {
    d_grace <- tryCatch({
      mahalanobis(M_grace, center = mu_model, cov = Sigma_model_reg)
    }, error = function(e) {
      NA_real_
    })

    # Compute Mahalanobis distance for each model member
    d_model <- tryCatch({
      mahalanobis(M_model_clean, center = mu_model, cov = Sigma_model_reg)
    }, error = function(e) {
      rep(NA_real_, nrow(M_model_clean))
    })

    # Compatibility index: percentile rank of GRACE in model distribution
    if (!is.na(d_grace) && !any(is.na(d_model))) {
      C_b <- mean(d_model <= d_grace, na.rm = TRUE)
    } else {
      C_b <- NA_real_
    }
  }

  # Compatibility classification
  if (is.na(C_b)) {
    compat_class <- NA_character_
  } else {
    dist_from_median <- abs(C_b - 0.5)

    if (dist_from_median > 0.45) {
      compat_class <- "incompatible_outlier"
    } else if (dist_from_median > 0.40) {
      compat_class <- "marginal"
    } else {
      compat_class <- "compatible"
    }
  }

  return(list(
    n_members = nrow(M_model_clean),
    model_medians = apply(M_model_clean, 2, median, na.rm = TRUE),
    d_mahal = sqrt(d_grace),  # Return square root for interpretability
    C_b = C_b,
    compat_class = compat_class
  ))
}

# ============================================================================
# LOAD DATA FROM ALL PHASES
# ============================================================================

cat("Loading results from all phases...\n")

# Phase 5: Persistence metrics (τ, A_LF)
cat("  Loading Phase 5: Persistence metrics...\n")
persistence_summary <- readRDS("outputs/phase05_persistence_summary.rds")

# Phase 6: Event morphology (H_max, D_max)
cat("  Loading Phase 6: Event morphology...\n")
event_summary <- readRDS("outputs/phase06_event_summary.rds")

# Phase 4: Wavelet metrics (P_LF) - GRACE wavelets
cat("  Loading Phase 4: Wavelet metrics...\n")
grace_wavelet <- readRDS("outputs/phase04_grace_wavelets.rds")

# Phase 4: Model wavelets (for P_LF per member)
cat("  Loading Phase 4: Model wavelets...\n")
cesm_wavelets <- readRDS("outputs/phase04_cesm_all_wavelets.rds")
ipsl_wavelets <- readRDS("outputs/phase04_ipsl_all_wavelets.rds")

# Phase 5: Model persistence (for τ, A_LF per member)
cat("  Loading Phase 5: Model persistence...\n")
cesm_persistence <- readRDS("outputs/phase05_cesm_persistence.rds")
ipsl_persistence <- readRDS("outputs/phase05_ipsl_persistence.rds")

# Phase 6: Model events (for H_max, D_max per member)
cat("  Loading Phase 6: Model events...\n")
events_models <- readRDS("outputs/phase06_events_models.rds")

cat("✓ All phase data loaded\n\n")

# ============================================================================
# CONSTRUCT GRACE METRIC VECTOR M_b^G PER BASIN
# ============================================================================

cat("Constructing GRACE metric vectors (10 metrics)...\n")

# Start with wavelet metrics (Phase 04)
grace_metrics <- data.table(
  basin = grace_wavelet$basin,
  basin_id = grace_wavelet$basin_id,
  basin_name = grace_wavelet$basin_name,
  dominant_power_1 = grace_wavelet$dominant_power_1
)

# Add ENSO and QD power from Phase 04 wavelet summary
wavelet_summary <- readRDS("outputs/phase04_wavelet_summary.rds")
grace_metrics <- merge(
  grace_metrics,
  wavelet_summary[, .(basin_id, basin_name,
                     grace_enso_power, grace_qd_power)],
  by = c("basin_id", "basin_name"),
  all.x = TRUE
)

# Add persistence metrics (τ, p_lf) from Phase 05
grace_metrics <- merge(
  grace_metrics,
  persistence_summary[, .(basin_id, basin_name,
                         tau_grace, p_lf_grace)],
  by = c("basin_id", "basin_name"),
  all.x = TRUE
)

# Add event metrics (H_max, D_max, mean_duration) from Phase 06
grace_metrics <- merge(
  grace_metrics,
  event_summary[, .(basin_id, basin_name,
                   H_max_grace, D_max_grace, mean_duration_grace, median_duration_grace)],
  by = c("basin_id", "basin_name"),
  all.x = TRUE
)

# Add regime persistence (pluvial_ar1, drought_ar1) from Phase 05
regime_persistence_summary <- readRDS("outputs/phase05_grace_regime_persistence.rds")
grace_metrics <- merge(
  grace_metrics,
  regime_persistence_summary[, .(basin_id, basin_name,
                                pluvial_ar1_grace, drought_ar1_grace)],
  by = c("basin_id", "basin_name"),
  all.x = TRUE
)

# Rename for clarity and select 10 metrics
setnames(grace_metrics,
         c("p_lf_grace", "tau_grace", "H_max_grace", "D_max_grace",
           "mean_duration_grace", "pluvial_ar1_grace", "drought_ar1_grace",
           "dominant_power_1", "grace_enso_power", "grace_qd_power"),
         c("p_lf", "tau", "H_max", "D_max",
           "mean_duration", "pluvial_ar1", "drought_ar1",
           "power_1", "enso_power", "qd_power"))

cat("  GRACE metrics constructed for", nrow(grace_metrics), "basins\n")
cat("  10 metrics: p_lf, tau, H_max, D_max, pluvial_ar1, drought_ar1, mean_duration, power_1, enso_power, qd_power\n\n")

# Create basin lookup for adding basin_id/basin_name to model data
basin_lookup <- grace_metrics[, .(basin = basin, basin_id, basin_name)]

# ============================================================================
# CONSTRUCT MODEL METRIC VECTORS M_{b,m}^M PER BASIN-MEMBER
# ============================================================================

cat("Constructing model metric vectors (10 metrics per member)...\n")

# CESM2: Combine wavelets, persistence, events, and regime persistence
cat("  Processing CESM2...\n")

# Wavelets: Aggregate power metrics per basin × member
cesm_wavelets_member <- cesm_wavelets[, .(
  power_1 = median(dominant_power_1, na.rm = TRUE),
  enso_power = median(model_enso_power, na.rm = TRUE),
  qd_power = median(model_qd_power, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

# Persistence: Aggregate τ, p_lf per basin × member
# Add basin_id and basin_name via lookup first
cesm_persistence <- merge(
  cesm_persistence,
  basin_lookup,
  by = "basin",
  all.x = TRUE
)

cesm_persistence_member <- cesm_persistence[, .(
  tau = median(tau, na.rm = TRUE),
  p_lf = median(p_lf, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

# Merge wavelets and persistence at member level
cesm_metrics <- merge(
  cesm_wavelets_member,
  cesm_persistence_member,
  by = c("basin", "basin_id", "basin_name", "member"),
  all = FALSE  # Inner join: only members with both wavelet and persistence data
)

# Events: basin × member → H_max, D_max, mean_duration (no window column in events_models)
cesm_events_member <- events_models[model == "CESM2", .(
  H_max = ifelse(any(type == "pluvial"), max(pluvial_height, na.rm = TRUE), NA_real_),
  D_max = ifelse(any(type == "drought"), min(drought_depth, na.rm = TRUE), NA_real_),
  mean_duration = mean(duration_months, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

cesm_metrics <- merge(
  cesm_metrics,
  cesm_events_member,
  by = c("basin", "basin_id", "basin_name", "member"),
  all.x = TRUE
)

# Regime persistence: basin × member → pluvial_ar1, drought_ar1 (from Phase 05)
cesm_regime_persistence <- readRDS("outputs/phase05_cesm_regime_persistence.rds")
# Add basin_id and basin_name via lookup
cesm_regime_persistence <- merge(
  cesm_regime_persistence,
  basin_lookup,
  by = "basin",
  all.x = TRUE
)

cesm_regime_member <- cesm_regime_persistence[, .(
  pluvial_ar1 = median(pluvial_ar1, na.rm = TRUE),
  drought_ar1 = median(drought_ar1, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

cesm_metrics <- merge(
  cesm_metrics,
  cesm_regime_member,
  by = c("basin", "basin_id", "basin_name", "member"),
  all.x = TRUE
)

cesm_metrics[, model := "CESM2"]

cat("    CESM2 metrics:", nrow(cesm_metrics), "rows (basin × member)\n")
cat("    10 metrics per member: p_lf, tau, H_max, D_max, pluvial_ar1, drought_ar1, mean_duration, power_1, enso_power, qd_power\n")

# IPSL: Same member-level aggregation
cat("  Processing IPSL...\n")

# Wavelets: Aggregate power metrics per basin × member
ipsl_wavelets_member <- ipsl_wavelets[, .(
  power_1 = median(dominant_power_1, na.rm = TRUE),
  enso_power = median(model_enso_power, na.rm = TRUE),
  qd_power = median(model_qd_power, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

# Persistence: Aggregate τ, p_lf per basin × member
# Add basin_id and basin_name via lookup first
ipsl_persistence <- merge(
  ipsl_persistence,
  basin_lookup,
  by = "basin",
  all.x = TRUE
)

ipsl_persistence_member <- ipsl_persistence[, .(
  tau = median(tau, na.rm = TRUE),
  p_lf = median(p_lf, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

# Merge wavelets and persistence at member level
ipsl_metrics <- merge(
  ipsl_wavelets_member,
  ipsl_persistence_member,
  by = c("basin", "basin_id", "basin_name", "member"),
  all = FALSE  # Inner join: only members with both wavelet and persistence data
)

# Events: basin × member → H_max, D_max, mean_duration (no window column in events_models)
ipsl_events_member <- events_models[model == "IPSL", .(
  H_max = ifelse(any(type == "pluvial"), max(pluvial_height, na.rm = TRUE), NA_real_),
  D_max = ifelse(any(type == "drought"), min(drought_depth, na.rm = TRUE), NA_real_),
  mean_duration = mean(duration_months, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

ipsl_metrics <- merge(
  ipsl_metrics,
  ipsl_events_member,
  by = c("basin", "basin_id", "basin_name", "member"),
  all.x = TRUE
)

# Regime persistence: basin × member → pluvial_ar1, drought_ar1 (from Phase 05)
ipsl_regime_persistence <- readRDS("outputs/phase05_ipsl_regime_persistence.rds")
# Add basin_id and basin_name via lookup
ipsl_regime_persistence <- merge(
  ipsl_regime_persistence,
  basin_lookup,
  by = "basin",
  all.x = TRUE
)

ipsl_regime_member <- ipsl_regime_persistence[, .(
  pluvial_ar1 = median(pluvial_ar1, na.rm = TRUE),
  drought_ar1 = median(drought_ar1, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

ipsl_metrics <- merge(
  ipsl_metrics,
  ipsl_regime_member,
  by = c("basin", "basin_id", "basin_name", "member"),
  all.x = TRUE
)

ipsl_metrics[, model := "IPSL"]

cat("    IPSL metrics:", nrow(ipsl_metrics), "rows (basin × member)\n")
cat("    10 metrics per member: p_lf, tau, H_max, D_max, pluvial_ar1, drought_ar1, mean_duration, power_1, enso_power, qd_power\n")

# NOTE: Computing compatibility SEPARATELY for CESM2 and IPSL
# (NOT pooling models into a combined ensemble)
cat("  Computing compatibility separately for CESM2 and IPSL\n")
cat("  (NOT pooling models - separate Mahalanobis spaces)\n\n")

# ============================================================================
# COMPUTE COMPATIBILITY INDEX PER BASIN (SEPARATELY FOR EACH MODEL)
# ============================================================================

cat("Computing compatibility indices per basin (CESM2 and IPSL separately)...\n")

# Define metric columns (10 metrics in order)
metric_cols <- c("p_lf", "tau", "H_max", "D_max", "pluvial_ar1", "drought_ar1",
                 "mean_duration", "power_1", "enso_power", "qd_power")

# Initialize results
compatibility_results_cesm <- list()
compatibility_results_ipsl <- list()

n_basins <- nrow(grace_metrics)
pb <- txtProgressBar(min = 0, max = n_basins, style = 3)

for (i in 1:n_basins) {
  basin_i <- grace_metrics$basin[i]
  basin_id_i <- grace_metrics$basin_id[i]
  basin_name_i <- grace_metrics$basin_name[i]

  # GRACE metric vector for this basin
  M_grace <- as.numeric(grace_metrics[i, ..metric_cols])

  # === CESM2 COMPATIBILITY ===
  model_basin_cesm <- cesm_metrics[basin == basin_i]
  M_model_cesm <- if (nrow(model_basin_cesm) > 0) as.matrix(model_basin_cesm[, ..metric_cols]) else matrix(NA_real_, 0, 10)

  result_cesm <- compute_compatibility_single_model(M_grace, M_model_cesm, RIDGE_LAMBDA)

  compatibility_results_cesm[[i]] <- data.table(
    basin = basin_i,
    basin_id = basin_id_i,
    basin_name = basin_name_i,
    p_lf_grace = M_grace[1],
    tau_grace = M_grace[2],
    H_max_grace = M_grace[3],
    D_max_grace = M_grace[4],
    pluvial_ar1_grace = M_grace[5],
    drought_ar1_grace = M_grace[6],
    mean_duration_grace = M_grace[7],
    power_1_grace = M_grace[8],
    enso_power_grace = M_grace[9],
    qd_power_grace = M_grace[10],
    p_lf_model_median_cesm = result_cesm$model_medians[1],
    tau_model_median_cesm = result_cesm$model_medians[2],
    H_max_model_median_cesm = result_cesm$model_medians[3],
    D_max_model_median_cesm = result_cesm$model_medians[4],
    pluvial_ar1_model_median_cesm = result_cesm$model_medians[5],
    drought_ar1_model_median_cesm = result_cesm$model_medians[6],
    mean_duration_model_median_cesm = result_cesm$model_medians[7],
    power_1_model_median_cesm = result_cesm$model_medians[8],
    enso_power_model_median_cesm = result_cesm$model_medians[9],
    qd_power_model_median_cesm = result_cesm$model_medians[10],
    n_members_cesm = result_cesm$n_members,
    d_mahal_cesm = result_cesm$d_mahal,
    C_b_cesm = result_cesm$C_b,
    compat_class_cesm = result_cesm$compat_class
  )

  # === IPSL COMPATIBILITY ===
  model_basin_ipsl <- ipsl_metrics[basin == basin_i]
  M_model_ipsl <- if (nrow(model_basin_ipsl) > 0) as.matrix(model_basin_ipsl[, ..metric_cols]) else matrix(NA_real_, 0, 10)

  result_ipsl <- compute_compatibility_single_model(M_grace, M_model_ipsl, RIDGE_LAMBDA)

  compatibility_results_ipsl[[i]] <- data.table(
    basin = basin_i,
    basin_id = basin_id_i,
    basin_name = basin_name_i,
    p_lf_model_median_ipsl = result_ipsl$model_medians[1],
    tau_model_median_ipsl = result_ipsl$model_medians[2],
    H_max_model_median_ipsl = result_ipsl$model_medians[3],
    D_max_model_median_ipsl = result_ipsl$model_medians[4],
    pluvial_ar1_model_median_ipsl = result_ipsl$model_medians[5],
    drought_ar1_model_median_ipsl = result_ipsl$model_medians[6],
    mean_duration_model_median_ipsl = result_ipsl$model_medians[7],
    power_1_model_median_ipsl = result_ipsl$model_medians[8],
    enso_power_model_median_ipsl = result_ipsl$model_medians[9],
    qd_power_model_median_ipsl = result_ipsl$model_medians[10],
    n_members_ipsl = result_ipsl$n_members,
    d_mahal_ipsl = result_ipsl$d_mahal,
    C_b_ipsl = result_ipsl$C_b,
    compat_class_ipsl = result_ipsl$compat_class
  )

  setTxtProgressBar(pb, i)
}

close(pb)

# Combine CESM2 and IPSL results (one row per basin with both models)
compat_cesm <- rbindlist(compatibility_results_cesm)
compat_ipsl <- rbindlist(compatibility_results_ipsl)

compatibility_basin <- merge(
  compat_cesm,
  compat_ipsl,
  by = c("basin", "basin_id", "basin_name"),
  all = TRUE
)

cat("\n✓ Compatibility indices computed for", nrow(compatibility_basin), "basins\n")
cat("  CESM2 compatibility column: C_b_cesm\n")
cat("  IPSL compatibility column: C_b_ipsl\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

saveRDS(compatibility_basin, "outputs/phase08_compatibility_basin.rds")
fwrite(compatibility_basin, "outputs/phase08_compatibility_basin.csv")

cat("  ✓ outputs/phase08_compatibility_basin.{rds,csv}\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("============================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================================================\n\n")

cat("CESM2 Compatibility index (C_b_cesm) distribution:\n")
cat("  Mean:", round(mean(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "\n")
cat("  Median:", round(median(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "\n")
cat("  Min:", round(min(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "\n")
cat("  Max:", round(max(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "\n\n")

cat("IPSL Compatibility index (C_b_ipsl) distribution:\n")
cat("  Mean:", round(mean(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "\n")
cat("  Median:", round(median(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "\n")
cat("  Min:", round(min(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "\n")
cat("  Max:", round(max(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "\n\n")

cat("CESM2 Mahalanobis distance (d_mahal_cesm) distribution:\n")
cat("  Mean:", round(mean(compatibility_basin$d_mahal_cesm, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(compatibility_basin$d_mahal_cesm, na.rm = TRUE), 2), "\n")
cat("  Min:", round(min(compatibility_basin$d_mahal_cesm, na.rm = TRUE), 2), "\n")
cat("  Max:", round(max(compatibility_basin$d_mahal_cesm, na.rm = TRUE), 2), "\n\n")

cat("IPSL Mahalanobis distance (d_mahal_ipsl) distribution:\n")
cat("  Mean:", round(mean(compatibility_basin$d_mahal_ipsl, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(compatibility_basin$d_mahal_ipsl, na.rm = TRUE), 2), "\n")
cat("  Min:", round(min(compatibility_basin$d_mahal_ipsl, na.rm = TRUE), 2), "\n")
cat("  Max:", round(max(compatibility_basin$d_mahal_ipsl, na.rm = TRUE), 2), "\n\n")

cat("CESM2 Compatibility classifications:\n")
print(table(compatibility_basin$compat_class_cesm, useNA = "ifany"))
cat("\n")

cat("IPSL Compatibility classifications:\n")
print(table(compatibility_basin$compat_class_ipsl, useNA = "ifany"))
cat("\n")

cat("Interpretation:\n")
cat("  C_b = percentile rank of GRACE Mahalanobis distance in ensemble\n")
cat("  C_b = 0.5 → GRACE at median (compatible)\n")
cat("  C_b = 1.0 → GRACE farther from center than ALL members (incompatible)\n")
cat("  C_b = 0.0 → GRACE closer to center than ALL members (also unusual)\n\n")
cat("  Classification based on |C_b - 0.5|:\n")
cat("    incompatible_outlier: |C_b - 0.5| > 0.45 (< 5th or > 95th percentile)\n")
cat("    marginal: 0.40 < |C_b - 0.5| ≤ 0.45 (5th-10th or 90th-95th percentile)\n")
cat("    compatible: |C_b - 0.5| ≤ 0.40 (within central 80% of ensemble)\n\n")

# Basins where GRACE is incompatible with CESM2
extreme_basins_cesm <- compatibility_basin[compat_class_cesm == "incompatible_outlier"]
cat("Basins incompatible with CESM2 (outliers):", nrow(extreme_basins_cesm), "\n")
if (nrow(extreme_basins_cesm) > 0) {
  cat("  Top 10 most incompatible (largest |C_b_cesm - 0.5|):\n")
  extreme_sorted_cesm <- extreme_basins_cesm[order(abs(C_b_cesm - 0.5), decreasing = TRUE)]
  print(extreme_sorted_cesm[1:min(10, nrow(extreme_sorted_cesm)), .(basin_name, C_b_cesm, d_mahal_cesm, compat_class_cesm)])
}
cat("\n")

# Basins where GRACE is incompatible with IPSL
extreme_basins_ipsl <- compatibility_basin[compat_class_ipsl == "incompatible_outlier"]
cat("Basins incompatible with IPSL (outliers):", nrow(extreme_basins_ipsl), "\n")
if (nrow(extreme_basins_ipsl) > 0) {
  cat("  Top 10 most incompatible (largest |C_b_ipsl - 0.5|):\n")
  extreme_sorted_ipsl <- extreme_basins_ipsl[order(abs(C_b_ipsl - 0.5), decreasing = TRUE)]
  print(extreme_sorted_ipsl[1:min(10, nrow(extreme_sorted_ipsl)), .(basin_name, C_b_ipsl, d_mahal_ipsl, compat_class_ipsl)])
}
cat("\n")

cat("Metric comparison (GRACE vs CESM2 vs IPSL medians):\n")
cat("  1. p_lf (low-frequency power):\n")
cat("    GRACE median:", round(median(compatibility_basin$p_lf_grace, na.rm = TRUE), 3), "\n")
cat("    CESM2 median:", round(median(compatibility_basin$p_lf_model_median_cesm, na.rm = TRUE), 3), "\n")
cat("    IPSL median:", round(median(compatibility_basin$p_lf_model_median_ipsl, na.rm = TRUE), 3), "\n")
cat("  2. tau (memory timescale):\n")
cat("    GRACE median:", round(median(compatibility_basin$tau_grace, na.rm = TRUE), 1), "months\n")
cat("    CESM2 median:", round(median(compatibility_basin$tau_model_median_cesm, na.rm = TRUE), 1), "months\n")
cat("    IPSL median:", round(median(compatibility_basin$tau_model_median_ipsl, na.rm = TRUE), 1), "months\n")
cat("  3. H_max (pluvial height):\n")
cat("    GRACE median:", round(median(compatibility_basin$H_max_grace, na.rm = TRUE), 1), "mm\n")
cat("    CESM2 median:", round(median(compatibility_basin$H_max_model_median_cesm, na.rm = TRUE), 1), "mm\n")
cat("    IPSL median:", round(median(compatibility_basin$H_max_model_median_ipsl, na.rm = TRUE), 1), "mm\n")
cat("  4. D_max (drought depth):\n")
cat("    GRACE median:", round(median(compatibility_basin$D_max_grace, na.rm = TRUE), 1), "mm\n")
cat("    CESM2 median:", round(median(compatibility_basin$D_max_model_median_cesm, na.rm = TRUE), 1), "mm\n")
cat("    IPSL median:", round(median(compatibility_basin$D_max_model_median_ipsl, na.rm = TRUE), 1), "mm\n")
cat("  5. pluvial_ar1 (pluvial regime persistence):\n")
cat("    GRACE median:", round(median(compatibility_basin$pluvial_ar1_grace, na.rm = TRUE), 3), "\n")
cat("    CESM2 median:", round(median(compatibility_basin$pluvial_ar1_model_median_cesm, na.rm = TRUE), 3), "\n")
cat("    IPSL median:", round(median(compatibility_basin$pluvial_ar1_model_median_ipsl, na.rm = TRUE), 3), "\n")
cat("  6. drought_ar1 (drought regime persistence):\n")
cat("    GRACE median:", round(median(compatibility_basin$drought_ar1_grace, na.rm = TRUE), 3), "\n")
cat("    CESM2 median:", round(median(compatibility_basin$drought_ar1_model_median_cesm, na.rm = TRUE), 3), "\n")
cat("    IPSL median:", round(median(compatibility_basin$drought_ar1_model_median_ipsl, na.rm = TRUE), 3), "\n")
cat("  7. mean_duration (mean event duration):\n")
cat("    GRACE median:", round(median(compatibility_basin$mean_duration_grace, na.rm = TRUE), 1), "months\n")
cat("    CESM2 median:", round(median(compatibility_basin$mean_duration_model_median_cesm, na.rm = TRUE), 1), "months\n")
cat("    IPSL median:", round(median(compatibility_basin$mean_duration_model_median_ipsl, na.rm = TRUE), 1), "months\n")
cat("  8. power_1 (dominant wavelet power):\n")
cat("    GRACE median:", round(median(compatibility_basin$power_1_grace, na.rm = TRUE), 1), "\n")
cat("    CESM2 median:", round(median(compatibility_basin$power_1_model_median_cesm, na.rm = TRUE), 1), "\n")
cat("    IPSL median:", round(median(compatibility_basin$power_1_model_median_ipsl, na.rm = TRUE), 1), "\n")
cat("  9. enso_power (ENSO core power 2-7yr):\n")
cat("    GRACE median:", round(median(compatibility_basin$enso_power_grace, na.rm = TRUE), 1), "\n")
cat("    CESM2 median:", round(median(compatibility_basin$enso_power_model_median_cesm, na.rm = TRUE), 1), "\n")
cat("    IPSL median:", round(median(compatibility_basin$enso_power_model_median_ipsl, na.rm = TRUE), 1), "\n")
cat("  10. qd_power (quasi-decadal power 7-15yr):\n")
cat("    GRACE median:", round(median(compatibility_basin$qd_power_grace, na.rm = TRUE), 1), "\n")
cat("    CESM2 median:", round(median(compatibility_basin$qd_power_model_median_cesm, na.rm = TRUE), 1), "\n")
cat("    IPSL median:", round(median(compatibility_basin$qd_power_model_median_ipsl, na.rm = TRUE), 1), "\n\n")

cat("============================================================================\n")
cat("PHASE 08 COMPLETE\n")
cat("============================================================================\n\n")
