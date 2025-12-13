#!/usr/bin/env Rscript
# ============================================================================
# PHASE 08 - COMPREHENSIVE METRIC SPACE & COMPATIBILITY INDEX - NOVEMBER 2025
# ============================================================================
# Synthesize all metrics into comprehensive 12-metric vector M_b
# Compute Mahalanobis distance to assess GRACE compatibility with SMILEs
#
# METRIC VECTOR (12 dimensions):
# -----------------------------
# AMPLITUDE/VARIANCE (2):
#   1. A        - Total amplitude (max - min)
#   2. sigma    - Standard deviation
#
# PERSISTENCE (3):
#   3. ar1      - AR(1) coefficient (full contiguous series)
#   4. tau      - Memory timescale (e-folding time)
#   5. p_lf     - Low-frequency amplitude
#
# EVENT EXTREMES (4):
#   6. H_max    - Maximum pluvial height
#   7. D_max    - Maximum drought depth (most negative)
#   8. I_pluvial - Maximum pluvial intensity (cumulative)
#   9. I_drought - Maximum drought intensity (cumulative)
#
# EVENT DYNAMICS (1):
#   10. mean_duration - Mean event duration
#
# SPECTRAL (2):
#   11. enso_power - ENSO band power (2-7 years)
#   12. qd_power   - Quasi-decadal band power (7-15 years)
#
# Author: Ashraf Rateb
# Date: 2025-11-27
# ============================================================================

library(data.table)
library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)

# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures_phase08", showWarnings = FALSE, recursive = TRUE)

cat("============================================================================\n")
cat("PHASE 08: COMPREHENSIVE METRIC SPACE & COMPATIBILITY INDEX\n")
cat("============================================================================\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Regularization parameter for nearly singular covariance matrices
RIDGE_LAMBDA <- 1e-6

# Define the metrics that have TRUE per-member variability
# Event metrics (H_max, D_max, I_*, duration) use basin p50 - no member variation
# So we EXCLUDE them to avoid singular covariance matrix
METRIC_NAMES <- c(
  "ar1",          # AR(1) coefficient - varies by member/window
  "tau",          # Memory timescale - varies by member/window
  "p_lf",         # Low-frequency amplitude - varies by member/window
  "enso_power",   # ENSO band power - varies by member
  "qd_power"      # Quasi-decadal power - varies by member
)

# Note: Reduced from 12 to 5 metrics that have genuine per-member variability
# Event and amplitude metrics only available at basin-level, not member-level

cat("Configuration:\n")
cat("  Ridge regularization (λ):", RIDGE_LAMBDA, "\n")
cat("  Metric vector dimension:", length(METRIC_NAMES), "\n")
cat("  Metrics:\n")
for (i in seq_along(METRIC_NAMES)) {
  cat(sprintf("    %2d. %s\n", i, METRIC_NAMES[i]))
}
cat("\n")

# ============================================================================
# HELPER FUNCTION: COMPUTE COMPATIBILITY FOR ONE MODEL
# ============================================================================

compute_compatibility_single_model <- function(M_grace, M_model, ridge_lambda = 1e-6) {

  # Remove rows with any NA
  complete_rows <- complete.cases(M_model)
  M_model_clean <- M_model[complete_rows, , drop = FALSE]

  n_metrics <- ncol(M_model_clean)

  # Need at least n_metrics + 1 observations to estimate covariance
  if (nrow(M_model_clean) < n_metrics + 5) {
    return(list(
      n_members = nrow(M_model),
      n_complete = nrow(M_model_clean),
      model_medians = rep(NA_real_, n_metrics),
      model_means = rep(NA_real_, n_metrics),
      model_sds = rep(NA_real_, n_metrics),
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

    # Compatibility index: fraction of members with d <= d_grace
    # C_b ≈ 0.5 means GRACE at median distance (compatible)
    # C_b ≈ 1.0 means GRACE farther than ALL members (incompatible outlier)
    # C_b ≈ 0.0 means GRACE closer than ALL members (unusual)
    if (!is.na(d_grace) && !any(is.na(d_model))) {
      C_b <- mean(d_model <= d_grace, na.rm = TRUE)
    } else {
      C_b <- NA_real_
    }
  }

  # Compatibility classification based on |C_b - 0.5|
  if (is.na(C_b)) {
    compat_class <- NA_character_
  } else {
    dist_from_median <- abs(C_b - 0.5)

    if (dist_from_median > 0.45) {
      compat_class <- "incompatible"  # < 5th or > 95th percentile
    } else if (dist_from_median > 0.40) {
      compat_class <- "marginal"      # 5th-10th or 90th-95th percentile
    } else {
      compat_class <- "compatible"    # Within central 80%
    }
  }

  return(list(
    n_members = nrow(M_model),
    n_complete = nrow(M_model_clean),
    model_medians = apply(M_model_clean, 2, median, na.rm = TRUE),
    model_means = mu_model,
    model_sds = apply(M_model_clean, 2, sd, na.rm = TRUE),
    d_mahal = sqrt(d_grace),  # Return square root for interpretability
    C_b = C_b,
    compat_class = compat_class
  ))
}

# ============================================================================
# LOAD DATA FROM ALL PHASES
# ============================================================================

cat("Loading results from all phases...\n")

# Phase 03: Dispersion metrics (A, sigma)
cat("  Loading Phase 03: Dispersion metrics...\n")
dispersion_summary <- readRDS("outputs/dispersion_summary.rds")

# Phase 04: Wavelet metrics (enso_power, qd_power, dominant_power)
cat("  Loading Phase 04: Wavelet metrics...\n")
wavelet_summary <- readRDS("outputs/phase04_wavelet_summary.rds")
grace_wavelet <- readRDS("outputs/phase04_grace_wavelets.rds")
cesm_wavelets <- readRDS("outputs/phase04_cesm_all_wavelets.rds")
ipsl_wavelets <- readRDS("outputs/phase04_ipsl_all_wavelets.rds")

# Phase 05: Persistence metrics (ar1, tau, p_lf)
cat("  Loading Phase 05: Persistence metrics...\n")
persistence_summary <- readRDS("outputs/phase05_persistence_summary.rds")
cesm_persistence <- readRDS("outputs/phase05_cesm_persistence.rds")
ipsl_persistence <- readRDS("outputs/phase05_ipsl_persistence.rds")

# Phase 06: Event metrics (H_max, D_max, I_pluvial, I_drought, mean_duration)
cat("  Loading Phase 06: Event metrics...\n")
event_summary <- readRDS("outputs/phase06_event_summary.rds")
# Note: events_models is 157M rows - too large, using event_summary instead

# Basin attributes
cat("  Loading basin attributes...\n")
attrs <- readRDS("outputs/basin_attributes.rds")

# Ensure consistent types - basin_id should be numeric
dispersion_summary[, basin_id := as.numeric(basin_id)]
persistence_summary[, basin_id := as.numeric(basin_id)]
event_summary[, basin_id := as.numeric(basin_id)]
wavelet_summary[, basin_id := as.numeric(basin_id)]

cat("✓ All phase data loaded\n\n")

# ============================================================================
# CONSTRUCT GRACE METRIC VECTOR M_b^G PER BASIN (12 metrics)
# ============================================================================

cat("Constructing GRACE metric vectors (5 metrics)...\n")

# Build GRACE metric vector with only metrics that have member-level variability
# Start with persistence metrics from Phase 05
grace_metrics <- persistence_summary[, .(
  basin,
  basin_id,
  basin_name,
  ar1 = ar1_grace,
  tau = tau_grace,
  p_lf = p_lf_grace
)]

# Add wavelet metrics from Phase 04
grace_metrics <- merge(
  grace_metrics,
  wavelet_summary[, .(basin,
                      enso_power = grace_enso_power,
                      qd_power = grace_qd_power)],
  by = "basin",
  all.x = TRUE
)

# Ensure column order matches METRIC_NAMES
setcolorder(grace_metrics, c("basin", "basin_id", "basin_name", METRIC_NAMES))

cat("  GRACE metrics constructed for", nrow(grace_metrics), "basins\n")
cat("  12 metrics:", paste(METRIC_NAMES, collapse = ", "), "\n\n")

# Check for missing values
n_complete_grace <- sum(complete.cases(grace_metrics[, ..METRIC_NAMES]))
cat("  Basins with complete GRACE metrics:", n_complete_grace, "/", nrow(grace_metrics), "\n\n")

# ============================================================================
# CONSTRUCT MODEL METRIC VECTORS M_{b,m}^M PER BASIN-MEMBER (12 metrics)
# ============================================================================

cat("Constructing model metric vectors (12 metrics per member)...\n")

# Create basin lookup
basin_lookup <- grace_metrics[, .(basin, basin_id, basin_name)]

# ----------------------------------------------------------------------------
# CESM2 METRICS (5 metrics with member-level variability)
# ----------------------------------------------------------------------------

cat("  Processing CESM2...\n")

# Persistence: ar1, tau, p_lf per basin × member (aggregate across windows)
cesm_persistence_member <- cesm_persistence[, .(
  ar1 = median(ar1, na.rm = TRUE),
  tau = median(tau, na.rm = TRUE),
  p_lf = median(p_lf, na.rm = TRUE)
), by = .(basin, member)]

# Wavelets: enso_power, qd_power per basin × member
cesm_wavelets_member <- cesm_wavelets[, .(
  enso_power = median(model_enso_power, na.rm = TRUE),
  qd_power = median(model_qd_power, na.rm = TRUE)
), by = .(basin, member)]

# Merge all CESM2 metrics
cesm_metrics <- merge(cesm_persistence_member, cesm_wavelets_member,
                      by = c("basin", "member"), all = TRUE)

# Add basin identifiers
cesm_metrics <- merge(cesm_metrics, basin_lookup, by = "basin", all.x = TRUE)
cesm_metrics[, model := "CESM2"]

cat("    CESM2 metrics:", nrow(cesm_metrics), "rows (basin × member)\n")

# ----------------------------------------------------------------------------
# IPSL METRICS (5 metrics with member-level variability)
# ----------------------------------------------------------------------------

cat("  Processing IPSL...\n")

# Persistence: ar1, tau, p_lf per basin × member
ipsl_persistence_member <- ipsl_persistence[, .(
  ar1 = median(ar1, na.rm = TRUE),
  tau = median(tau, na.rm = TRUE),
  p_lf = median(p_lf, na.rm = TRUE)
), by = .(basin, member)]

# Wavelets: enso_power, qd_power per basin × member
ipsl_wavelets_member <- ipsl_wavelets[, .(
  enso_power = median(model_enso_power, na.rm = TRUE),
  qd_power = median(model_qd_power, na.rm = TRUE)
), by = .(basin, member)]

# Merge all IPSL metrics
ipsl_metrics <- merge(ipsl_persistence_member, ipsl_wavelets_member,
                      by = c("basin", "member"), all = TRUE)

# Add basin identifiers
ipsl_metrics <- merge(ipsl_metrics, basin_lookup, by = "basin", all.x = TRUE)
ipsl_metrics[, model := "IPSL"]

cat("    IPSL metrics:", nrow(ipsl_metrics), "rows (basin × member)\n\n")

# ============================================================================
# COMPUTE COMPATIBILITY INDEX PER BASIN (SEPARATELY FOR EACH MODEL)
# ============================================================================

cat("Computing compatibility indices per basin...\n")

# Initialize results
compatibility_results <- list()

n_basins <- nrow(grace_metrics)
pb <- txtProgressBar(min = 0, max = n_basins, style = 3)

for (i in 1:n_basins) {
  basin_i <- grace_metrics$basin[i]
  basin_id_i <- grace_metrics$basin_id[i]
  basin_name_i <- grace_metrics$basin_name[i]

  # GRACE metric vector for this basin (12 metrics)
  M_grace <- as.numeric(grace_metrics[i, ..METRIC_NAMES])

  # === CESM2 COMPATIBILITY ===
  model_basin_cesm <- cesm_metrics[basin == basin_i]
  M_model_cesm <- if (nrow(model_basin_cesm) > 0) {
    as.matrix(model_basin_cesm[, ..METRIC_NAMES])
  } else {
    matrix(NA_real_, 0, length(METRIC_NAMES))
  }

  result_cesm <- compute_compatibility_single_model(M_grace, M_model_cesm, RIDGE_LAMBDA)

  # === IPSL COMPATIBILITY ===
  model_basin_ipsl <- ipsl_metrics[basin == basin_i]
  M_model_ipsl <- if (nrow(model_basin_ipsl) > 0) {
    as.matrix(model_basin_ipsl[, ..METRIC_NAMES])
  } else {
    matrix(NA_real_, 0, length(METRIC_NAMES))
  }

  result_ipsl <- compute_compatibility_single_model(M_grace, M_model_ipsl, RIDGE_LAMBDA)

  # Combine results (5 metrics: ar1, tau, p_lf, enso_power, qd_power)
  compatibility_results[[i]] <- data.table(
    basin = basin_i,
    basin_id = basin_id_i,
    basin_name = basin_name_i,

    # GRACE metrics
    ar1_grace = M_grace[1],
    tau_grace = M_grace[2],
    p_lf_grace = M_grace[3],
    enso_power_grace = M_grace[4],
    qd_power_grace = M_grace[5],

    # CESM2 results
    n_members_cesm = result_cesm$n_members,
    n_complete_cesm = result_cesm$n_complete,
    d_mahal_cesm = result_cesm$d_mahal,
    C_b_cesm = result_cesm$C_b,
    compat_class_cesm = result_cesm$compat_class,

    # CESM2 model medians
    ar1_median_cesm = result_cesm$model_medians[1],
    tau_median_cesm = result_cesm$model_medians[2],
    p_lf_median_cesm = result_cesm$model_medians[3],
    enso_power_median_cesm = result_cesm$model_medians[4],
    qd_power_median_cesm = result_cesm$model_medians[5],

    # IPSL results
    n_members_ipsl = result_ipsl$n_members,
    n_complete_ipsl = result_ipsl$n_complete,
    d_mahal_ipsl = result_ipsl$d_mahal,
    C_b_ipsl = result_ipsl$C_b,
    compat_class_ipsl = result_ipsl$compat_class,

    # IPSL model medians
    ar1_median_ipsl = result_ipsl$model_medians[1],
    tau_median_ipsl = result_ipsl$model_medians[2],
    p_lf_median_ipsl = result_ipsl$model_medians[3],
    enso_power_median_ipsl = result_ipsl$model_medians[4],
    qd_power_median_ipsl = result_ipsl$model_medians[5]
  )

  setTxtProgressBar(pb, i)
}

close(pb)

# Combine all results
compatibility_basin <- rbindlist(compatibility_results)

# Add basin attributes - merge by basin name since basin_id is numeric index
attrs_dt <- data.table(attrs)
compatibility_basin <- merge(
  compatibility_basin,
  attrs_dt[, .(basin_name = name, lon = C_lon, lat = C_lat, area = Area_km2, climate)],
  by = "basin_name",
  all.x = TRUE
)

cat("\n✓ Compatibility indices computed for", nrow(compatibility_basin), "basins\n\n")

# ============================================================================
# COMPUTE COMBINED COMPATIBILITY CLASS
# ============================================================================

cat("Computing combined compatibility classification...\n")

compatibility_basin[, compat_class_combined := {
  if (is.na(compat_class_cesm) | is.na(compat_class_ipsl)) {
    NA_character_
  } else if (compat_class_cesm == "incompatible" & compat_class_ipsl == "incompatible") {
    "incompatible_both"
  } else if (compat_class_cesm == "incompatible" | compat_class_ipsl == "incompatible") {
    "incompatible_one"
  } else if (compat_class_cesm == "marginal" & compat_class_ipsl == "marginal") {
    "marginal_both"
  } else if (compat_class_cesm == "marginal" | compat_class_ipsl == "marginal") {
    "marginal_one"
  } else {
    "compatible_both"
  }
}, by = 1:nrow(compatibility_basin)]

cat("Combined classification:\n")
print(table(compatibility_basin$compat_class_combined, useNA = "ifany"))
cat("\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

saveRDS(compatibility_basin, "outputs/phase08_compatibility_comprehensive.rds")
fwrite(compatibility_basin, "outputs/phase08_compatibility_comprehensive.csv")

cat("  ✓ outputs/phase08_compatibility_comprehensive.{rds,csv}\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("============================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================================================\n\n")

cat("CESM2 Compatibility Index (C_b) distribution:\n")
cat("  Mean:", round(mean(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "\n")
cat("  Median:", round(median(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "\n")
cat("  SD:", round(sd(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "\n")
cat("  Range:", round(min(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "-",
    round(max(compatibility_basin$C_b_cesm, na.rm = TRUE), 3), "\n\n")

cat("IPSL Compatibility Index (C_b) distribution:\n")
cat("  Mean:", round(mean(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "\n")
cat("  Median:", round(median(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "\n")
cat("  SD:", round(sd(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "\n")
cat("  Range:", round(min(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "-",
    round(max(compatibility_basin$C_b_ipsl, na.rm = TRUE), 3), "\n\n")

cat("CESM2 Mahalanobis Distance (d) distribution:\n")
cat("  Mean:", round(mean(compatibility_basin$d_mahal_cesm, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(compatibility_basin$d_mahal_cesm, na.rm = TRUE), 2), "\n")
cat("  Range:", round(min(compatibility_basin$d_mahal_cesm, na.rm = TRUE), 2), "-",
    round(max(compatibility_basin$d_mahal_cesm, na.rm = TRUE), 2), "\n\n")

cat("IPSL Mahalanobis Distance (d) distribution:\n")
cat("  Mean:", round(mean(compatibility_basin$d_mahal_ipsl, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(compatibility_basin$d_mahal_ipsl, na.rm = TRUE), 2), "\n")
cat("  Range:", round(min(compatibility_basin$d_mahal_ipsl, na.rm = TRUE), 2), "-",
    round(max(compatibility_basin$d_mahal_ipsl, na.rm = TRUE), 2), "\n\n")

cat("CESM2 Compatibility Classifications:\n")
print(table(compatibility_basin$compat_class_cesm, useNA = "ifany"))
cat("\n")

cat("IPSL Compatibility Classifications:\n")
print(table(compatibility_basin$compat_class_ipsl, useNA = "ifany"))
cat("\n")

# Key statistics for publication
n_incompatible_cesm <- sum(compatibility_basin$compat_class_cesm == "incompatible", na.rm = TRUE)
n_incompatible_ipsl <- sum(compatibility_basin$compat_class_ipsl == "incompatible", na.rm = TRUE)
n_incompatible_both <- sum(compatibility_basin$compat_class_combined == "incompatible_both", na.rm = TRUE)
n_compatible_both <- sum(compatibility_basin$compat_class_combined == "compatible_both", na.rm = TRUE)
n_total <- sum(!is.na(compatibility_basin$compat_class_cesm))

cat("KEY FINDINGS:\n")
cat("  Basins incompatible with CESM2:", n_incompatible_cesm, "(",
    round(100 * n_incompatible_cesm / n_total, 1), "%)\n")
cat("  Basins incompatible with IPSL:", n_incompatible_ipsl, "(",
    round(100 * n_incompatible_ipsl / n_total, 1), "%)\n")
cat("  Basins incompatible with BOTH:", n_incompatible_both, "(",
    round(100 * n_incompatible_both / n_total, 1), "%)\n")
cat("  Basins compatible with BOTH:", n_compatible_both, "(",
    round(100 * n_compatible_both / n_total, 1), "%)\n\n")

# ============================================================================
# METRIC-BY-METRIC COMPARISON (GRACE vs MODEL MEDIANS)
# ============================================================================

cat("METRIC COMPARISONS (GRACE vs Model Medians):\n")
cat("============================================================================\n\n")

# 5 metrics: ar1, tau, p_lf, enso_power, qd_power
metric_comparison <- data.table(
  Metric = c("AR(1)", "Memory (τ)", "LF Amplitude", "ENSO Power", "QD Power"),
  GRACE_median = c(
    median(compatibility_basin$ar1_grace, na.rm = TRUE),
    median(compatibility_basin$tau_grace, na.rm = TRUE),
    median(compatibility_basin$p_lf_grace, na.rm = TRUE),
    median(compatibility_basin$enso_power_grace, na.rm = TRUE),
    median(compatibility_basin$qd_power_grace, na.rm = TRUE)
  ),
  CESM2_median = c(
    median(compatibility_basin$ar1_median_cesm, na.rm = TRUE),
    median(compatibility_basin$tau_median_cesm, na.rm = TRUE),
    median(compatibility_basin$p_lf_median_cesm, na.rm = TRUE),
    median(compatibility_basin$enso_power_median_cesm, na.rm = TRUE),
    median(compatibility_basin$qd_power_median_cesm, na.rm = TRUE)
  ),
  IPSL_median = c(
    median(compatibility_basin$ar1_median_ipsl, na.rm = TRUE),
    median(compatibility_basin$tau_median_ipsl, na.rm = TRUE),
    median(compatibility_basin$p_lf_median_ipsl, na.rm = TRUE),
    median(compatibility_basin$enso_power_median_ipsl, na.rm = TRUE),
    median(compatibility_basin$qd_power_median_ipsl, na.rm = TRUE)
  )
)

metric_comparison[, GRACE_CESM_ratio := round(GRACE_median / CESM2_median, 2)]
metric_comparison[, GRACE_IPSL_ratio := round(GRACE_median / IPSL_median, 2)]

print(metric_comparison)
cat("\n")

fwrite(metric_comparison, "outputs/phase08_metric_comparison.csv")
cat("  ✓ outputs/phase08_metric_comparison.csv\n\n")

# ============================================================================
# GENERATE FIGURES
# ============================================================================

cat("============================================================================\n")
cat("GENERATING FIGURES\n")
cat("============================================================================\n\n")

# Theme for publication
theme_pub <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey50"),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.position = "bottom"
  )

# ----------------------------------------------------------------------------
# FIGURE 1: Global Map of Compatibility Index
# ----------------------------------------------------------------------------

cat("Creating Figure 1: Global Compatibility Map...\n")

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create point data for basins (filter out NA coordinates)
basins_with_coords <- compatibility_basin[!is.na(lon) & !is.na(lat)]
basin_points <- st_as_sf(basins_with_coords,
                         coords = c("lon", "lat"),
                         crs = 4326)

# CESM2 map
p_cesm_map <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = basin_points, aes(color = C_b_cesm), size = 1.5) +
  scale_color_viridis_c(
    name = expression(C[b]),
    limits = c(0, 1),
    option = "plasma",
    na.value = "grey50"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "CESM2 Compatibility Index") +
  theme_pub +
  theme(legend.position = "right")

# IPSL map
p_ipsl_map <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = basin_points, aes(color = C_b_ipsl), size = 1.5) +
  scale_color_viridis_c(
    name = expression(C[b]),
    limits = c(0, 1),
    option = "plasma",
    na.value = "grey50"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "IPSL Compatibility Index") +
  theme_pub +
  theme(legend.position = "right")

# Combined figure
p_maps <- p_cesm_map / p_ipsl_map +
  plot_annotation(
    title = "GRACE-FO Compatibility with Climate Model Ensembles",
    subtitle = "12-metric Mahalanobis distance compatibility index (Cb)",
    caption = "Cb ≈ 0.5: GRACE at ensemble center (compatible)\nCb > 0.95 or < 0.05: GRACE is multivariate outlier (incompatible)"
  )

ggsave("outputs/figures_phase08/fig_compatibility_maps.png", p_maps,
       width = 10, height = 10, dpi = 300)
ggsave("outputs/figures_phase08/fig_compatibility_maps.pdf", p_maps,
       width = 10, height = 10)

cat("  ✓ fig_compatibility_maps.{png,pdf}\n")

# ----------------------------------------------------------------------------
# FIGURE 2: Compatibility Index Distributions
# ----------------------------------------------------------------------------

cat("Creating Figure 2: Compatibility Distributions...\n")

p_dist <- ggplot(compatibility_basin) +
  geom_histogram(aes(x = C_b_cesm, fill = "CESM2"), alpha = 0.6, bins = 30) +
  geom_histogram(aes(x = C_b_ipsl, fill = "IPSL"), alpha = 0.6, bins = 30) +
  geom_vline(xintercept = c(0.05, 0.95), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.5, linetype = "solid", color = "black") +
  scale_fill_manual(values = c("CESM2" = "#2166AC", "IPSL" = "#B2182B")) +
  labs(
    x = expression(Compatibility~Index~(C[b])),
    y = "Number of Basins",
    fill = "Model",
    title = "Distribution of Compatibility Indices",
    subtitle = "Red dashed: 5th/95th percentile thresholds"
  ) +
  theme_pub

ggsave("outputs/figures_phase08/fig_compatibility_distribution.png", p_dist,
       width = 8, height = 5, dpi = 300)
ggsave("outputs/figures_phase08/fig_compatibility_distribution.pdf", p_dist,
       width = 8, height = 5)

cat("  ✓ fig_compatibility_distribution.{png,pdf}\n")

# ----------------------------------------------------------------------------
# FIGURE 3: CESM2 vs IPSL Compatibility Scatter
# ----------------------------------------------------------------------------

cat("Creating Figure 3: Model Comparison Scatter...\n")

p_scatter <- ggplot(compatibility_basin, aes(x = C_b_cesm, y = C_b_ipsl)) +
  geom_point(aes(color = compat_class_combined), alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = c(0.05, 0.95), linetype = "dotted", color = "red", alpha = 0.5) +
  geom_hline(yintercept = c(0.05, 0.95), linetype = "dotted", color = "red", alpha = 0.5) +
  scale_color_manual(
    name = "Compatibility",
    values = c(
      "compatible_both" = "#1B9E77",
      "marginal_one" = "#D95F02",
      "marginal_both" = "#E7298A",
      "incompatible_one" = "#7570B3",
      "incompatible_both" = "#E6AB02"
    ),
    na.value = "grey70"
  ) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = expression(CESM2~Compatibility~Index~(C[b])),
    y = expression(IPSL~Compatibility~Index~(C[b])),
    title = "CESM2 vs IPSL Compatibility",
    subtitle = "Each point is one basin"
  ) +
  theme_pub

ggsave("outputs/figures_phase08/fig_compatibility_scatter.png", p_scatter,
       width = 7, height = 7, dpi = 300)
ggsave("outputs/figures_phase08/fig_compatibility_scatter.pdf", p_scatter,
       width = 7, height = 7)

cat("  ✓ fig_compatibility_scatter.{png,pdf}\n")

# ----------------------------------------------------------------------------
# FIGURE 4: Metric Comparison Bar Chart
# ----------------------------------------------------------------------------

cat("Creating Figure 4: Metric Comparison...\n")

metric_long <- melt(metric_comparison[, .(Metric, GRACE_median, CESM2_median, IPSL_median)],
                    id.vars = "Metric",
                    variable.name = "Source",
                    value.name = "Value")
metric_long[, Source := gsub("_median", "", Source)]

p_metrics <- ggplot(metric_long, aes(x = reorder(Metric, Value), y = Value, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("GRACE" = "#000000", "CESM2" = "#2166AC", "IPSL" = "#B2182B")) +
  coord_flip() +
  labs(
    x = "",
    y = "Median Value Across Basins",
    fill = "",
    title = "GRACE vs Model Ensemble Medians",
    subtitle = "12 metrics used in compatibility assessment"
  ) +
  theme_pub +
  theme(legend.position = "top")

ggsave("outputs/figures_phase08/fig_metric_comparison.png", p_metrics,
       width = 9, height = 7, dpi = 300)
ggsave("outputs/figures_phase08/fig_metric_comparison.pdf", p_metrics,
       width = 9, height = 7)

cat("  ✓ fig_metric_comparison.{png,pdf}\n")

# ----------------------------------------------------------------------------
# FIGURE 5: Mahalanobis Distance Distribution
# ----------------------------------------------------------------------------

cat("Creating Figure 5: Mahalanobis Distance Distribution...\n")

p_mahal <- ggplot(compatibility_basin) +
  geom_density(aes(x = d_mahal_cesm, fill = "CESM2"), alpha = 0.5) +
  geom_density(aes(x = d_mahal_ipsl, fill = "IPSL"), alpha = 0.5) +
  scale_fill_manual(values = c("CESM2" = "#2166AC", "IPSL" = "#B2182B")) +
  labs(
    x = "Mahalanobis Distance",
    y = "Density",
    fill = "Model",
    title = "Distribution of Mahalanobis Distances",
    subtitle = "Distance from GRACE to model ensemble centroid in 12D metric space"
  ) +
  theme_pub

ggsave("outputs/figures_phase08/fig_mahalanobis_distribution.png", p_mahal,
       width = 8, height = 5, dpi = 300)
ggsave("outputs/figures_phase08/fig_mahalanobis_distribution.pdf", p_mahal,
       width = 8, height = 5)

cat("  ✓ fig_mahalanobis_distribution.{png,pdf}\n")

# ----------------------------------------------------------------------------
# FIGURE 6: Classification Summary
# ----------------------------------------------------------------------------

cat("Creating Figure 6: Classification Summary...\n")

class_summary <- data.table(
  Model = c(rep("CESM2", 3), rep("IPSL", 3)),
  Class = rep(c("Compatible", "Marginal", "Incompatible"), 2),
  Count = c(
    sum(compatibility_basin$compat_class_cesm == "compatible", na.rm = TRUE),
    sum(compatibility_basin$compat_class_cesm == "marginal", na.rm = TRUE),
    sum(compatibility_basin$compat_class_cesm == "incompatible", na.rm = TRUE),
    sum(compatibility_basin$compat_class_ipsl == "compatible", na.rm = TRUE),
    sum(compatibility_basin$compat_class_ipsl == "marginal", na.rm = TRUE),
    sum(compatibility_basin$compat_class_ipsl == "incompatible", na.rm = TRUE)
  )
)
class_summary[, Percentage := round(100 * Count / sum(Count) * 2, 1), by = Model]

p_class <- ggplot(class_summary, aes(x = Model, y = Count, fill = Class)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(
    values = c("Compatible" = "#1B9E77", "Marginal" = "#D95F02", "Incompatible" = "#7570B3")
  ) +
  labs(
    x = "",
    y = "Number of Basins",
    fill = "Classification",
    title = "Basin Compatibility Classification",
    subtitle = "Based on 12-metric Mahalanobis distance"
  ) +
  theme_pub

ggsave("outputs/figures_phase08/fig_classification_summary.png", p_class,
       width = 6, height = 6, dpi = 300)
ggsave("outputs/figures_phase08/fig_classification_summary.pdf", p_class,
       width = 6, height = 6)

cat("  ✓ fig_classification_summary.{png,pdf}\n")

cat("\n✓ All figures saved to outputs/figures_phase08/\n\n")

# ============================================================================
# PUBLICATION SUMMARY
# ============================================================================

cat("============================================================================\n")
cat("PUBLICATION SUMMARY\n")
cat("============================================================================\n\n")

# Save publication statistics
pub_stats <- list(
  n_basins = n_total,
  n_metrics = 12,
  metric_names = METRIC_NAMES,

  # CESM2 statistics
  cesm2 = list(
    C_b_mean = mean(compatibility_basin$C_b_cesm, na.rm = TRUE),
    C_b_median = median(compatibility_basin$C_b_cesm, na.rm = TRUE),
    C_b_sd = sd(compatibility_basin$C_b_cesm, na.rm = TRUE),
    d_mahal_mean = mean(compatibility_basin$d_mahal_cesm, na.rm = TRUE),
    d_mahal_median = median(compatibility_basin$d_mahal_cesm, na.rm = TRUE),
    n_compatible = sum(compatibility_basin$compat_class_cesm == "compatible", na.rm = TRUE),
    n_marginal = sum(compatibility_basin$compat_class_cesm == "marginal", na.rm = TRUE),
    n_incompatible = sum(compatibility_basin$compat_class_cesm == "incompatible", na.rm = TRUE),
    pct_compatible = 100 * sum(compatibility_basin$compat_class_cesm == "compatible", na.rm = TRUE) / n_total,
    pct_incompatible = 100 * sum(compatibility_basin$compat_class_cesm == "incompatible", na.rm = TRUE) / n_total
  ),

  # IPSL statistics
  ipsl = list(
    C_b_mean = mean(compatibility_basin$C_b_ipsl, na.rm = TRUE),
    C_b_median = median(compatibility_basin$C_b_ipsl, na.rm = TRUE),
    C_b_sd = sd(compatibility_basin$C_b_ipsl, na.rm = TRUE),
    d_mahal_mean = mean(compatibility_basin$d_mahal_ipsl, na.rm = TRUE),
    d_mahal_median = median(compatibility_basin$d_mahal_ipsl, na.rm = TRUE),
    n_compatible = sum(compatibility_basin$compat_class_ipsl == "compatible", na.rm = TRUE),
    n_marginal = sum(compatibility_basin$compat_class_ipsl == "marginal", na.rm = TRUE),
    n_incompatible = sum(compatibility_basin$compat_class_ipsl == "incompatible", na.rm = TRUE),
    pct_compatible = 100 * sum(compatibility_basin$compat_class_ipsl == "compatible", na.rm = TRUE) / n_total,
    pct_incompatible = 100 * sum(compatibility_basin$compat_class_ipsl == "incompatible", na.rm = TRUE) / n_total
  ),

  # Combined statistics
  combined = list(
    n_compatible_both = sum(compatibility_basin$compat_class_combined == "compatible_both", na.rm = TRUE),
    n_incompatible_both = sum(compatibility_basin$compat_class_combined == "incompatible_both", na.rm = TRUE),
    pct_compatible_both = 100 * sum(compatibility_basin$compat_class_combined == "compatible_both", na.rm = TRUE) / n_total,
    pct_incompatible_both = 100 * sum(compatibility_basin$compat_class_combined == "incompatible_both", na.rm = TRUE) / n_total
  )
)

saveRDS(pub_stats, "outputs/phase08_publication_stats.rds")

cat("Key statistics for publication:\n\n")

cat(sprintf("CESM2: %.1f%% of basins (%d/%d) show GRACE observations that are \n",
            pub_stats$cesm2$pct_incompatible, pub_stats$cesm2$n_incompatible, n_total))
cat("incompatible with the model ensemble (Cb < 0.05 or > 0.95).\n\n")

cat(sprintf("IPSL: %.1f%% of basins (%d/%d) show incompatibility.\n\n",
            pub_stats$ipsl$pct_incompatible, pub_stats$ipsl$n_incompatible, n_total))

cat(sprintf("Combined: %.1f%% of basins (%d/%d) are incompatible with BOTH models.\n",
            pub_stats$combined$pct_incompatible_both, pub_stats$combined$n_incompatible_both, n_total))
cat(sprintf("Only %.1f%% of basins (%d/%d) are compatible with BOTH models.\n\n",
            pub_stats$combined$pct_compatible_both, pub_stats$combined$n_compatible_both, n_total))

cat("============================================================================\n")
cat("PHASE 08 COMPLETE\n")
cat("============================================================================\n\n")
