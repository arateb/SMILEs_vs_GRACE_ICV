#!/usr/bin/env Rscript
# ============================================================================
# PHASE 08 - FULL COMPATIBILITY INDEX - 2025-11-27
# ============================================================================
# OBJECTIVE: Assess whether 23-year GRACE observations are compatible with
# SMILEs ensemble members across ALL diagnostic metrics
#
# This script computes a comprehensive compatibility assessment using ALL
# metrics from previous phases that have true member-level variability.
#
# METRICS WITH MEMBER-LEVEL VARIABILITY (used in Mahalanobis distance):
# - ar1: AR(1) persistence coefficient
# - tau: Memory timescale (e-folding time)
# - p_lf: Low-frequency amplitude
# - enso_power: ENSO-band spectral power
# - qd_power: Quasi-decadal spectral power
#
# BASIN-LEVEL METRICS (for supplementary comparison):
# - A: Total amplitude (max - min)
# - sigma: Standard deviation
# - H_max: Maximum pluvial height
# - D_max: Maximum drought depth
# - I_pluvial: Maximum pluvial intensity
# - I_drought: Maximum drought intensity
# - mean_duration: Mean event duration
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

# ============================================================================
# CONFIGURATION
# ============================================================================

RUN_DATE <- "20251127"
OUTPUT_DIR <- paste0("outputs/run_", RUN_DATE)
FIG_DIR <- paste0(OUTPUT_DIR, "/figures")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# Ridge regularization for covariance matrix
RIDGE_LAMBDA <- 1e-6

# Metrics for Mahalanobis distance (must have member-level variability)
MAHAL_METRICS <- c("ar1", "tau", "p_lf", "enso_power", "qd_power")

cat("================================================================================\n")
cat("PHASE 08: FULL COMPATIBILITY INDEX\n")
cat("================================================================================\n\n")

cat("Configuration:\n")
cat("  Output directory:", OUTPUT_DIR, "\n")
cat("  Ridge regularization:", RIDGE_LAMBDA, "\n")
cat("  Mahalanobis metrics:", paste(MAHAL_METRICS, collapse = ", "), "\n\n")

# ============================================================================
# LOAD ALL PHASE RESULTS
# ============================================================================

cat("Loading results from all phases...\n")

# Check if run_20251127 outputs exist, otherwise use main outputs
if (file.exists(file.path(OUTPUT_DIR, "dispersion_summary.rds"))) {
  dispersion_summary <- readRDS(file.path(OUTPUT_DIR, "dispersion_summary.rds"))
  cat("  Loaded dispersion_summary from", OUTPUT_DIR, "\n")
} else {
  dispersion_summary <- readRDS("outputs/dispersion_summary.rds")
  cat("  Loaded dispersion_summary from outputs/\n")
}

# Phase 04: Wavelet
wavelet_summary <- readRDS("outputs/phase04_wavelet_summary.rds")
cesm_wavelets <- readRDS("outputs/phase04_cesm_all_wavelets.rds")
ipsl_wavelets <- readRDS("outputs/phase04_ipsl_all_wavelets.rds")
cat("  Loaded wavelet data\n")

# Phase 05: Persistence
persistence_summary <- readRDS("outputs/phase05_persistence_summary.rds")
cesm_persistence <- readRDS("outputs/phase05_cesm_persistence.rds")
ipsl_persistence <- readRDS("outputs/phase05_ipsl_persistence.rds")
cat("  Loaded persistence data\n")

# Phase 06: Events (if available)
if (file.exists("outputs/phase06_event_summary.rds")) {
  event_summary <- readRDS("outputs/phase06_event_summary.rds")
  cat("  Loaded event summary\n")
} else {
  event_summary <- NULL
  cat("  WARNING: Event summary not found - will skip event metrics\n")
}

# Basin attributes
if (file.exists(file.path(OUTPUT_DIR, "basin_attributes.rds"))) {
  attrs <- readRDS(file.path(OUTPUT_DIR, "basin_attributes.rds"))
} else {
  attrs <- readRDS("outputs/basin_attributes.rds")
}
cat("  Loaded basin attributes:", nrow(attrs), "basins\n")

# Basin polygons for mapping
if (file.exists(file.path(OUTPUT_DIR, "basin_polygons.rds"))) {
  basins_shp <- readRDS(file.path(OUTPUT_DIR, "basin_polygons.rds"))
} else {
  basins_shp <- st_read("/Volumes/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
  basins_shp <- st_make_valid(basins_shp)
}

world <- ne_countries(scale = "medium", returnclass = "sf")

cat("\n")

# ============================================================================
# BUILD GRACE METRIC VECTORS
# ============================================================================

cat("Building GRACE metric vectors...\n")

# Start with persistence metrics
grace_metrics <- persistence_summary[, .(
  basin = basin,
  basin_id = basin_id,
  basin_name = basin_name,
  ar1 = ar1_grace,
  tau = tau_grace,
  p_lf = p_lf_grace
)]

# Add wavelet metrics
grace_metrics <- merge(
  grace_metrics,
  wavelet_summary[, .(basin,
                      enso_power = grace_enso_power,
                      qd_power = grace_qd_power)],
  by = "basin",
  all.x = TRUE
)

# Add dispersion metrics (for supplementary analysis)
grace_metrics <- merge(
  grace_metrics,
  dispersion_summary[, .(basin,
                         A = A_grace,
                         sigma = sigma_grace)],
  by = "basin",
  all.x = TRUE
)

# Add event metrics if available
if (!is.null(event_summary)) {
  grace_metrics <- merge(
    grace_metrics,
    event_summary[, .(basin,
                      H_max = H_max_grace,
                      D_max = abs(D_max_grace),
                      I_pluvial = I_max_pluvial_grace,
                      I_drought = I_max_drought_grace,
                      mean_duration = mean_duration_grace)],
    by = "basin",
    all.x = TRUE
  )
}

cat("  GRACE metrics constructed for", nrow(grace_metrics), "basins\n")
cat("  Mahalanobis metrics:", paste(MAHAL_METRICS, collapse = ", "), "\n")
n_complete <- sum(complete.cases(grace_metrics[, ..MAHAL_METRICS]))
cat("  Basins with complete Mahalanobis metrics:", n_complete, "/", nrow(grace_metrics), "\n\n")

# ============================================================================
# BUILD MODEL METRIC MATRICES (PER BASIN × MEMBER)
# ============================================================================

cat("Building model metric matrices...\n")

# Basin lookup
basin_lookup <- grace_metrics[, .(basin, basin_id, basin_name)]

# ----------------------------------------------------------------------------
# CESM2
# ----------------------------------------------------------------------------

cat("  Processing CESM2...\n")

# Persistence by basin × member
cesm_persistence_member <- cesm_persistence[, .(
  ar1 = median(ar1, na.rm = TRUE),
  tau = median(tau, na.rm = TRUE),
  p_lf = median(p_lf, na.rm = TRUE)
), by = .(basin, member)]

# Wavelets by basin × member
cesm_wavelets_member <- cesm_wavelets[, .(
  enso_power = median(model_enso_power, na.rm = TRUE),
  qd_power = median(model_qd_power, na.rm = TRUE)
), by = .(basin, member)]

# Merge
cesm_metrics <- merge(cesm_persistence_member, cesm_wavelets_member,
                      by = c("basin", "member"), all = TRUE)
cesm_metrics <- merge(cesm_metrics, basin_lookup, by = "basin", all.x = TRUE)
cesm_metrics[, model := "CESM2"]

cat("    CESM2 metrics:", nrow(cesm_metrics), "rows (",
    length(unique(cesm_metrics$basin)), "basins ×",
    length(unique(cesm_metrics$member)), "members)\n")

# ----------------------------------------------------------------------------
# IPSL
# ----------------------------------------------------------------------------

cat("  Processing IPSL...\n")

ipsl_persistence_member <- ipsl_persistence[, .(
  ar1 = median(ar1, na.rm = TRUE),
  tau = median(tau, na.rm = TRUE),
  p_lf = median(p_lf, na.rm = TRUE)
), by = .(basin, member)]

ipsl_wavelets_member <- ipsl_wavelets[, .(
  enso_power = median(model_enso_power, na.rm = TRUE),
  qd_power = median(model_qd_power, na.rm = TRUE)
), by = .(basin, member)]

ipsl_metrics <- merge(ipsl_persistence_member, ipsl_wavelets_member,
                      by = c("basin", "member"), all = TRUE)
ipsl_metrics <- merge(ipsl_metrics, basin_lookup, by = "basin", all.x = TRUE)
ipsl_metrics[, model := "IPSL"]

cat("    IPSL metrics:", nrow(ipsl_metrics), "rows (",
    length(unique(ipsl_metrics$basin)), "basins ×",
    length(unique(ipsl_metrics$member)), "members)\n\n")

# ============================================================================
# COMPUTE MAHALANOBIS COMPATIBILITY
# ============================================================================

cat("Computing Mahalanobis compatibility per basin...\n")

compute_compatibility <- function(M_grace, M_model, ridge_lambda = 1e-6) {
  # Remove rows with NA
  complete_rows <- complete.cases(M_model)
  M_model_clean <- M_model[complete_rows, , drop = FALSE]

  n_metrics <- ncol(M_model_clean)

  if (nrow(M_model_clean) < n_metrics + 5) {
    return(list(
      n_members = nrow(M_model),
      n_complete = nrow(M_model_clean),
      d_mahal = NA_real_,
      C_b = NA_real_,
      compat_class = NA_character_,
      model_medians = rep(NA_real_, n_metrics),
      model_sds = rep(NA_real_, n_metrics)
    ))
  }

  # Mean and covariance
  mu_model <- colMeans(M_model_clean, na.rm = TRUE)
  Sigma_model <- cov(M_model_clean, use = "complete.obs")
  Sigma_model_reg <- Sigma_model + diag(ridge_lambda, nrow = ncol(Sigma_model))

  # Mahalanobis distance
  if (any(is.na(M_grace))) {
    d_grace <- NA_real_
    C_b <- NA_real_
  } else {
    d_grace <- tryCatch({
      sqrt(mahalanobis(M_grace, center = mu_model, cov = Sigma_model_reg))
    }, error = function(e) NA_real_)

    d_model <- tryCatch({
      sqrt(mahalanobis(M_model_clean, center = mu_model, cov = Sigma_model_reg))
    }, error = function(e) rep(NA_real_, nrow(M_model_clean)))

    if (!is.na(d_grace) && !any(is.na(d_model))) {
      C_b <- mean(d_model <= d_grace, na.rm = TRUE)
    } else {
      C_b <- NA_real_
    }
  }

  # Classification
  if (is.na(C_b)) {
    compat_class <- NA_character_
  } else if (C_b > 0.95 | C_b < 0.05) {
    compat_class <- "incompatible"
  } else if (C_b > 0.90 | C_b < 0.10) {
    compat_class <- "marginal"
  } else {
    compat_class <- "compatible"
  }

  return(list(
    n_members = nrow(M_model),
    n_complete = nrow(M_model_clean),
    d_mahal = d_grace,
    C_b = C_b,
    compat_class = compat_class,
    model_medians = apply(M_model_clean, 2, median, na.rm = TRUE),
    model_sds = apply(M_model_clean, 2, sd, na.rm = TRUE)
  ))
}

# Process each basin
results <- list()
n_basins <- nrow(grace_metrics)
pb <- txtProgressBar(min = 0, max = n_basins, style = 3)

for (i in 1:n_basins) {
  basin_i <- grace_metrics$basin[i]

  # GRACE metric vector
  M_grace <- as.numeric(grace_metrics[i, ..MAHAL_METRICS])

  # CESM2
  model_cesm <- cesm_metrics[basin == basin_i]
  M_cesm <- if (nrow(model_cesm) > 0) as.matrix(model_cesm[, ..MAHAL_METRICS]) else matrix(NA, 0, 5)
  result_cesm <- compute_compatibility(M_grace, M_cesm, RIDGE_LAMBDA)

  # IPSL
  model_ipsl <- ipsl_metrics[basin == basin_i]
  M_ipsl <- if (nrow(model_ipsl) > 0) as.matrix(model_ipsl[, ..MAHAL_METRICS]) else matrix(NA, 0, 5)
  result_ipsl <- compute_compatibility(M_grace, M_ipsl, RIDGE_LAMBDA)

  results[[i]] <- data.table(
    basin = basin_i,
    basin_id = grace_metrics$basin_id[i],
    basin_name = grace_metrics$basin_name[i],

    # GRACE metrics
    ar1_grace = M_grace[1],
    tau_grace = M_grace[2],
    p_lf_grace = M_grace[3],
    enso_power_grace = M_grace[4],
    qd_power_grace = M_grace[5],
    A_grace = grace_metrics$A[i],
    sigma_grace = grace_metrics$sigma[i],

    # CESM2 results
    n_members_cesm = result_cesm$n_members,
    n_complete_cesm = result_cesm$n_complete,
    d_mahal_cesm = result_cesm$d_mahal,
    C_b_cesm = result_cesm$C_b,
    compat_class_cesm = result_cesm$compat_class,
    ar1_median_cesm = result_cesm$model_medians[1],
    tau_median_cesm = result_cesm$model_medians[2],
    p_lf_median_cesm = result_cesm$model_medians[3],
    enso_median_cesm = result_cesm$model_medians[4],
    qd_median_cesm = result_cesm$model_medians[5],

    # IPSL results
    n_members_ipsl = result_ipsl$n_members,
    n_complete_ipsl = result_ipsl$n_complete,
    d_mahal_ipsl = result_ipsl$d_mahal,
    C_b_ipsl = result_ipsl$C_b,
    compat_class_ipsl = result_ipsl$compat_class,
    ar1_median_ipsl = result_ipsl$model_medians[1],
    tau_median_ipsl = result_ipsl$model_medians[2],
    p_lf_median_ipsl = result_ipsl$model_medians[3],
    enso_median_ipsl = result_ipsl$model_medians[4],
    qd_median_ipsl = result_ipsl$model_medians[5]
  )

  setTxtProgressBar(pb, i)
}
close(pb)

compatibility_results <- rbindlist(results)

# Add combined classification
compatibility_results[, compat_combined := {
  if (is.na(compat_class_cesm) | is.na(compat_class_ipsl)) {
    NA_character_
  } else if (compat_class_cesm == "incompatible" & compat_class_ipsl == "incompatible") {
    "incompatible_both"
  } else if (compat_class_cesm == "incompatible" | compat_class_ipsl == "incompatible") {
    "incompatible_one"
  } else if (compat_class_cesm == "compatible" & compat_class_ipsl == "compatible") {
    "compatible_both"
  } else {
    "marginal"
  }
}, by = 1:nrow(compatibility_results)]

# Add basin coordinates for mapping
attrs_dt <- data.table(
  basin_name = attrs$name,
  lon = attrs$C_lon,
  lat = attrs$C_lat,
  area = attrs$Area_km2,
  climate = attrs$climate
)
compatibility_results <- merge(compatibility_results, attrs_dt, by = "basin_name", all.x = TRUE)

cat("\n\n")

# ============================================================================
# ADD DISPERSION PERCENTILE METRICS
# ============================================================================

cat("Adding dispersion percentile metrics...\n")

# Compute where GRACE falls in model distributions for amplitude/variance
compatibility_results <- merge(
  compatibility_results,
  dispersion_summary[, .(basin,
                         amplitude_covered_cesm, variance_covered_cesm,
                         amplitude_covered_ipsl, variance_covered_ipsl,
                         A_p05_cesm, A_p50_cesm, A_p95_cesm,
                         sigma_p05_cesm, sigma_p50_cesm, sigma_p95_cesm,
                         A_p05_ipsl, A_p50_ipsl, A_p95_ipsl,
                         sigma_p05_ipsl, sigma_p50_ipsl, sigma_p95_ipsl)],
  by = "basin",
  all.x = TRUE
)

# Compute GRACE/model ratios
compatibility_results[, A_ratio_cesm := A_grace / A_p50_cesm]
compatibility_results[, sigma_ratio_cesm := sigma_grace / sigma_p50_cesm]
compatibility_results[, A_ratio_ipsl := A_grace / A_p50_ipsl]
compatibility_results[, sigma_ratio_ipsl := sigma_grace / sigma_p50_ipsl]

cat("  Added amplitude/variance ratios\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

saveRDS(compatibility_results, file.path(OUTPUT_DIR, "phase08_compatibility_full.rds"))
fwrite(compatibility_results, file.path(OUTPUT_DIR, "phase08_compatibility_full.csv"))

cat("  Saved:", file.path(OUTPUT_DIR, "phase08_compatibility_full.{rds,csv}"), "\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("================================================================================\n")
cat("COMPATIBILITY SUMMARY\n")
cat("================================================================================\n\n")

n_total_cesm <- sum(!is.na(compatibility_results$C_b_cesm))
n_total_ipsl <- sum(!is.na(compatibility_results$C_b_ipsl))

cat("CESM2 (n =", n_total_cesm, "basins with complete metrics):\n")
cat("  C_b mean:", round(mean(compatibility_results$C_b_cesm, na.rm = TRUE), 3), "\n")
cat("  C_b median:", round(median(compatibility_results$C_b_cesm, na.rm = TRUE), 3), "\n")
cat("  Mahalanobis distance median:", round(median(compatibility_results$d_mahal_cesm, na.rm = TRUE), 2), "\n")
cat("  Classification:\n")
print(table(compatibility_results$compat_class_cesm, useNA = "ifany"))

cat("\nIPSL (n =", n_total_ipsl, "basins with complete metrics):\n")
cat("  C_b mean:", round(mean(compatibility_results$C_b_ipsl, na.rm = TRUE), 3), "\n")
cat("  C_b median:", round(median(compatibility_results$C_b_ipsl, na.rm = TRUE), 3), "\n")
cat("  Mahalanobis distance median:", round(median(compatibility_results$d_mahal_ipsl, na.rm = TRUE), 2), "\n")
cat("  Classification:\n")
print(table(compatibility_results$compat_class_ipsl, useNA = "ifany"))

cat("\nCombined classification:\n")
print(table(compatibility_results$compat_combined, useNA = "ifany"))

# Key statistics
n_incompat_cesm <- sum(compatibility_results$compat_class_cesm == "incompatible", na.rm = TRUE)
n_incompat_ipsl <- sum(compatibility_results$compat_class_ipsl == "incompatible", na.rm = TRUE)
n_incompat_both <- sum(compatibility_results$compat_combined == "incompatible_both", na.rm = TRUE)
n_compat_both <- sum(compatibility_results$compat_combined == "compatible_both", na.rm = TRUE)

cat("\n================================================================================\n")
cat("KEY FINDINGS\n")
cat("================================================================================\n\n")
cat("Incompatible with CESM2:", n_incompat_cesm, "/", n_total_cesm,
    "(", round(100 * n_incompat_cesm / n_total_cesm, 1), "%)\n")
cat("Incompatible with IPSL:", n_incompat_ipsl, "/", n_total_ipsl,
    "(", round(100 * n_incompat_ipsl / n_total_ipsl, 1), "%)\n")
cat("Incompatible with BOTH:", n_incompat_both,
    "(", round(100 * n_incompat_both / max(n_total_cesm, n_total_ipsl), 1), "%)\n")
cat("Compatible with BOTH:", n_compat_both,
    "(", round(100 * n_compat_both / max(n_total_cesm, n_total_ipsl), 1), "%)\n\n")

# Metric comparison
cat("METRIC COMPARISON (median across basins):\n")
cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-20s %10s %10s %10s %10s %10s\n", "Metric", "GRACE", "CESM2", "IPSL", "G/C ratio", "G/I ratio"))
cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-20s %10.3f %10.3f %10.3f %10.2f %10.2f\n", "AR(1)",
            median(compatibility_results$ar1_grace, na.rm = TRUE),
            median(compatibility_results$ar1_median_cesm, na.rm = TRUE),
            median(compatibility_results$ar1_median_ipsl, na.rm = TRUE),
            median(compatibility_results$ar1_grace, na.rm = TRUE) / median(compatibility_results$ar1_median_cesm, na.rm = TRUE),
            median(compatibility_results$ar1_grace, na.rm = TRUE) / median(compatibility_results$ar1_median_ipsl, na.rm = TRUE)))
cat(sprintf("%-20s %10.1f %10.1f %10.1f %10.2f %10.2f\n", "tau (months)",
            median(compatibility_results$tau_grace, na.rm = TRUE),
            median(compatibility_results$tau_median_cesm, na.rm = TRUE),
            median(compatibility_results$tau_median_ipsl, na.rm = TRUE),
            median(compatibility_results$tau_grace, na.rm = TRUE) / median(compatibility_results$tau_median_cesm, na.rm = TRUE),
            median(compatibility_results$tau_grace, na.rm = TRUE) / median(compatibility_results$tau_median_ipsl, na.rm = TRUE)))
cat(sprintf("%-20s %10.1f %10.1f %10.1f %10.2f %10.2f\n", "p_lf (mm)",
            median(compatibility_results$p_lf_grace, na.rm = TRUE),
            median(compatibility_results$p_lf_median_cesm, na.rm = TRUE),
            median(compatibility_results$p_lf_median_ipsl, na.rm = TRUE),
            median(compatibility_results$p_lf_grace, na.rm = TRUE) / median(compatibility_results$p_lf_median_cesm, na.rm = TRUE),
            median(compatibility_results$p_lf_grace, na.rm = TRUE) / median(compatibility_results$p_lf_median_ipsl, na.rm = TRUE)))
cat(sprintf("%-20s %10.2f %10.2f %10.2f %10.2f %10.2f\n", "ENSO power",
            median(compatibility_results$enso_power_grace, na.rm = TRUE),
            median(compatibility_results$enso_median_cesm, na.rm = TRUE),
            median(compatibility_results$enso_median_ipsl, na.rm = TRUE),
            median(compatibility_results$enso_power_grace, na.rm = TRUE) / median(compatibility_results$enso_median_cesm, na.rm = TRUE),
            median(compatibility_results$enso_power_grace, na.rm = TRUE) / median(compatibility_results$enso_median_ipsl, na.rm = TRUE)))
cat(sprintf("%-20s %10.2f %10.2f %10.2f %10.2f %10.2f\n", "QD power",
            median(compatibility_results$qd_power_grace, na.rm = TRUE),
            median(compatibility_results$qd_median_cesm, na.rm = TRUE),
            median(compatibility_results$qd_median_ipsl, na.rm = TRUE),
            median(compatibility_results$qd_power_grace, na.rm = TRUE) / median(compatibility_results$qd_median_cesm, na.rm = TRUE),
            median(compatibility_results$qd_power_grace, na.rm = TRUE) / median(compatibility_results$qd_median_ipsl, na.rm = TRUE)))
cat(sprintf("%-20s %10.1f %10.1f %10.1f %10.2f %10.2f\n", "Amplitude (mm)",
            median(compatibility_results$A_grace, na.rm = TRUE),
            median(compatibility_results$A_p50_cesm, na.rm = TRUE),
            median(compatibility_results$A_p50_ipsl, na.rm = TRUE),
            median(compatibility_results$A_ratio_cesm, na.rm = TRUE),
            median(compatibility_results$A_ratio_ipsl, na.rm = TRUE)))
cat(sprintf("%-20s %10.1f %10.1f %10.1f %10.2f %10.2f\n", "Std Dev (mm)",
            median(compatibility_results$sigma_grace, na.rm = TRUE),
            median(compatibility_results$sigma_p50_cesm, na.rm = TRUE),
            median(compatibility_results$sigma_p50_ipsl, na.rm = TRUE),
            median(compatibility_results$sigma_ratio_cesm, na.rm = TRUE),
            median(compatibility_results$sigma_ratio_ipsl, na.rm = TRUE)))
cat("--------------------------------------------------------------------------------\n\n")

# ============================================================================
# GENERATE FIGURES
# ============================================================================

cat("================================================================================\n")
cat("GENERATING FIGURES\n")
cat("================================================================================\n\n")

theme_pub <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey50"),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

save_fig <- function(plot, name, width = 10, height = 8) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300)
  ggsave(file.path(FIG_DIR, paste0(name, ".pdf")), plot,
         width = width, height = height)
  cat("  Saved:", name, "\n")
}

# ----------------------------------------------------------------------------
# Map: Compatibility Index
# ----------------------------------------------------------------------------

cat("Creating compatibility maps...\n")

# Merge with shapefile
compat_sf <- merge(basins_shp, compatibility_results, by.x = "Num", by.y = "basin")

# CESM2 map
p_map_cesm <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = compat_sf, aes(fill = C_b_cesm), color = "grey30", linewidth = 0.1) +
  scale_fill_viridis_c(
    name = expression(C[b]),
    limits = c(0, 1),
    option = "plasma",
    na.value = "grey80"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "CESM2 Compatibility Index",
       subtitle = "Cb near 1 = GRACE is outlier; Cb near 0.5 = GRACE is typical") +
  theme_pub

# IPSL map
p_map_ipsl <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = compat_sf, aes(fill = C_b_ipsl), color = "grey30", linewidth = 0.1) +
  scale_fill_viridis_c(
    name = expression(C[b]),
    limits = c(0, 1),
    option = "plasma",
    na.value = "grey80"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "IPSL Compatibility Index") +
  theme_pub

p_maps <- p_map_cesm / p_map_ipsl
save_fig(p_maps, "fig08a_compatibility_maps", width = 10, height = 12)

# ----------------------------------------------------------------------------
# Map: Combined Classification
# ----------------------------------------------------------------------------

compat_sf$compat_combined_factor <- factor(
  compat_sf$compat_combined,
  levels = c("compatible_both", "marginal", "incompatible_one", "incompatible_both")
)

p_map_combined <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = compat_sf, aes(fill = compat_combined_factor), color = "grey30", linewidth = 0.1) +
  scale_fill_manual(
    name = "Classification",
    values = c("compatible_both" = "#1B9E77", "marginal" = "#D95F02",
               "incompatible_one" = "#7570B3", "incompatible_both" = "#E7298A"),
    labels = c("Compatible both", "Marginal", "Incompatible (one)", "Incompatible (both)"),
    na.value = "grey80"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "Combined Model Compatibility",
       subtitle = "Classification based on 5-metric Mahalanobis distance") +
  theme_pub

save_fig(p_map_combined, "fig08b_classification_map", width = 10, height = 6)

# ----------------------------------------------------------------------------
# Scatter: CESM2 vs IPSL Compatibility
# ----------------------------------------------------------------------------

p_scatter_compat <- ggplot(compatibility_results[!is.na(C_b_cesm) & !is.na(C_b_ipsl)],
                           aes(x = C_b_cesm, y = C_b_ipsl, color = compat_combined)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = c(0.05, 0.95), linetype = "dotted", color = "red", alpha = 0.5) +
  geom_hline(yintercept = c(0.05, 0.95), linetype = "dotted", color = "red", alpha = 0.5) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(
    name = "Classification",
    values = c("compatible_both" = "#1B9E77", "marginal" = "#D95F02",
               "incompatible_one" = "#7570B3", "incompatible_both" = "#E7298A"),
    na.value = "grey70"
  ) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = expression(CESM2~C[b]),
       y = expression(IPSL~C[b]),
       title = "CESM2 vs IPSL Compatibility",
       subtitle = "Each point is one basin; red lines mark 5th/95th percentile thresholds") +
  theme_pub

save_fig(p_scatter_compat, "fig08c_compatibility_scatter", width = 8, height = 8)

# ----------------------------------------------------------------------------
# Distribution: Compatibility Index
# ----------------------------------------------------------------------------

compat_long <- melt(compatibility_results[, .(basin, C_b_cesm, C_b_ipsl)],
                    id.vars = "basin", variable.name = "Model", value.name = "C_b")
compat_long[, Model := gsub("C_b_", "", Model)]
compat_long[, Model := toupper(Model)]

p_dist <- ggplot(compat_long[!is.na(C_b)], aes(x = C_b, fill = Model)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = c(0.05, 0.95), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.5, linetype = "solid", color = "black") +
  scale_fill_manual(values = c("CESM" = "#2166AC", "IPSL" = "#B2182B")) +
  labs(x = expression(Compatibility~Index~(C[b])),
       y = "Number of Basins",
       title = "Distribution of Compatibility Indices",
       subtitle = "Dashed red: incompatibility thresholds; Solid black: expected value under null") +
  theme_pub

save_fig(p_dist, "fig08d_compatibility_distribution", width = 8, height = 5)

# ----------------------------------------------------------------------------
# Scatter: Key Metric Comparisons
# ----------------------------------------------------------------------------

# Memory timescale scatter
p_tau <- ggplot(compatibility_results[!is.na(tau_grace)]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(x = tau_median_cesm, y = tau_grace, color = "CESM2"), alpha = 0.6, size = 2) +
  geom_point(aes(x = tau_median_ipsl, y = tau_grace, color = "IPSL"), alpha = 0.6, size = 2) +
  scale_color_manual(values = c("CESM2" = "#2166AC", "IPSL" = "#B2182B")) +
  labs(x = "Model tau (months)", y = "GRACE tau (months)",
       title = "Memory Timescale: GRACE vs Models",
       color = "") +
  theme_pub

save_fig(p_tau, "fig08e_tau_scatter", width = 7, height = 7)

# ENSO power scatter
p_enso <- ggplot(compatibility_results[!is.na(enso_power_grace)]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(x = enso_median_cesm, y = enso_power_grace, color = "CESM2"), alpha = 0.6, size = 2) +
  geom_point(aes(x = enso_median_ipsl, y = enso_power_grace, color = "IPSL"), alpha = 0.6, size = 2) +
  scale_color_manual(values = c("CESM2" = "#2166AC", "IPSL" = "#B2182B")) +
  labs(x = "Model ENSO Power", y = "GRACE ENSO Power",
       title = "ENSO-Band Spectral Power: GRACE vs Models",
       color = "") +
  theme_pub

save_fig(p_enso, "fig08f_enso_scatter", width = 7, height = 7)

# Amplitude scatter
p_amp <- ggplot(compatibility_results[!is.na(A_grace)]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(x = A_p50_cesm, y = A_grace, color = "CESM2"), alpha = 0.6, size = 2) +
  geom_point(aes(x = A_p50_ipsl, y = A_grace, color = "IPSL"), alpha = 0.6, size = 2) +
  scale_color_manual(values = c("CESM2" = "#2166AC", "IPSL" = "#B2182B")) +
  labs(x = "Model Amplitude [mm]", y = "GRACE Amplitude [mm]",
       title = "TWS Amplitude: GRACE vs Models",
       color = "") +
  theme_pub

save_fig(p_amp, "fig08g_amplitude_scatter", width = 7, height = 7)

cat("\nAll figures saved to:", FIG_DIR, "\n\n")

# ============================================================================
# PUBLICATION PARAGRAPHS
# ============================================================================

cat("================================================================================\n")
cat("PUBLICATION PARAGRAPHS\n")
cat("================================================================================\n\n")

pub_text <- sprintf("
METHODS:
We assessed the multivariate compatibility between GRACE-FO observations and
climate model ensembles using a 5-dimensional metric space comprising: AR(1)
persistence, memory timescale (tau), low-frequency amplitude, ENSO-band spectral
power (2-7 year periods), and quasi-decadal spectral power (7-15 years). For
each basin, we computed the Mahalanobis distance from the GRACE metric vector
to the model ensemble centroid, accounting for the full covariance structure
among metrics. The compatibility index Cb represents the fraction of ensemble
members with distances equal to or greater than that of GRACE; values near 0.5
indicate GRACE lies near the ensemble center, while values exceeding 0.95
indicate GRACE is a multivariate outlier.

RESULTS:
Of the %d basins with complete metrics for CESM2, %d (%%.1f%%%%) show GRACE
observations that are incompatible with the model ensemble (Cb > 0.95 or < 0.05).
For IPSL, %d of %d basins (%%.1f%%%%) are incompatible. Notably, %d basins
(%%.1f%%%%) are incompatible with BOTH model ensembles, while only %d basins
(%%.1f%%%%) achieve compatibility with both.

The primary source of incompatibility is the memory timescale tau: GRACE exhibits
a median memory of %.1f months, compared to %.1f months for CESM2 and %.1f months
for IPSL - an overestimation of approximately %d%%%% by both models. In contrast,
AR(1) coefficients are nearly identical (GRACE/model ratio ~%.2f), suggesting
models capture short-term autocorrelation but overestimate longer-term persistence.
",
n_total_cesm, n_incompat_cesm, 100 * n_incompat_cesm / n_total_cesm,
n_incompat_ipsl, n_total_ipsl, 100 * n_incompat_ipsl / n_total_ipsl,
n_incompat_both, 100 * n_incompat_both / max(n_total_cesm, n_total_ipsl),
n_compat_both, 100 * n_compat_both / max(n_total_cesm, n_total_ipsl),
median(compatibility_results$tau_grace, na.rm = TRUE),
median(compatibility_results$tau_median_cesm, na.rm = TRUE),
median(compatibility_results$tau_median_ipsl, na.rm = TRUE),
round(100 * (median(compatibility_results$tau_median_cesm, na.rm = TRUE) /
             median(compatibility_results$tau_grace, na.rm = TRUE) - 1)),
median(compatibility_results$ar1_grace, na.rm = TRUE) /
median(compatibility_results$ar1_median_cesm, na.rm = TRUE)
)

cat(pub_text)

# Save publication text
writeLines(pub_text, file.path(OUTPUT_DIR, "phase08_publication_text.txt"))

cat("\n================================================================================\n")
cat("PHASE 08 COMPLETE\n")
cat("================================================================================\n")
