#!/usr/bin/env Rscript
# ============================================================================
# PHASE 04 - WAVELET MODE MATCHING ANALYSIS
# ============================================================================
# Per-member wavelet analysis: match GRACE modes and explore member oscillations
# Author: Ashraf Rateb
# Date: 2025-11-19
# ============================================================================

library(tidyverse)
library(data.table)
library(WaveletComp)
library(parallel)
library(foreach)
library(doParallel)

# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Setup parallel processing
n_cores <- detectCores() - 1  # Leave 1 core free
cl <- makeCluster(n_cores)
registerDoParallel(cl)
cat("Using", n_cores, "cores for parallel processing\n\n")

# Load utilities
source("src/functions/utils_wavelet_nov2025.R")

cat("============================================================================\n")
cat("PHASE 04: WAVELET MODE MATCHING - PER-MEMBER ANALYSIS\n")
cat("============================================================================\n\n")

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

# Extract GRACE data and attrs from raw
attrs <- raw_data$attrs
gfo_dtrend <- raw_data$gfo_dtrend
dates_grace <- raw_data$dates_grace

# Extract FILTERED model data from Phase 1
G_CESM_filtered <- filtered_data$G_CESM_filtered
dates_cesm <- filtered_data$date_cesm
G_IPSL_combined <- filtered_data$G_IPSL_combined
dates_ipsl <- filtered_data$date_ipsl

cat("  CESM2 filtered dimensions:", dim(G_CESM_filtered), "\n")
cat("  IPSL combined filtered dimensions:", dim(G_IPSL_combined), "\n")
cat("  Basins:", nrow(attrs), "\n\n")

# ============================================================================
# COMPUTE GRACE WAVELET METRICS
# ============================================================================

cat("Computing GRACE wavelet metrics...\n")

grace_wavelet_list <- list()
grace_spectra_list <- list()

pb_grace <- txtProgressBar(min = 0, max = nrow(attrs), style = 3)

for (i in 1:nrow(attrs)) {
  basin_id <- attrs$ID[i]
  basin_name <- attrs$name[i]

  # Extract GRACE time series
  # gfo_dtrend is a list with $median (dataframe where each column is a basin)
  grace_series <- gfo_dtrend$median[[basin_name]]
  grace_dates <- dates_grace

  # Compute wavelet metrics (dt = 1/12 for monthly data → periods in YEARS)
  wm <- compute_wavelet_metrics(grace_series, grace_dates, dt = 1/12, omega0 = 6)

  grace_wavelet_list[[i]] <- data.table(
    basin = i,
    basin_id = basin_id,
    basin_name = basin_name,
    dominant_period_1 = wm$dominant_periods[1],
    dominant_period_2 = wm$dominant_periods[2],
    dominant_period_3 = wm$dominant_periods[3],
    dominant_power_1 = wm$dominant_powers[1],
    dominant_power_2 = wm$dominant_powers[2],
    dominant_power_3 = wm$dominant_powers[3],
    band_class_1 = wm$band_class[1],
    band_class_2 = wm$band_class[2],
    band_class_3 = wm$band_class[3],
    success = wm$success
  )

  # Save GRACE global spectrum
  if (wm$success && !is.null(wm$global_spectrum)) {
    spectrum_dt <- as.data.table(wm$global_spectrum)
    spectrum_dt[, `:=`(basin = i,
                      basin_id = basin_id,
                      basin_name = basin_name)]
    grace_spectra_list[[i]] <- spectrum_dt
  }

  setTxtProgressBar(pb_grace, i)
}

close(pb_grace)

grace_wavelet <- rbindlist(grace_wavelet_list)
grace_spectra <- rbindlist(grace_spectra_list, fill = TRUE)

cat("\n✓ GRACE wavelet analysis complete\n")
cat("  Successful analyses:", sum(grace_wavelet$success), "/", nrow(grace_wavelet), "\n\n")

cat("✓ Using filtered data from Phase 1 (already deforced + STL filtered)\n\n")

# Export functions to cluster (once for both CESM2 and IPSL)
clusterExport(cl, c("compute_wavelet_metrics", "classify_period_band"))

# ============================================================================
# CESM2 FULL-SERIES WAVELET ANALYSIS (ALL 201 YEARS)
# ============================================================================

cat("============================================================================\n")
cat("CESM2 FULL-SERIES WAVELET ANALYSIS\n")
cat("============================================================================\n\n")

cat("Analyzing FULL 201-year time series for each member...\n")
cat("  80 members × 184 basins\n")
cat("  Processing basins in parallel...\n\n")

n_basins <- dim(G_CESM_filtered)[1]
n_members_cesm <- dim(G_CESM_filtered)[2]
n_times_cesm <- dim(G_CESM_filtered)[3]

# Parallel processing across basins
cat("Processing", n_basins, "basins in parallel...\n")

results <- foreach(basin = 1:n_basins, .packages = c("data.table", "WaveletComp")) %dopar% {

  tryCatch({
    # Skip if GRACE analysis failed
    if (!grace_wavelet$success[basin]) {
      return(NULL)
    }

    # Extract basin data [members × time] - FULL SERIES
    basin_data <- G_CESM_filtered[basin, , ]

    # Storage for all members' full-series wavelets and spectra
    all_wavelets_list <- list()
    all_spectra_list <- list()

    for (member in 1:n_members_cesm) {
      member_series <- basin_data[member, ]

      # Compute wavelet metrics on FULL time series (dt = 1/12 for monthly data → periods in YEARS)
      wm <- compute_wavelet_metrics(member_series, dt = 1/12, omega0 = 6)

      if (wm$success) {
        all_wavelets_list[[length(all_wavelets_list) + 1]] <- data.table(
          basin = basin,
          basin_id = attrs$ID[basin],
          basin_name = attrs$name[basin],
          member = member,
          dominant_period_1 = wm$dominant_periods[1],
          dominant_period_2 = wm$dominant_periods[2],
          dominant_period_3 = wm$dominant_periods[3],
          dominant_power_1 = wm$dominant_powers[1],
          dominant_power_2 = wm$dominant_powers[2],
          dominant_power_3 = wm$dominant_powers[3],
          band_class_1 = wm$band_class[1],
          band_class_2 = wm$band_class[2],
          band_class_3 = wm$band_class[3],
          success = TRUE
        )

        # Save global spectrum
        if (!is.null(wm$global_spectrum)) {
          spectrum_dt <- as.data.table(wm$global_spectrum)
          spectrum_dt[, `:=`(basin = basin,
                            basin_id = attrs$ID[basin],
                            basin_name = attrs$name[basin],
                            member = member)]
          all_spectra_list[[length(all_spectra_list) + 1]] <- spectrum_dt
        }
      }
    }

    list(
      all_wavelets = if (length(all_wavelets_list) > 0) rbindlist(all_wavelets_list) else NULL,
      all_spectra = if (length(all_spectra_list) > 0) rbindlist(all_spectra_list) else NULL
    )

  }, error = function(e) {
    NULL
  })
}

# Extract results
cesm_all_wavelets_list <- lapply(results, function(x) if (!is.null(x)) x$all_wavelets else NULL)
cesm_all_spectra_list <- lapply(results, function(x) if (!is.null(x)) x$all_spectra else NULL)

# Combine wavelets
cesm_all_wavelets_list <- cesm_all_wavelets_list[!sapply(cesm_all_wavelets_list, is.null)]

if (length(cesm_all_wavelets_list) == 0) {
  stop("No CESM2 wavelets were generated. Check GRACE wavelet analysis.")
}

cesm_all_wavelets <- rbindlist(cesm_all_wavelets_list, fill = TRUE)

# Combine spectra
cesm_all_spectra_list <- cesm_all_spectra_list[!sapply(cesm_all_spectra_list, is.null)]
cesm_all_spectra <- if (length(cesm_all_spectra_list) > 0) rbindlist(cesm_all_spectra_list, fill = TRUE) else NULL

# Attach full basin attributes
attrs_dt <- data.table(attrs)
attrs_dt[, basin_id := ID]

cesm_all_wavelets <- merge(cesm_all_wavelets, attrs_dt, by = "basin_id", all.x = TRUE, suffixes = c("", "_attr"))

cat("\n✓ CESM2 full-series wavelet analysis complete\n")
cat("  Total member estimates:", nrow(cesm_all_wavelets), "\n")
cat("  Basins:", length(unique(cesm_all_wavelets$basin)), "\n")
cat("  Members:", length(unique(cesm_all_wavelets$member)), "\n\n")

# ============================================================================
# COMPUTE BAND CLASS MATCHES FOR CESM2 (ENSO + QD ONLY)
# ============================================================================

cat("Computing ENSO + QD band matches (GRACE vs CESM2)...\n")

# Merge GRACE bands into cesm_all_wavelets for matching
grace_bands_dt <- grace_wavelet[, .(basin,
                                     grace_band_1 = band_class_1,
                                     grace_band_2 = band_class_2,
                                     grace_band_3 = band_class_3,
                                     grace_period_1 = dominant_period_1,
                                     grace_period_2 = dominant_period_2,
                                     grace_period_3 = dominant_period_3,
                                     grace_power_1 = dominant_power_1,
                                     grace_power_2 = dominant_power_2,
                                     grace_power_3 = dominant_power_3)]
cesm_all_wavelets <- merge(cesm_all_wavelets, grace_bands_dt, by = "basin", all.x = TRUE)

# Extract ENSO and QD bands from GRACE for matching
cesm_all_wavelets[, grace_enso_qd := {
  bands <- c(grace_band_1, grace_band_2, grace_band_3)
  paste(bands[bands %in% c("ENSO_core", "Quasi-decadal")], collapse = ",")
}, by = 1:nrow(cesm_all_wavelets)]

# Extract ENSO and QD bands from model for matching
cesm_all_wavelets[, model_enso_qd := {
  bands <- c(band_class_1, band_class_2, band_class_3)
  paste(bands[bands %in% c("ENSO_core", "Quasi-decadal")], collapse = ",")
}, by = 1:nrow(cesm_all_wavelets)]

# Count ENSO+QD matches only
cesm_all_wavelets[, n_enso_qd_matches := {
  grace_eq <- strsplit(grace_enso_qd, ",")[[1]]
  model_eq <- strsplit(model_enso_qd, ",")[[1]]
  sum(model_eq %in% grace_eq)
}, by = 1:nrow(cesm_all_wavelets)]

# Extract ENSO_core power from GRACE and model
cesm_all_wavelets[, grace_enso_power := {
  idx <- which(c(grace_band_1, grace_band_2, grace_band_3) == "ENSO_core")[1]
  if (length(idx) > 0 && !is.na(idx)) {
    c(grace_power_1, grace_power_2, grace_power_3)[idx]
  } else NA_real_
}, by = 1:nrow(cesm_all_wavelets)]

cesm_all_wavelets[, model_enso_power := {
  idx <- which(c(band_class_1, band_class_2, band_class_3) == "ENSO_core")[1]
  if (length(idx) > 0 && !is.na(idx)) {
    c(dominant_power_1, dominant_power_2, dominant_power_3)[idx]
  } else NA_real_
}, by = 1:nrow(cesm_all_wavelets)]

# Extract Quasi-decadal power from GRACE and model
cesm_all_wavelets[, grace_qd_power := {
  idx <- which(c(grace_band_1, grace_band_2, grace_band_3) == "Quasi-decadal")[1]
  if (length(idx) > 0 && !is.na(idx)) {
    c(grace_power_1, grace_power_2, grace_power_3)[idx]
  } else NA_real_
}, by = 1:nrow(cesm_all_wavelets)]

cesm_all_wavelets[, model_qd_power := {
  idx <- which(c(band_class_1, band_class_2, band_class_3) == "Quasi-decadal")[1]
  if (length(idx) > 0 && !is.na(idx)) {
    c(dominant_power_1, dominant_power_2, dominant_power_3)[idx]
  } else NA_real_
}, by = 1:nrow(cesm_all_wavelets)]

cat("  ✓ ENSO + QD band matches computed:", sum(!is.na(cesm_all_wavelets$n_enso_qd_matches)), "members\n")
cat("  ✓ ENSO_core and Quasi-decadal power extracted for comparison\n")

# Compute basin-level power distribution summaries
cat("  Computing GRACE power position within model distributions...\n")

# For each basin, compute full distribution statistics for ENSO and QD power
power_distributions <- cesm_all_wavelets[,
  .(# ENSO_core power distribution
    grace_enso_power = grace_enso_power[1],
    enso_power_mean = mean(model_enso_power, na.rm = TRUE),
    enso_power_sd = sd(model_enso_power, na.rm = TRUE),
    enso_power_p05 = quantile(model_enso_power, 0.05, na.rm = TRUE),
    enso_power_p25 = quantile(model_enso_power, 0.25, na.rm = TRUE),
    enso_power_p50 = quantile(model_enso_power, 0.50, na.rm = TRUE),
    enso_power_p75 = quantile(model_enso_power, 0.75, na.rm = TRUE),
    enso_power_p95 = quantile(model_enso_power, 0.95, na.rm = TRUE),
    enso_power_percentile = mean(model_enso_power < grace_enso_power[1], na.rm = TRUE),
    n_members_enso = sum(!is.na(model_enso_power)),

    # Quasi-decadal power distribution
    grace_qd_power = grace_qd_power[1],
    qd_power_mean = mean(model_qd_power, na.rm = TRUE),
    qd_power_sd = sd(model_qd_power, na.rm = TRUE),
    qd_power_p05 = quantile(model_qd_power, 0.05, na.rm = TRUE),
    qd_power_p25 = quantile(model_qd_power, 0.25, na.rm = TRUE),
    qd_power_p50 = quantile(model_qd_power, 0.50, na.rm = TRUE),
    qd_power_p75 = quantile(model_qd_power, 0.75, na.rm = TRUE),
    qd_power_p95 = quantile(model_qd_power, 0.95, na.rm = TRUE),
    qd_power_percentile = mean(model_qd_power < grace_qd_power[1], na.rm = TRUE),
    n_members_qd = sum(!is.na(model_qd_power))),
  by = basin]

# Merge back into main data
cesm_all_wavelets <- merge(cesm_all_wavelets, power_distributions, by = "basin", all.x = TRUE, suffixes = c("", "_dist"))

cat("  ✓ Power distributions computed for", nrow(power_distributions), "basins\n\n")

# ============================================================================
# IPSL FULL-SERIES WAVELET ANALYSIS (ALL 121 YEARS)
# ============================================================================

cat("============================================================================\n")
cat("IPSL FULL-SERIES WAVELET ANALYSIS\n")
cat("============================================================================\n\n")

cat("Analyzing FULL 121-year time series for each member...\n")
cat("  18 members × 184 basins\n\n")

n_members_ipsl <- dim(G_IPSL_combined)[2]
n_times_ipsl <- dim(G_IPSL_combined)[3]

# Parallel processing across basins
cat("Processing", n_basins, "basins in parallel...\n")

results_ipsl <- foreach(basin = 1:n_basins, .packages = c("data.table", "WaveletComp")) %dopar% {

  tryCatch({
    # Skip if GRACE analysis failed
    if (!grace_wavelet$success[basin]) {
      return(NULL)
    }

    # Extract basin data [members × time] - FULL SERIES
    basin_data <- G_IPSL_combined[basin, , ]

    # Storage for all members' full-series wavelets and spectra
    all_wavelets_list <- list()
    all_spectra_list <- list()

    for (member in 1:n_members_ipsl) {
      member_series <- basin_data[member, ]

      # Compute wavelet metrics on FULL time series (dt = 1/12 for monthly data → periods in YEARS)
      wm <- compute_wavelet_metrics(member_series, dt = 1/12, omega0 = 6)

      if (wm$success) {
        all_wavelets_list[[length(all_wavelets_list) + 1]] <- data.table(
          basin = basin,
          basin_id = attrs$ID[basin],
          basin_name = attrs$name[basin],
          member = member,
          dominant_period_1 = wm$dominant_periods[1],
          dominant_period_2 = wm$dominant_periods[2],
          dominant_period_3 = wm$dominant_periods[3],
          dominant_power_1 = wm$dominant_powers[1],
          dominant_power_2 = wm$dominant_powers[2],
          dominant_power_3 = wm$dominant_powers[3],
          band_class_1 = wm$band_class[1],
          band_class_2 = wm$band_class[2],
          band_class_3 = wm$band_class[3],
          success = TRUE
        )

        # Save global spectrum
        if (!is.null(wm$global_spectrum)) {
          spectrum_dt <- as.data.table(wm$global_spectrum)
          spectrum_dt[, `:=`(basin = basin,
                            basin_id = attrs$ID[basin],
                            basin_name = attrs$name[basin],
                            member = member)]
          all_spectra_list[[length(all_spectra_list) + 1]] <- spectrum_dt
        }
      }
    }

    list(
      all_wavelets = if (length(all_wavelets_list) > 0) rbindlist(all_wavelets_list) else NULL,
      all_spectra = if (length(all_spectra_list) > 0) rbindlist(all_spectra_list) else NULL
    )

  }, error = function(e) {
    NULL
  })
}

# Extract results
ipsl_all_wavelets_list <- lapply(results_ipsl, function(x) if (!is.null(x)) x$all_wavelets else NULL)
ipsl_all_spectra_list <- lapply(results_ipsl, function(x) if (!is.null(x)) x$all_spectra else NULL)

# Combine wavelets
ipsl_all_wavelets_list <- ipsl_all_wavelets_list[!sapply(ipsl_all_wavelets_list, is.null)]

if (length(ipsl_all_wavelets_list) == 0) {
  stop("No IPSL wavelets were generated. Check GRACE wavelet analysis.")
}

ipsl_all_wavelets <- rbindlist(ipsl_all_wavelets_list, fill = TRUE)

# Combine spectra
ipsl_all_spectra_list <- ipsl_all_spectra_list[!sapply(ipsl_all_spectra_list, is.null)]
ipsl_all_spectra <- if (length(ipsl_all_spectra_list) > 0) rbindlist(ipsl_all_spectra_list, fill = TRUE) else NULL

# Attach full basin attributes
ipsl_all_wavelets <- merge(ipsl_all_wavelets, attrs_dt, by = "basin_id", all.x = TRUE, suffixes = c("", "_attr"))

cat("\n✓ IPSL full-series wavelet analysis complete\n")
cat("  Total member estimates:", nrow(ipsl_all_wavelets), "\n")
cat("  Basins:", length(unique(ipsl_all_wavelets$basin)), "\n")
cat("  Members:", length(unique(ipsl_all_wavelets$member)), "\n\n")

# ============================================================================
# COMPUTE BAND CLASS MATCHES FOR IPSL (ENSO + QD ONLY)
# ============================================================================

cat("Computing ENSO + QD band matches (GRACE vs IPSL)...\n")

# Merge GRACE bands into ipsl_all_wavelets for matching
ipsl_all_wavelets <- merge(ipsl_all_wavelets, grace_bands_dt, by = "basin", all.x = TRUE)

# Extract ENSO and QD bands from GRACE for matching
ipsl_all_wavelets[, grace_enso_qd := {
  bands <- c(grace_band_1, grace_band_2, grace_band_3)
  paste(bands[bands %in% c("ENSO_core", "Quasi-decadal")], collapse = ",")
}, by = 1:nrow(ipsl_all_wavelets)]

# Extract ENSO and QD bands from model for matching
ipsl_all_wavelets[, model_enso_qd := {
  bands <- c(band_class_1, band_class_2, band_class_3)
  paste(bands[bands %in% c("ENSO_core", "Quasi-decadal")], collapse = ",")
}, by = 1:nrow(ipsl_all_wavelets)]

# Count ENSO+QD matches only
ipsl_all_wavelets[, n_enso_qd_matches := {
  grace_eq <- strsplit(grace_enso_qd, ",")[[1]]
  model_eq <- strsplit(model_enso_qd, ",")[[1]]
  sum(model_eq %in% grace_eq)
}, by = 1:nrow(ipsl_all_wavelets)]

# Extract ENSO_core power from GRACE and model
ipsl_all_wavelets[, grace_enso_power := {
  idx <- which(c(grace_band_1, grace_band_2, grace_band_3) == "ENSO_core")[1]
  if (length(idx) > 0 && !is.na(idx)) {
    c(grace_power_1, grace_power_2, grace_power_3)[idx]
  } else NA_real_
}, by = 1:nrow(ipsl_all_wavelets)]

ipsl_all_wavelets[, model_enso_power := {
  idx <- which(c(band_class_1, band_class_2, band_class_3) == "ENSO_core")[1]
  if (length(idx) > 0 && !is.na(idx)) {
    c(dominant_power_1, dominant_power_2, dominant_power_3)[idx]
  } else NA_real_
}, by = 1:nrow(ipsl_all_wavelets)]

# Extract Quasi-decadal power from GRACE and model
ipsl_all_wavelets[, grace_qd_power := {
  idx <- which(c(grace_band_1, grace_band_2, grace_band_3) == "Quasi-decadal")[1]
  if (length(idx) > 0 && !is.na(idx)) {
    c(grace_power_1, grace_power_2, grace_power_3)[idx]
  } else NA_real_
}, by = 1:nrow(ipsl_all_wavelets)]

ipsl_all_wavelets[, model_qd_power := {
  idx <- which(c(band_class_1, band_class_2, band_class_3) == "Quasi-decadal")[1]
  if (length(idx) > 0 && !is.na(idx)) {
    c(dominant_power_1, dominant_power_2, dominant_power_3)[idx]
  } else NA_real_
}, by = 1:nrow(ipsl_all_wavelets)]

cat("  ✓ ENSO + QD band matches computed:", sum(!is.na(ipsl_all_wavelets$n_enso_qd_matches)), "members\n")
cat("  ✓ ENSO_core and Quasi-decadal power extracted for comparison\n")

# Compute basin-level power distribution summaries
cat("  Computing GRACE power position within model distributions...\n")

# For each basin, compute full distribution statistics for ENSO and QD power
power_distributions_ipsl <- ipsl_all_wavelets[,
  .(# ENSO_core power distribution
    grace_enso_power = grace_enso_power[1],
    enso_power_mean = mean(model_enso_power, na.rm = TRUE),
    enso_power_sd = sd(model_enso_power, na.rm = TRUE),
    enso_power_p05 = quantile(model_enso_power, 0.05, na.rm = TRUE),
    enso_power_p25 = quantile(model_enso_power, 0.25, na.rm = TRUE),
    enso_power_p50 = quantile(model_enso_power, 0.50, na.rm = TRUE),
    enso_power_p75 = quantile(model_enso_power, 0.75, na.rm = TRUE),
    enso_power_p95 = quantile(model_enso_power, 0.95, na.rm = TRUE),
    enso_power_percentile = mean(model_enso_power < grace_enso_power[1], na.rm = TRUE),
    n_members_enso = sum(!is.na(model_enso_power)),

    # Quasi-decadal power distribution
    grace_qd_power = grace_qd_power[1],
    qd_power_mean = mean(model_qd_power, na.rm = TRUE),
    qd_power_sd = sd(model_qd_power, na.rm = TRUE),
    qd_power_p05 = quantile(model_qd_power, 0.05, na.rm = TRUE),
    qd_power_p25 = quantile(model_qd_power, 0.25, na.rm = TRUE),
    qd_power_p50 = quantile(model_qd_power, 0.50, na.rm = TRUE),
    qd_power_p75 = quantile(model_qd_power, 0.75, na.rm = TRUE),
    qd_power_p95 = quantile(model_qd_power, 0.95, na.rm = TRUE),
    qd_power_percentile = mean(model_qd_power < grace_qd_power[1], na.rm = TRUE),
    n_members_qd = sum(!is.na(model_qd_power))),
  by = basin]

# Merge back into main data
ipsl_all_wavelets <- merge(ipsl_all_wavelets, power_distributions_ipsl, by = "basin", all.x = TRUE, suffixes = c("", "_dist"))

cat("  ✓ Power distributions computed for", nrow(power_distributions_ipsl), "basins\n\n")

# ============================================================================
# CREATE BASIN-LEVEL WAVELET SUMMARY (similar to Phase 03 dispersion_summary)
# ============================================================================

cat("Creating basin-level wavelet power summary...\n")

# Merge CESM and IPSL power distributions by basin
wavelet_summary <- merge(
  power_distributions[, .(basin, basin_id = attrs$ID[basin], basin_name = attrs$name[basin],
                          grace_enso_power, grace_qd_power,
                          enso_power_p05_cesm = enso_power_p05,
                          enso_power_p50_cesm = enso_power_p50,
                          enso_power_p95_cesm = enso_power_p95,
                          enso_power_mean_cesm = enso_power_mean,
                          enso_power_sd_cesm = enso_power_sd,
                          enso_power_percentile_cesm = enso_power_percentile,
                          qd_power_p05_cesm = qd_power_p05,
                          qd_power_p50_cesm = qd_power_p50,
                          qd_power_p95_cesm = qd_power_p95,
                          qd_power_mean_cesm = qd_power_mean,
                          qd_power_sd_cesm = qd_power_sd,
                          qd_power_percentile_cesm = qd_power_percentile,
                          n_members_enso_cesm = n_members_enso,
                          n_members_qd_cesm = n_members_qd)],
  power_distributions_ipsl[, .(basin,
                               enso_power_p05_ipsl = enso_power_p05,
                               enso_power_p50_ipsl = enso_power_p50,
                               enso_power_p95_ipsl = enso_power_p95,
                               enso_power_mean_ipsl = enso_power_mean,
                               enso_power_sd_ipsl = enso_power_sd,
                               enso_power_percentile_ipsl = enso_power_percentile,
                               qd_power_p05_ipsl = qd_power_p05,
                               qd_power_p50_ipsl = qd_power_p50,
                               qd_power_p95_ipsl = qd_power_p95,
                               qd_power_mean_ipsl = qd_power_mean,
                               qd_power_sd_ipsl = qd_power_sd,
                               qd_power_percentile_ipsl = qd_power_percentile,
                               n_members_enso_ipsl = n_members_enso,
                               n_members_qd_ipsl = n_members_qd)],
  by = "basin",
  all = TRUE
)

# Merge GRACE band classifications
wavelet_summary <- merge(
  wavelet_summary,
  grace_wavelet[, .(basin, basin_id, basin_name,
                   grace_period_1 = dominant_period_1,
                   grace_period_2 = dominant_period_2,
                   grace_period_3 = dominant_period_3,
                   grace_band_1 = band_class_1,
                   grace_band_2 = band_class_2,
                   grace_band_3 = band_class_3)],
  by = c("basin", "basin_id", "basin_name"),
  all.x = TRUE
)

# Merge basin attributes
wavelet_summary <- merge(wavelet_summary, attrs_dt, by = "basin_id", all.x = TRUE, suffixes = c("", "_attr"))

cat("  ✓ Basin-level summary created:", nrow(wavelet_summary), "basins\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

# Basin-level summary (like Phase 03 dispersion_summary)
saveRDS(wavelet_summary, "outputs/phase04_wavelet_summary.rds")
fwrite(wavelet_summary, "outputs/phase04_wavelet_summary.csv")

# Member-level full data
saveRDS(cesm_all_wavelets, "outputs/phase04_cesm_all_wavelets.rds")
fwrite(cesm_all_wavelets, "outputs/phase04_cesm_all_wavelets.csv")

saveRDS(ipsl_all_wavelets, "outputs/phase04_ipsl_all_wavelets.rds")
fwrite(ipsl_all_wavelets, "outputs/phase04_ipsl_all_wavelets.csv")

# GRACE wavelet metrics
saveRDS(grace_wavelet, "outputs/phase04_grace_wavelets.rds")
fwrite(grace_wavelet, "outputs/phase04_grace_wavelets.csv")

# Global power spectra (all periods × all members × all basins)
saveRDS(grace_spectra, "outputs/phase04_grace_spectra.rds")
fwrite(grace_spectra, "outputs/phase04_grace_spectra.csv")

if (!is.null(cesm_all_spectra)) {
  saveRDS(cesm_all_spectra, "outputs/phase04_cesm_spectra.rds")
  fwrite(cesm_all_spectra, "outputs/phase04_cesm_spectra.csv")
}

if (!is.null(ipsl_all_spectra)) {
  saveRDS(ipsl_all_spectra, "outputs/phase04_ipsl_spectra.rds")
  fwrite(ipsl_all_spectra, "outputs/phase04_ipsl_spectra.csv")
}

cat("  ✓ outputs/phase04_wavelet_summary.{rds,csv} (basin-level)\n")
cat("  ✓ outputs/phase04_grace_wavelets.{rds,csv}\n")
cat("  ✓ outputs/phase04_grace_spectra.{rds,csv} (full power spectra)\n")
cat("  ✓ outputs/phase04_cesm_all_wavelets.{rds,csv} (member-level)\n")
cat("  ✓ outputs/phase04_cesm_spectra.{rds,csv} (full power spectra)\n")
cat("  ✓ outputs/phase04_ipsl_all_wavelets.{rds,csv} (member-level)\n")
cat("  ✓ outputs/phase04_ipsl_spectra.{rds,csv} (full power spectra)\n\n")

# ============================================================================
# SUMMARY STATISTICS - ENSO + QD BAND MATCHING
# ============================================================================

cat("============================================================================\n")
cat("ENSO + QD BAND MATCHING SUMMARY\n")
cat("============================================================================\n\n")

cat("CESM2 ENSO + QD MATCHES (member-level):\n")
cesm_match_summary <- cesm_all_wavelets[, .N, by = n_enso_qd_matches]
setorder(cesm_match_summary, n_enso_qd_matches)
print(cesm_match_summary)
cat("  Mean ENSO+QD matches per member:", round(mean(cesm_all_wavelets$n_enso_qd_matches, na.rm = TRUE), 2), "\n")
cat("  Members with ≥1 ENSO/QD match:", sum(cesm_all_wavelets$n_enso_qd_matches >= 1, na.rm = TRUE),
    paste0("(", round(mean(cesm_all_wavelets$n_enso_qd_matches >= 1, na.rm = TRUE) * 100, 1), "%)"), "\n")
cat("  Members with 2 ENSO/QD matches:", sum(cesm_all_wavelets$n_enso_qd_matches == 2, na.rm = TRUE),
    paste0("(", round(mean(cesm_all_wavelets$n_enso_qd_matches == 2, na.rm = TRUE) * 100, 1), "%)"), "\n\n")

cat("IPSL ENSO + QD MATCHES (member-level):\n")
ipsl_match_summary <- ipsl_all_wavelets[, .N, by = n_enso_qd_matches]
setorder(ipsl_match_summary, n_enso_qd_matches)
print(ipsl_match_summary)
cat("  Mean ENSO+QD matches per member:", round(mean(ipsl_all_wavelets$n_enso_qd_matches, na.rm = TRUE), 2), "\n")
cat("  Members with ≥1 ENSO/QD match:", sum(ipsl_all_wavelets$n_enso_qd_matches >= 1, na.rm = TRUE),
    paste0("(", round(mean(ipsl_all_wavelets$n_enso_qd_matches >= 1, na.rm = TRUE) * 100, 1), "%)"), "\n")
cat("  Members with 2 ENSO/QD matches:", sum(ipsl_all_wavelets$n_enso_qd_matches == 2, na.rm = TRUE),
    paste0("(", round(mean(ipsl_all_wavelets$n_enso_qd_matches == 2, na.rm = TRUE) * 100, 1), "%)"), "\n\n")


# Cleanup parallel cluster
stopCluster(cl)
cat("✓ Parallel cluster stopped\n\n")

cat("============================================================================\n")
cat("PHASE 04 COMPLETE\n")
cat("============================================================================\n\n")
