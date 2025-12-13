# ============================================================================
# PHASE 03 - DISPERSION & WINDOWING ANALYSIS - NOVEMBER 2025
# ============================================================================
# Windowing analysis comparing GRACE-length windows from models vs GRACE
# Computes amplitude, variance, envelopes, and implied return periods
# Author: Ashraf Rateb
# Date: 2025-11-18
# ============================================================================
library(data.table)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Load windowing functions
source("src/functions/windowing_functions_nov2025.R")

cat("============================================================================\n")
cat("PHASE 03: DISPERSION & WINDOWING ANALYSIS\n")
cat("============================================================================\n\n")

# ============================================================================
# LOAD FILTERED DATA FROM PHASE 01
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

# Extract FILTERED model data from Phase 1 (already subset, deforced, STL filtered)
G_CESM_filtered <- filtered_data$G_CESM_filtered
date_cesm_subset <- filtered_data$date_cesm
G_IPSL_combined <- filtered_data$G_IPSL_combined
date_ipsl_subset <- filtered_data$date_ipsl

cat("  CESM2 filtered dimensions:", dim(G_CESM_filtered), "(1900-2100)\n")
cat("  IPSL combined filtered dimensions:", dim(G_IPSL_combined), "(1900-2020)\n")
cat("  Basins:", nrow(attrs), "\n")

# GRACE: Already filtered, use median
GRACE_median <- gfo_dtrend$median  # [time × basins]
GRACE_mad <- gfo_dtrend$mad

cat("✓ Using filtered data from Phase 1 (already subset + deforced + STL filtered)\n\n")

# Setup parallel processing
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)
cat("Using", n_cores, "cores for parallel processing\n\n")

# ============================================================================
# COMPUTE GRACE METRICS
# ============================================================================

cat("Computing GRACE metrics...\n")

grace_metrics <- compute_grace_metrics(
  x_grace = GRACE_median,
  dates_grace = dates_grace,
  attrs = attrs
)

cat("  GRACE period:", as.character(range(dates_grace)), "\n")
cat("  Window length (Lg):", length(dates_grace), "months\n")
cat("  Window length (years):", round(length(dates_grace) / 12, 1), "years\n\n")

# ============================================================================
# BASIN ALIGNMENT CHECKS
# ============================================================================

cat("Checking basin alignment...\n")

# Verify dimensions match
stopifnot("CESM basin dimension mismatch" = dim(G_CESM_filtered)[1] == nrow(attrs))
stopifnot("IPSL basin dimension mismatch" = dim(G_IPSL_combined)[1] == nrow(attrs))
stopifnot("GRACE metrics basin count mismatch" = nrow(grace_metrics) == nrow(attrs))

cat("  ✓ All basin dimensions aligned (n =", nrow(attrs), ")\n")
cat("  ✓ CESM basins:", dim(G_CESM_filtered)[1], "\n")
cat("  ✓ IPSL basins:", dim(G_IPSL_combined)[1], "\n")
cat("  ✓ GRACE metrics basins:", nrow(grace_metrics), "\n\n")

# ============================================================================
# CESM2 WINDOWING ANALYSIS
# ============================================================================

cat("CESM2 windowing analysis...\n")
cat("  Date range: 1900-2100\n")
cat("  Members:", dim(G_CESM_filtered)[2], "\n")
cat("  Time points:", dim(G_CESM_filtered)[3], "\n")

Lg <- length(dates_grace)

# Compute summary in parallel (one basin per core)
cat("  Computing dispersion metrics in parallel...\n")
cesm_summary <- compute_dispersion_summary_parallel(
  x = G_CESM_filtered,
  dates = date_cesm_subset,
  window_length = Lg,
  date_start = "1900-01-01",
  date_end = "2100-12-31",
  grace_metrics = grace_metrics,
  attrs = attrs,
  model_name = "CESM2"
)

cat("✓ CESM2 analysis complete\n")
cat("  Basins processed:", nrow(cesm_summary), "\n\n")

# ============================================================================
# IPSL WINDOWING ANALYSIS
# ============================================================================

cat("IPSL windowing analysis...\n")
cat("  Date range: 1900-2020\n")
cat("  Members:", dim(G_IPSL_combined)[2], "\n")
cat("  Time points:", dim(G_IPSL_combined)[3], "\n")

# Compute summary in parallel (one basin per core)
cat("  Computing dispersion metrics in parallel...\n")
ipsl_summary <- compute_dispersion_summary_parallel(
  x = G_IPSL_combined,
  dates = date_ipsl_subset,
  window_length = Lg,
  date_start = "1900-01-01",
  date_end = "2020-12-31",
  grace_metrics = grace_metrics,
  attrs = attrs,
  model_name = "IPSL"
)

cat("✓ IPSL analysis complete\n")
cat("  Basins processed:", nrow(ipsl_summary), "\n\n")

# ============================================================================
# BASIN CLASSIFICATION
# ============================================================================

cat("Classifying basins by envelope coverage...\n")

basin_classification <- classify_basin_coverage(
  summary_cesm = cesm_summary,
  summary_ipsl = ipsl_summary
)

cat("  Amplitude coverage classification:\n")
print(table(basin_classification$basin_class_amplitude))

cat("\n  Variance coverage classification:\n")
print(table(basin_classification$basin_class_variance))

cat("\n  Strict (amplitude AND variance) classification:\n")
print(table(basin_classification$basin_class_strict))

cat("\n✓ Classification complete\n\n")

# ============================================================================
# COMBINE SUMMARIES
# ============================================================================

cat("Combining CESM2 and IPSL summaries...\n")

# Get all attr column names that exist in both summaries
cesm_names <- names(cesm_summary)
ipsl_names <- names(ipsl_summary)

# Find attr columns (present in cesm but not core metrics)
core_cols <- c("basin", "basin_id", "basin_name", "A_grace", "sigma_grace", "n_obs",
               "A_p05", "A_p50", "A_p95", "sigma_p05", "sigma_p50", "sigma_p95",
               "T_A", "n_windows", "n_exceed", "n_total",
               "amplitude_covered", "variance_covered", "model")

attr_cols <- setdiff(cesm_names, core_cols)

# Select columns from cesm_summary (including all attrs)
cesm_cols_to_keep <- c("basin", "basin_id", "basin_name", "A_grace", "sigma_grace", "n_obs",
                       "A_p05", "A_p50", "A_p95",
                       "sigma_p05", "sigma_p50", "sigma_p95",
                       "T_A", "n_windows",
                       "amplitude_covered", "variance_covered",
                       attr_cols)

# Only keep columns that actually exist
cesm_cols_to_keep <- intersect(cesm_cols_to_keep, cesm_names)

# Copy and rename CESM2 columns
cesm_select <- copy(cesm_summary[, ..cesm_cols_to_keep])
setnames(cesm_select,
         old = c("A_p05", "A_p50", "A_p95",
                 "sigma_p05", "sigma_p50", "sigma_p95",
                 "T_A", "n_windows",
                 "amplitude_covered", "variance_covered"),
         new = c("A_p05_cesm", "A_p50_cesm", "A_p95_cesm",
                 "sigma_p05_cesm", "sigma_p50_cesm", "sigma_p95_cesm",
                 "T_A_cesm", "n_windows_cesm",
                 "amplitude_covered_cesm", "variance_covered_cesm"))

# Select columns from ipsl_summary
ipsl_cols_to_keep <- c("basin",
                       "A_p05", "A_p50", "A_p95",
                       "sigma_p05", "sigma_p50", "sigma_p95",
                       "T_A", "n_windows",
                       "amplitude_covered", "variance_covered")

ipsl_select <- copy(ipsl_summary[, ..ipsl_cols_to_keep])
setnames(ipsl_select,
         old = c("A_p05", "A_p50", "A_p95",
                 "sigma_p05", "sigma_p50", "sigma_p95",
                 "T_A", "n_windows",
                 "amplitude_covered", "variance_covered"),
         new = c("A_p05_ipsl", "A_p50_ipsl", "A_p95_ipsl",
                 "sigma_p05_ipsl", "sigma_p50_ipsl", "sigma_p95_ipsl",
                 "T_A_ipsl", "n_windows_ipsl",
                 "amplitude_covered_ipsl", "variance_covered_ipsl"))

# Merge both summaries
dispersion_summary_combined <- merge(cesm_select, ipsl_select, by = "basin")

# Merge with classification
dispersion_summary_final <- merge(
  dispersion_summary_combined,
  basin_classification[, .(basin, basin_id, basin_name,
                          cesm_amp_covered, ipsl_amp_covered,
                          cesm_var_covered, ipsl_var_covered,
                          basin_class_amplitude, basin_class_variance, basin_class_strict)],
  by = c("basin", "basin_id", "basin_name")
)

cat("✓ Summaries combined\n")
cat("  Columns in final summary:", ncol(dispersion_summary_final), "\n")
cat("  Including all basin attributes from attrs\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

# Save RDS files
saveRDS(dispersion_summary_final, "outputs/dispersion_summary.rds")

# Save CSV
fwrite(dispersion_summary_final, "outputs/dispersion_summary.csv")

cat("  ✓ outputs/dispersion_summary.rds\n")
cat("  ✓ outputs/dispersion_summary.csv\n")
cat("\nNote: Individual windows NOT saved (memory-efficient mode)\n")
cat("All statistics computed directly from streaming window analysis\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("============================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================================================\n\n")

cat("Total basins analyzed:", nrow(dispersion_summary_final), "\n\n")

cat("CESM2:\n")
cat("  Basins processed:", nrow(cesm_summary), "\n")
cat("  Mean windows per basin:", round(mean(cesm_summary$n_windows), 1), "\n")
cat("  Basins where GRACE amplitude covered:", sum(cesm_summary$amplitude_covered, na.rm = TRUE), "\n")
cat("  Basins where GRACE variance covered:", sum(cesm_summary$variance_covered, na.rm = TRUE), "\n\n")

cat("IPSL:\n")
cat("  Basins processed:", nrow(ipsl_summary), "\n")
cat("  Mean windows per basin:", round(mean(ipsl_summary$n_windows), 1), "\n")
cat("  Basins where GRACE amplitude covered:", sum(ipsl_summary$amplitude_covered, na.rm = TRUE), "\n")
cat("  Basins where GRACE variance covered:", sum(ipsl_summary$variance_covered, na.rm = TRUE), "\n\n")

cat("Return periods (median across basins):\n")
cat("  CESM2 T_A (amplitude):", round(median(cesm_summary$T_A[is.finite(cesm_summary$T_A)], na.rm = TRUE), 2), "\n")
cat("  IPSL T_A (amplitude):", round(median(ipsl_summary$T_A[is.finite(ipsl_summary$T_A)], na.rm = TRUE), 2), "\n\n")

cat("Basin classifications (amplitude):\n")
print(table(basin_classification$basin_class_amplitude))

cat("\n============================================================================\n")
cat("PHASE 03 COMPLETE\n")
cat("============================================================================\n\n")

# Cleanup parallel cluster
stopCluster(cl)
cat("✓ Parallel cluster stopped\n")
