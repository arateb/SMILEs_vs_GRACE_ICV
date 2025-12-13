#!/usr/bin/env Rscript
# ============================================================================
# MASTER SCRIPT - RUN ALL PHASES WITH FIGURES - 2025-11-27
# ============================================================================
# Complete analysis pipeline from Phase 03 through Phase 08
# Generates all figures including maps and scatters
#
# OBJECTIVE: Assess whether SMILEs members are compatible with 23-year GRACE
# observations across multiple diagnostic metrics
#
# Author: Ashraf Rateb
# Date: 2025-11-27
# ============================================================================

# ============================================================================
# SETUP
# ============================================================================

library(data.table)
library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(parallel)
library(foreach)
library(doParallel)

# Create output directory for today's run
RUN_DATE <- "20251127"
OUTPUT_DIR <- paste0("outputs/run_", RUN_DATE)
FIG_DIR <- paste0(OUTPUT_DIR, "/figures")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

cat("================================================================================\n")
cat("MASTER ANALYSIS PIPELINE - RUN DATE:", RUN_DATE, "\n")
cat("================================================================================\n\n")
cat("Output directory:", OUTPUT_DIR, "\n")
cat("Figures directory:", FIG_DIR, "\n\n")

# ============================================================================
# LOAD BASE DATA
# ============================================================================

cat("Loading base data...\n")

# Load FILTERED data from Phase 1
filtered_data <- readRDS("data/Processed_Filtered_Nov2025.rds")
raw_data <- readRDS("data/Enhanced_GGFO_MMLEs_Nov2025.rds")

# Extract components
attrs <- raw_data$attrs
gfo_dtrend <- raw_data$gfo_dtrend
dates_grace <- raw_data$dates_grace

G_CESM_filtered <- filtered_data$G_CESM_filtered
date_cesm_subset <- filtered_data$date_cesm
G_IPSL_combined <- filtered_data$G_IPSL_combined
date_ipsl_subset <- filtered_data$date_ipsl

GRACE_median <- gfo_dtrend$median

cat("  CESM2 dimensions:", dim(G_CESM_filtered), "\n")
cat("  IPSL dimensions:", dim(G_IPSL_combined), "\n")
cat("  GRACE period:", as.character(range(dates_grace)), "\n")
cat("  Basins:", nrow(attrs), "\n\n")

# Load basin shapefile for maps
cat("Loading basin shapefile...\n")
basins_shp <- st_read("/Volumes/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)
cat("  Basins in shapefile:", nrow(basins_shp), "\n\n")

# Load world map for background
world <- ne_countries(scale = "medium", returnclass = "sf")

# ============================================================================
# GLOBAL THEME FOR FIGURES
# ============================================================================

theme_pub <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey50"),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# ============================================================================
# HELPER: SAVE FIGURE FUNCTION
# ============================================================================

save_fig <- function(plot, name, width = 10, height = 8) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300)
  ggsave(file.path(FIG_DIR, paste0(name, ".pdf")), plot,
         width = width, height = height)
  cat("  Saved:", name, "\n")
}

# ============================================================================
# PHASE 03: DISPERSION ANALYSIS
# ============================================================================

cat("================================================================================\n")
cat("PHASE 03: DISPERSION ANALYSIS\n")
cat("================================================================================\n\n")

# Source windowing functions
source("src/functions/windowing_functions_nov2025.R")

# Setup parallel
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)
cat("Using", n_cores, "cores\n\n")

# Compute GRACE metrics
cat("Computing GRACE metrics...\n")
grace_metrics <- compute_grace_metrics(
  x_grace = GRACE_median,
  dates_grace = dates_grace,
  attrs = attrs
)

Lg <- length(dates_grace)
cat("Window length:", Lg, "months (", round(Lg/12, 1), "years)\n\n")

# CESM2 dispersion
cat("CESM2 windowing analysis...\n")
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

# IPSL dispersion
cat("IPSL windowing analysis...\n")
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

# Classify basins
basin_classification <- classify_basin_coverage(
  summary_cesm = cesm_summary,
  summary_ipsl = ipsl_summary
)

# Combine summaries
cesm_cols_to_keep <- c("basin", "basin_id", "basin_name", "A_grace", "sigma_grace", "n_obs",
                       "A_p05", "A_p50", "A_p95",
                       "sigma_p05", "sigma_p50", "sigma_p95",
                       "T_A", "n_windows",
                       "amplitude_covered", "variance_covered")
cesm_cols_to_keep <- intersect(cesm_cols_to_keep, names(cesm_summary))

cesm_select <- copy(cesm_summary[, ..cesm_cols_to_keep])
setnames(cesm_select,
         old = c("A_p05", "A_p50", "A_p95", "sigma_p05", "sigma_p50", "sigma_p95",
                 "T_A", "n_windows", "amplitude_covered", "variance_covered"),
         new = c("A_p05_cesm", "A_p50_cesm", "A_p95_cesm",
                 "sigma_p05_cesm", "sigma_p50_cesm", "sigma_p95_cesm",
                 "T_A_cesm", "n_windows_cesm",
                 "amplitude_covered_cesm", "variance_covered_cesm"))

ipsl_select <- copy(ipsl_summary[, c("basin", "A_p05", "A_p50", "A_p95",
                                      "sigma_p05", "sigma_p50", "sigma_p95",
                                      "T_A", "n_windows",
                                      "amplitude_covered", "variance_covered")])
setnames(ipsl_select,
         old = c("A_p05", "A_p50", "A_p95", "sigma_p05", "sigma_p50", "sigma_p95",
                 "T_A", "n_windows", "amplitude_covered", "variance_covered"),
         new = c("A_p05_ipsl", "A_p50_ipsl", "A_p95_ipsl",
                 "sigma_p05_ipsl", "sigma_p50_ipsl", "sigma_p95_ipsl",
                 "T_A_ipsl", "n_windows_ipsl",
                 "amplitude_covered_ipsl", "variance_covered_ipsl"))

dispersion_summary <- merge(cesm_select, ipsl_select, by = "basin")
dispersion_summary <- merge(dispersion_summary,
                            basin_classification[, .(basin, basin_class_amplitude,
                                                    basin_class_variance, basin_class_strict)],
                            by = "basin")

# Save Phase 03 results
saveRDS(dispersion_summary, file.path(OUTPUT_DIR, "dispersion_summary.rds"))
fwrite(dispersion_summary, file.path(OUTPUT_DIR, "dispersion_summary.csv"))

cat("\nPhase 03 Summary:\n")
cat("  CESM2 - Amplitude covered:", sum(dispersion_summary$amplitude_covered_cesm, na.rm = TRUE), "/", nrow(dispersion_summary), "\n")
cat("  CESM2 - Variance covered:", sum(dispersion_summary$variance_covered_cesm, na.rm = TRUE), "/", nrow(dispersion_summary), "\n")
cat("  IPSL - Amplitude covered:", sum(dispersion_summary$amplitude_covered_ipsl, na.rm = TRUE), "/", nrow(dispersion_summary), "\n")
cat("  IPSL - Variance covered:", sum(dispersion_summary$variance_covered_ipsl, na.rm = TRUE), "/", nrow(dispersion_summary), "\n\n")

# ----------------------------------------------------------------------------
# PHASE 03 FIGURES
# ----------------------------------------------------------------------------

cat("Generating Phase 03 figures...\n")

# Merge with shapefile for mapping
basins_shp$basin_id <- as.numeric(basins_shp$Num)
dispersion_sf <- merge(basins_shp, dispersion_summary, by.x = "Num", by.y = "basin")

# Figure 3a: Amplitude GRACE vs Model map
p3a_amp_cesm <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = dispersion_sf, aes(fill = A_grace / A_p50_cesm), color = "grey30", linewidth = 0.1) +
  scale_fill_gradient2(
    name = "GRACE/CESM2\nAmplitude Ratio",
    low = "blue", mid = "white", high = "red",
    midpoint = 1, limits = c(0.5, 2), oob = scales::squish
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "GRACE vs CESM2 Amplitude Ratio",
       subtitle = "Values > 1 indicate GRACE exceeds model ensemble median") +
  theme_pub

p3a_amp_ipsl <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = dispersion_sf, aes(fill = A_grace / A_p50_ipsl), color = "grey30", linewidth = 0.1) +
  scale_fill_gradient2(
    name = "GRACE/IPSL\nAmplitude Ratio",
    low = "blue", mid = "white", high = "red",
    midpoint = 1, limits = c(0.5, 2), oob = scales::squish
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "GRACE vs IPSL Amplitude Ratio") +
  theme_pub

p3a <- p3a_amp_cesm / p3a_amp_ipsl
save_fig(p3a, "fig03a_amplitude_ratio_maps", width = 10, height = 12)

# Figure 3b: Variance ratio maps
p3b_var_cesm <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = dispersion_sf, aes(fill = sigma_grace / sigma_p50_cesm), color = "grey30", linewidth = 0.1) +
  scale_fill_gradient2(
    name = "GRACE/CESM2\nStd Dev Ratio",
    low = "blue", mid = "white", high = "red",
    midpoint = 1, limits = c(0.5, 2), oob = scales::squish
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "GRACE vs CESM2 Standard Deviation Ratio") +
  theme_pub

p3b_var_ipsl <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = dispersion_sf, aes(fill = sigma_grace / sigma_p50_ipsl), color = "grey30", linewidth = 0.1) +
  scale_fill_gradient2(
    name = "GRACE/IPSL\nStd Dev Ratio",
    low = "blue", mid = "white", high = "red",
    midpoint = 1, limits = c(0.5, 2), oob = scales::squish
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "GRACE vs IPSL Standard Deviation Ratio") +
  theme_pub

p3b <- p3b_var_cesm / p3b_var_ipsl
save_fig(p3b, "fig03b_variance_ratio_maps", width = 10, height = 12)

# Figure 3c: Scatter - GRACE vs Model Amplitude
p3c <- ggplot(dispersion_summary) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(x = A_p50_cesm, y = A_grace, color = "CESM2"), alpha = 0.6, size = 2) +
  geom_point(aes(x = A_p50_ipsl, y = A_grace, color = "IPSL"), alpha = 0.6, size = 2) +
  geom_errorbarh(aes(xmin = A_p05_cesm, xmax = A_p95_cesm, y = A_grace, color = "CESM2"),
                 alpha = 0.3, height = 0) +
  geom_errorbarh(aes(xmin = A_p05_ipsl, xmax = A_p95_ipsl, y = A_grace, color = "IPSL"),
                 alpha = 0.3, height = 0) +
  scale_color_manual(values = c("CESM2" = "#2166AC", "IPSL" = "#B2182B")) +
  labs(x = "Model Amplitude [mm] (median with 90% CI)",
       y = "GRACE Amplitude [mm]",
       color = "Model",
       title = "GRACE vs Model TWS Amplitude",
       subtitle = "Error bars show 5th-95th percentile of model windows") +
  theme_pub +
  coord_fixed()

save_fig(p3c, "fig03c_amplitude_scatter", width = 8, height = 8)

# Figure 3d: Scatter - GRACE vs Model Variance
p3d <- ggplot(dispersion_summary) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(x = sigma_p50_cesm, y = sigma_grace, color = "CESM2"), alpha = 0.6, size = 2) +
  geom_point(aes(x = sigma_p50_ipsl, y = sigma_grace, color = "IPSL"), alpha = 0.6, size = 2) +
  geom_errorbarh(aes(xmin = sigma_p05_cesm, xmax = sigma_p95_cesm, y = sigma_grace, color = "CESM2"),
                 alpha = 0.3, height = 0) +
  geom_errorbarh(aes(xmin = sigma_p05_ipsl, xmax = sigma_p95_ipsl, y = sigma_grace, color = "IPSL"),
                 alpha = 0.3, height = 0) +
  scale_color_manual(values = c("CESM2" = "#2166AC", "IPSL" = "#B2182B")) +
  labs(x = "Model Std Dev [mm] (median with 90% CI)",
       y = "GRACE Std Dev [mm]",
       color = "Model",
       title = "GRACE vs Model TWS Standard Deviation") +
  theme_pub +
  coord_fixed()

save_fig(p3d, "fig03d_variance_scatter", width = 8, height = 8)

# Figure 3e: Return period maps
p3e_cesm <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = dispersion_sf, aes(fill = pmin(T_A_cesm, 1000)), color = "grey30", linewidth = 0.1) +
  scale_fill_viridis_c(
    name = "Return Period\n(years)",
    trans = "log10",
    breaks = c(10, 100, 1000),
    labels = c("10", "100", "1000+"),
    na.value = "grey50"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "CESM2: Implied Return Period of GRACE Amplitude",
       subtitle = "Years until ensemble produces GRACE-like amplitude") +
  theme_pub

p3e_ipsl <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = dispersion_sf, aes(fill = pmin(T_A_ipsl, 1000)), color = "grey30", linewidth = 0.1) +
  scale_fill_viridis_c(
    name = "Return Period\n(years)",
    trans = "log10",
    breaks = c(10, 100, 1000),
    labels = c("10", "100", "1000+"),
    na.value = "grey50"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "IPSL: Implied Return Period") +
  theme_pub

p3e <- p3e_cesm / p3e_ipsl
save_fig(p3e, "fig03e_return_period_maps", width = 10, height = 12)

# Figure 3f: Coverage classification map
dispersion_sf$coverage_class <- factor(
  dispersion_sf$basin_class_amplitude,
  levels = c("both_covered", "cesm_only", "ipsl_only", "neither")
)

p3f <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = dispersion_sf, aes(fill = coverage_class), color = "grey30", linewidth = 0.1) +
  scale_fill_manual(
    name = "Coverage",
    values = c("both_covered" = "#1B9E77", "cesm_only" = "#2166AC",
               "ipsl_only" = "#B2182B", "neither" = "#7570B3"),
    labels = c("Both models cover", "CESM2 only", "IPSL only", "Neither model"),
    na.value = "grey80"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "Basin Classification: GRACE Amplitude Coverage",
       subtitle = "Which model ensembles encompass GRACE observations?") +
  theme_pub

save_fig(p3f, "fig03f_coverage_classification_map", width = 10, height = 6)

cat("Phase 03 figures complete\n\n")

stopCluster(cl)

cat("================================================================================\n")
cat("PHASE 03 COMPLETE - Results saved to", OUTPUT_DIR, "\n")
cat("================================================================================\n\n")

# Save basin attributes for later phases
saveRDS(attrs, file.path(OUTPUT_DIR, "basin_attributes.rds"))
saveRDS(basins_shp, file.path(OUTPUT_DIR, "basin_polygons.rds"))

# ============================================================================
# Continue to run Phase 04, 05, 06, 08...
# These are sourced as separate scripts but output to same directory
# ============================================================================

cat("NOTE: Run remaining phases (04, 05, 06, 08) separately or continue in this script\n")
cat("All outputs will go to:", OUTPUT_DIR, "\n\n")

# Store configuration for other phases
config <- list(
  run_date = RUN_DATE,
  output_dir = OUTPUT_DIR,
  fig_dir = FIG_DIR,
  n_basins = nrow(attrs),
  grace_period = range(dates_grace),
  window_length = Lg
)

saveRDS(config, file.path(OUTPUT_DIR, "run_config.rds"))

cat("================================================================================\n")
cat("MASTER SCRIPT PHASE 03 SECTION COMPLETE\n")
cat("================================================================================\n")
