#!/usr/bin/env Rscript
# ==============================================================================
# FIGURE: PHASE 08 METRIC SPACE ELLIPSES
# ==============================================================================
#
# Pairwise 2D projections of 10-metric compatibility space with:
# - Density contours for model ensemble distributions
# - GRACE observations as points
# - Mahalanobis ellipses showing ensemble spread
# - Color-coded by compatibility class
#
# Creates elliptical contour plots for all key metric combinations
#
# Author: Claude Code
# Date: November 2025
# ==============================================================================

library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(MASS)  # For kde2d
library(ellipse)  # For ellipse computation

# Set output directory
fig_dir <- "outputs/figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading Phase 08 compatibility data...\n")

# Basin-level compatibility
compat <- readRDS("outputs/phase08_compatibility_basin.rds")

# Member-level data for ensemble distributions
cesm_persistence <- readRDS("outputs/phase05_cesm_persistence.rds")
ipsl_persistence <- readRDS("outputs/phase05_ipsl_persistence.rds")

cesm_wavelets <- readRDS("outputs/phase04_cesm_all_wavelets.rds")
ipsl_wavelets <- readRDS("outputs/phase04_ipsl_all_wavelets.rds")

# Phase 06 outputs (when available)
if (file.exists("outputs/phase06_events_models.rds") &&
    file.exists("outputs/phase06_cesm_regime_persistence.rds") &&
    file.exists("outputs/phase06_ipsl_regime_persistence.rds")) {
  events_models <- readRDS("outputs/phase06_events_models.rds")
  cesm_regime <- readRDS("outputs/phase06_cesm_regime_persistence.rds")
  ipsl_regime <- readRDS("outputs/phase06_ipsl_regime_persistence.rds")
  phase06_available <- TRUE
  cat("  ✓ Phase 06 data loaded (with regime persistence)\n")
} else {
  phase06_available <- FALSE
  cat("  ! Phase 06 data not available - using Phase 04 & 05 metrics only\n")
  cat("  ! Waiting for Phase 06 to complete (regime persistence)\n")
}

cat("  Loaded compatibility for", nrow(compat), "basins\n\n")

# ==============================================================================
# CONSTRUCT MEMBER-LEVEL METRIC DATA
# ==============================================================================

cat("Constructing member-level metric datasets...\n")

# CESM2: Aggregate wavelets per member
cesm_wavelets_member <- cesm_wavelets[, .(
  power_1 = median(dominant_power_1, na.rm = TRUE),
  enso_power = median(model_enso_power, na.rm = TRUE),
  qd_power = median(model_qd_power, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

# CESM2: Aggregate persistence per member
cesm_persistence_member <- cesm_persistence[, .(
  tau = median(tau, na.rm = TRUE),
  p_lf = median(p_lf, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

# Merge CESM2
cesm_metrics <- merge(
  cesm_wavelets_member,
  cesm_persistence_member,
  by = c("basin", "basin_id", "basin_name", "member"),
  all = FALSE
)

# Add Phase 06 metrics if available
if (phase06_available) {
  # Events
  cesm_events_summary <- events_models[model == "CESM2", .(
    H_max = ifelse(any(type == "pluvial"), max(pluvial_height, na.rm = TRUE), NA_real_),
    D_max = ifelse(any(type == "drought"), min(drought_depth, na.rm = TRUE), NA_real_),
    mean_duration = mean(duration_months, na.rm = TRUE)
  ), by = .(basin, basin_id, basin_name, member, window)]

  cesm_events_member <- cesm_events_summary[, .(
    H_max = median(H_max, na.rm = TRUE),
    D_max = median(D_max, na.rm = TRUE),
    mean_duration = median(mean_duration, na.rm = TRUE)
  ), by = .(basin, basin_id, basin_name, member)]

  cesm_metrics <- merge(cesm_metrics, cesm_events_member,
                        by = c("basin", "basin_id", "basin_name", "member"),
                        all.x = TRUE)

  # Regime persistence
  cesm_regime_member <- cesm_regime[, .(
    pluvial_ar1 = median(pluvial_ar1, na.rm = TRUE),
    drought_ar1 = median(drought_ar1, na.rm = TRUE)
  ), by = .(basin, basin_id, basin_name, member)]

  cesm_metrics <- merge(cesm_metrics, cesm_regime_member,
                        by = c("basin", "basin_id", "basin_name", "member"),
                        all.x = TRUE)
}

cesm_metrics[, model := "CESM2"]

# IPSL: Same process
ipsl_wavelets_member <- ipsl_wavelets[, .(
  power_1 = median(dominant_power_1, na.rm = TRUE),
  enso_power = median(model_enso_power, na.rm = TRUE),
  qd_power = median(model_qd_power, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

ipsl_persistence_member <- ipsl_persistence[, .(
  tau = median(tau, na.rm = TRUE),
  p_lf = median(p_lf, na.rm = TRUE)
), by = .(basin, basin_id, basin_name, member)]

ipsl_metrics <- merge(
  ipsl_wavelets_member,
  ipsl_persistence_member,
  by = c("basin", "basin_id", "basin_name", "member"),
  all = FALSE
)

if (phase06_available) {
  ipsl_events_summary <- events_models[model == "IPSL", .(
    H_max = ifelse(any(type == "pluvial"), max(pluvial_height, na.rm = TRUE), NA_real_),
    D_max = ifelse(any(type == "drought"), min(drought_depth, na.rm = TRUE), NA_real_),
    mean_duration = mean(duration_months, na.rm = TRUE)
  ), by = .(basin, basin_id, basin_name, member, window)]

  ipsl_events_member <- ipsl_events_summary[, .(
    H_max = median(H_max, na.rm = TRUE),
    D_max = median(D_max, na.rm = TRUE),
    mean_duration = median(mean_duration, na.rm = TRUE)
  ), by = .(basin, basin_id, basin_name, member)]

  ipsl_metrics <- merge(ipsl_metrics, ipsl_events_member,
                        by = c("basin", "basin_id", "basin_name", "member"),
                        all.x = TRUE)

  ipsl_regime_member <- ipsl_regime[, .(
    pluvial_ar1 = median(pluvial_ar1, na.rm = TRUE),
    drought_ar1 = median(drought_ar1, na.rm = TRUE)
  ), by = .(basin, basin_id, basin_name, member)]

  ipsl_metrics <- merge(ipsl_metrics, ipsl_regime_member,
                        by = c("basin", "basin_id", "basin_name", "member"),
                        all.x = TRUE)
}

ipsl_metrics[, model := "IPSL"]

cat("  CESM2 member-level metrics:", nrow(cesm_metrics), "rows\n")
cat("  IPSL member-level metrics:", nrow(ipsl_metrics), "rows\n\n")

# ==============================================================================
# HELPER FUNCTION: COMPUTE MAHALANOBIS ELLIPSE
# ==============================================================================

#' Compute 2D Mahalanobis ellipse at confidence level
#'
#' @param x Numeric vector (first metric)
#' @param y Numeric vector (second metric)
#' @param level Confidence level (default 0.95 for 95% ellipse)
#' @return data.frame with x, y coordinates of ellipse
compute_ellipse <- function(x, y, level = 0.95) {

  # Remove NA
  valid <- !is.na(x) & !is.na(y)
  x <- x[valid]
  y <- y[valid]

  if (length(x) < 3) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  # Compute mean and covariance
  mu <- c(mean(x), mean(y))
  sigma <- cov(cbind(x, y))

  # Add small ridge for stability
  sigma <- sigma + diag(1e-6, 2)

  # Compute ellipse (chi-square quantile for 2D)
  chi2_val <- qchisq(level, df = 2)

  ell <- ellipse::ellipse(sigma, centre = mu, level = level, npoints = 100)

  data.frame(x = ell[, 1], y = ell[, 2])
}

# ==============================================================================
# THEME
# ==============================================================================

theme_ellipse <- function() {
  theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      legend.position = "right",
      legend.key.size = unit(0.35, "cm"),
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 8, color = "gray30"),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8)
    )
}

# Color palette
model_colors <- c("CESM2" = "#1b9e77", "IPSL" = "#d95f02")
compat_colors <- c(
  "compatible" = "#2c7bb6",
  "marginal" = "#fdae61",
  "incompatible_outlier" = "#d7191c"
)

# ==============================================================================
# FIGURE 1: KEY METRIC PAIRS WITH ELLIPSES
# ==============================================================================

cat("Creating Figure 1: Key metric pair ellipses...\n")

# Select a few representative basins for detailed plotting
sample_basins <- c(1, 50, 100, 150)  # Basin indices

# Metric pairs to plot (most important combinations)
metric_pairs <- list(
  list(x = "p_lf", y = "tau",
       xlab = "Low-Frequency Power", ylab = "Memory Timescale (months)"),
  list(x = "enso_power", y = "qd_power",
       xlab = "ENSO Core Power (2-7 year band)", ylab = "Quasi-Decadal Power (7-15 year band)"),
  list(x = "H_max", y = "D_max",
       xlab = "Pluvial Height (mm)", ylab = "Drought Depth (mm)"),
  list(x = "pluvial_ar1", y = "drought_ar1",
       xlab = "Pluvial Regime Persistence", ylab = "Drought Regime Persistence")
)

# Check which metrics are available
available_metrics <- if (phase06_available) {
  c("p_lf", "tau", "enso_power", "qd_power", "H_max", "D_max",
    "pluvial_ar1", "drought_ar1", "mean_duration", "power_1")
} else {
  c("p_lf", "tau", "enso_power", "qd_power", "power_1")
}

# Filter metric pairs to only available ones
metric_pairs <- metric_pairs[sapply(metric_pairs, function(p) {
  p$x %in% available_metrics && p$y %in% available_metrics
})]

# Create plots for each metric pair
plot_list <- list()

for (i in seq_along(metric_pairs)) {
  pair <- metric_pairs[[i]]

  # Select one representative basin
  basin_idx <- sample_basins[min(i, length(sample_basins))]

  # Extract CESM2 data for this basin
  cesm_basin <- cesm_metrics[basin == basin_idx & !is.na(get(pair$x)) & !is.na(get(pair$y))]

  # Extract IPSL data for this basin
  ipsl_basin <- ipsl_metrics[basin == basin_idx & !is.na(get(pair$x)) & !is.na(get(pair$y))]

  # GRACE values
  grace_x <- compat[basin == basin_idx, get(paste0(pair$x, "_grace"))]
  grace_y <- compat[basin == basin_idx, get(paste0(pair$y, "_grace"))]
  grace_compat_cesm <- compat[basin == basin_idx, compat_class_cesm]
  grace_compat_ipsl <- compat[basin == basin_idx, compat_class_ipsl]

  # Skip if no data
  if (nrow(cesm_basin) == 0 || nrow(ipsl_basin) == 0) next

  # Compute ellipses
  cesm_ell_95 <- compute_ellipse(cesm_basin[[pair$x]], cesm_basin[[pair$y]], level = 0.95)
  cesm_ell_68 <- compute_ellipse(cesm_basin[[pair$x]], cesm_basin[[pair$y]], level = 0.68)

  ipsl_ell_95 <- compute_ellipse(ipsl_basin[[pair$x]], ipsl_basin[[pair$y]], level = 0.95)
  ipsl_ell_68 <- compute_ellipse(ipsl_basin[[pair$x]], ipsl_basin[[pair$y]], level = 0.68)

  if (nrow(cesm_ell_95) == 0 || nrow(ipsl_ell_95) == 0) next

  cesm_ell_95$model <- "CESM2"
  cesm_ell_68$model <- "CESM2"
  ipsl_ell_95$model <- "IPSL"
  ipsl_ell_68$model <- "IPSL"

  cesm_ell_95$level <- "95%"
  cesm_ell_68$level <- "68%"
  ipsl_ell_95$level <- "95%"
  ipsl_ell_68$level <- "68%"

  all_ellipses <- rbind(cesm_ell_95, cesm_ell_68, ipsl_ell_95, ipsl_ell_68)

  # Combine member data
  all_members <- rbind(
    cesm_basin[, .(x = get(pair$x), y = get(pair$y), model = "CESM2")],
    ipsl_basin[, .(x = get(pair$x), y = get(pair$y), model = "IPSL")]
  )

  # Create plot
  p <- ggplot() +
    # Model ensemble members (semi-transparent points)
    geom_point(data = all_members, aes(x = x, y = y, color = model),
               alpha = 0.15, size = 0.8) +
    # 95% ellipses
    geom_path(data = all_ellipses[all_ellipses$level == "95%", ],
              aes(x = x, y = y, color = model, linetype = level),
              linewidth = 0.8) +
    # 68% ellipses
    geom_path(data = all_ellipses[all_ellipses$level == "68%", ],
              aes(x = x, y = y, color = model, linetype = level),
              linewidth = 0.6) +
    # GRACE observation
    geom_point(aes(x = grace_x, y = grace_y),
               color = "black", fill = "gold", shape = 23, size = 3, stroke = 1) +
    scale_color_manual(values = model_colors, name = "Model") +
    scale_linetype_manual(values = c("95%" = "solid", "68%" = "dashed"), name = "CI") +
    labs(
      x = pair$xlab,
      y = pair$ylab,
      title = paste0("Basin ", basin_idx, ": ", compat$basin_name[basin_idx]),
      subtitle = paste0("GRACE compatibility: CESM2 = ", grace_compat_cesm,
                       " | IPSL = ", grace_compat_ipsl)
    ) +
    theme_ellipse()

  plot_list[[i]] <- p
}

# Combine all plots
if (length(plot_list) > 0) {
  fig1 <- wrap_plots(plot_list, ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")

  # Save
  ggsave(
    filename = file.path(fig_dir, "fig08_metric_ellipses_examples.pdf"),
    plot = fig1,
    width = 11,
    height = 10,
    units = "in"
  )

  ggsave(
    filename = file.path(fig_dir, "fig08_metric_ellipses_examples.png"),
    plot = fig1,
    width = 11,
    height = 10,
    units = "in",
    dpi = 300
  )

  cat("  ✓ Figure 1 saved (", length(plot_list), "panels )\n\n")
}

# ==============================================================================
# FIGURE 2: DENSITY CONTOURS FOR ALL BASINS (COMBINED)
# ==============================================================================

cat("Creating Figure 2: Global density contours...\n")

# Combine all basins for global view
if (phase06_available) {

  plot_list_global <- list()

  for (i in seq_along(metric_pairs)) {
    pair <- metric_pairs[[i]]

    # All CESM2 data
    cesm_all <- cesm_metrics[!is.na(get(pair$x)) & !is.na(get(pair$y))]

    # All IPSL data
    ipsl_all <- ipsl_metrics[!is.na(get(pair$x)) & !is.na(get(pair$y))]

    # All GRACE data
    grace_all <- compat[, .(
      x = get(paste0(pair$x, "_grace")),
      y = get(paste0(pair$y, "_grace")),
      compat_cesm = compat_class_cesm,
      compat_ipsl = compat_class_ipsl
    )]
    grace_all <- grace_all[!is.na(x) & !is.na(y)]

    if (nrow(cesm_all) == 0 || nrow(ipsl_all) == 0 || nrow(grace_all) == 0) next

    # Create plot with density contours
    p <- ggplot() +
      # CESM2 density contours
      stat_density_2d(data = cesm_all,
                      aes(x = get(pair$x), y = get(pair$y), color = "CESM2"),
                      linewidth = 0.5, bins = 8, alpha = 0.7) +
      # IPSL density contours
      stat_density_2d(data = ipsl_all,
                      aes(x = get(pair$x), y = get(pair$y), color = "IPSL"),
                      linewidth = 0.5, bins = 8, alpha = 0.7) +
      # GRACE points colored by CESM2 compatibility
      geom_point(data = grace_all,
                 aes(x = x, y = y, fill = compat_cesm),
                 shape = 21, size = 1.5, color = "black", stroke = 0.3, alpha = 0.8) +
      scale_color_manual(values = model_colors, name = "Model Ensemble") +
      scale_fill_manual(values = compat_colors, name = "GRACE\nCompatibility") +
      labs(
        x = pair$xlab,
        y = pair$ylab,
        title = paste0(pair$xlab, " vs ", pair$ylab),
        subtitle = "All basins | Contours = model ensemble density"
      ) +
      theme_ellipse()

    plot_list_global[[i]] <- p
  }

  # Combine
  if (length(plot_list_global) > 0) {
    fig2 <- wrap_plots(plot_list_global, ncol = 2, guides = "collect") &
      theme(legend.position = "bottom")

    # Save
    ggsave(
      filename = file.path(fig_dir, "fig08_metric_density_contours.pdf"),
      plot = fig2,
      width = 11,
      height = 10,
      units = "in"
    )

    ggsave(
      filename = file.path(fig_dir, "fig08_metric_density_contours.png"),
      plot = fig2,
      width = 11,
      height = 10,
      units = "in",
      dpi = 300
    )

    cat("  ✓ Figure 2 saved (", length(plot_list_global), "panels )\n\n")
  }
}

# ==============================================================================
# FIGURE 3: COMBINED ELLIPSE OVERLAY FOR INCOMPATIBLE BASINS
# ==============================================================================

cat("Creating Figure 3: Incompatible basin highlights...\n")

# Find basins incompatible with both models
incomp_both <- compat[compat_class_cesm == "incompatible_outlier" &
                        compat_class_ipsl == "incompatible_outlier"]

if (nrow(incomp_both) > 0) {

  cat("  Found", nrow(incomp_both), "basins incompatible with both models\n")

  # Select up to 6 basins for detailed plots
  selected_basins <- head(incomp_both$basin, 6)

  plot_list_incomp <- list()

  for (basin_idx in selected_basins) {

    # Use first metric pair
    pair <- metric_pairs[[1]]

    # Extract data
    cesm_basin <- cesm_metrics[basin == basin_idx & !is.na(get(pair$x)) & !is.na(get(pair$y))]
    ipsl_basin <- ipsl_metrics[basin == basin_idx & !is.na(get(pair$x)) & !is.na(get(pair$y))]

    grace_x <- compat[basin == basin_idx, get(paste0(pair$x, "_grace"))]
    grace_y <- compat[basin == basin_idx, get(paste0(pair$y, "_grace"))]

    if (nrow(cesm_basin) == 0 || nrow(ipsl_basin) == 0) next

    # Compute ellipses
    cesm_ell <- compute_ellipse(cesm_basin[[pair$x]], cesm_basin[[pair$y]], level = 0.95)
    ipsl_ell <- compute_ellipse(ipsl_basin[[pair$x]], ipsl_basin[[pair$y]], level = 0.95)

    if (nrow(cesm_ell) == 0 || nrow(ipsl_ell) == 0) next

    cesm_ell$model <- "CESM2"
    ipsl_ell$model <- "IPSL"
    all_ell <- rbind(cesm_ell, ipsl_ell)

    # Plot
    p <- ggplot() +
      geom_polygon(data = cesm_ell, aes(x = x, y = y),
                   fill = model_colors["CESM2"], alpha = 0.15, color = model_colors["CESM2"],
                   linewidth = 0.8) +
      geom_polygon(data = ipsl_ell, aes(x = x, y = y),
                   fill = model_colors["IPSL"], alpha = 0.15, color = model_colors["IPSL"],
                   linewidth = 0.8) +
      geom_point(aes(x = grace_x, y = grace_y),
                 color = "black", fill = "#d7191c", shape = 23, size = 4, stroke = 1.2) +
      labs(
        x = pair$xlab,
        y = pair$ylab,
        title = compat[basin == basin_idx, basin_name],
        subtitle = "Incompatible with both CESM2 and IPSL"
      ) +
      theme_ellipse()

    plot_list_incomp[[length(plot_list_incomp) + 1]] <- p
  }

  # Combine
  if (length(plot_list_incomp) > 0) {
    fig3 <- wrap_plots(plot_list_incomp, ncol = 3)

    # Save
    ggsave(
      filename = file.path(fig_dir, "fig08_incompatible_basins_ellipses.pdf"),
      plot = fig3,
      width = 12,
      height = 8,
      units = "in"
    )

    ggsave(
      filename = file.path(fig_dir, "fig08_incompatible_basins_ellipses.png"),
      plot = fig3,
      width = 12,
      height = 8,
      units = "in",
      dpi = 300
    )

    cat("  ✓ Figure 3 saved (", length(plot_list_incomp), "panels )\n\n")
  }
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("============================================================================\n")
cat("PHASE 08 ELLIPSE FIGURES COMPLETE\n")
cat("============================================================================\n\n")

cat("Generated figures:\n")
cat("  1. fig08_metric_ellipses_examples.{pdf,png} - Key metric pairs with ellipses\n")
if (phase06_available) {
  cat("  2. fig08_metric_density_contours.{pdf,png} - Global density contours\n")
}
if (nrow(incomp_both) > 0) {
  cat("  3. fig08_incompatible_basins_ellipses.{pdf,png} - Incompatible basin highlights\n")
}
cat("\nAll figures saved to:", fig_dir, "\n\n")
