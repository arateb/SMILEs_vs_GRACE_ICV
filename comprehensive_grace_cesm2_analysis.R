# ============================================================================
# COMPREHENSIVE GRACE vs CESM2 ANALYSIS - PUBLICATION FIGURES
# ============================================================================
# Author:Ashraf Rateb
# Date: 2025-11-16
# ============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggthemes)
  library(scales)
  library(viridis)
  library(scico)
  library(patchwork)
  library(corrplot)
  library(RColorBrewer)
})

# ============================================================================
# SETUP: FOLDER STRUCTURE
# ============================================================================

# Create dated folder structure
today_folder <- paste0("figures_", Sys.Date())
dir.create(today_folder, showWarnings = FALSE)

subfolders <- c(
  "01_main_comparisons",
  "02_performance_summary",
  "03_classification",
  "04_bias_analysis",
  "05_coverage",
  "06_irrigation_effects",
  "07_cross_metric",
  "08_ensemble_spread"
)

for (folder in subfolders) {
  dir.create(file.path(today_folder, folder), showWarnings = FALSE)
}

cat("\n=================================================================\n")
cat("COMPREHENSIVE GRACE vs CESM2 ANALYSIS\n")
cat("=================================================================\n")
cat("Output folder:", today_folder, "\n")
cat("Subfolders created:", length(subfolders), "\n\n")

# ============================================================================
# LOAD AND PREPARE DATA
# ============================================================================

cat("Loading data...\n")
G <- readRDS('analysis/GGFo_vs_MMILEs_Comparison_UpdatedAug25.rds')
amplsd_aug <- G$updated_AMp_SD_Min_MAC

# Filter for CESM2 only and prepare data
cesm_data <- amplsd_aug %>%
  filter(model_name == "CESM2") %>%
  mutate(
    # CRITICAL: Convert basin names from ALL CAPS to Proper Case
    River = str_to_title(River),

    # Create performance indicators: is GRACE inside 5-95% envelope?
    amp_inside = (amp_grace >= q5_amp) & (amp_grace <= q95_amp),
    sd_inside = (sd_grace >= q5_sd) & (sd_grace <= q95_sd),
    min_inside = (min_grace >= q5_min) & (min_grace <= q95_min),
    max_inside = (max_grace >= q5_max) & (max_grace <= q95_max),

    # Bias metrics (GRACE - ensemble median)
    bias_amp = amp_grace - q50_amp,
    bias_sd = sd_grace - q50_sd,
    bias_min = min_grace - q50_min,
    bias_max = max_grace - q50_max,

    # Overall performance score (mean of u-scores)
    mean_u = rowMeans(cbind(u_amp, u_sd, u_min, u_max), na.rm = TRUE),

    # Overall CRPS score (mean of nCRPS)
    mean_nCRPS = rowMeans(cbind(nCRPS_amp, nCRPS_sd, nCRPS_min, nCRPS_max), na.rm = TRUE)
  )

cat("Data loaded. Basins:", nrow(cesm_data), "\n")
cat("Basin names converted to proper case\n\n")

# ============================================================================
# DEFINE BASIN CATEGORIES
# ============================================================================

cat("Defining basin categories...\n")

# Category 1: Top 50 irrigated basins
top50_irrigated <- cesm_data %>%
  arrange(desc(Irrig_pct)) %>%
  slice_head(n = 50) %>%
  mutate(
    category = "Top 50 Irrigated",
    basin_label = paste0(River, " (", round(Irrig_pct, 1), "%)")
  )

# Category 2: Humid only (Arid == "H")
humid_basins <- cesm_data %>%
  filter(Arid == "H") %>%
  arrange(desc(Area_km2)) %>%
  mutate(category = "Humid (H)")

# Category 3: Arid only (Arid == "A")
arid_basins <- cesm_data %>%
  filter(Arid == "A") %>%
  arrange(desc(Area_km2)) %>%
  mutate(category = "Arid (A)")

# Category 4: Semi-Arid only (Arid == "SA")
semiarid_basins <- cesm_data %>%
  filter(Arid == "SA") %>%
  arrange(desc(Area_km2)) %>%
  mutate(category = "Semi-Arid (SA)")

cat("Categories defined:\n")
cat("  Top 50 Irrigated: ", nrow(top50_irrigated), "basins\n")
cat("  Humid (H):        ", nrow(humid_basins), "basins\n")
cat("  Arid (A):         ", nrow(arid_basins), "basins\n")
cat("  Semi-Arid (SA):   ", nrow(semiarid_basins), "basins\n\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Standardized save function
save_figure <- function(plot, filename, subfolder, width = 12, height = 10) {
  # PDF version
  ggsave(
    filename = file.path(today_folder, subfolder, paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    dpi = 600,
    device = "pdf"
  )

  # PNG version
  ggsave(
    filename = file.path(today_folder, subfolder, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 600,
    device = "png"
  )

  cat("  ✓", filename, "\n")
}

# Climate type color palette
climate_colors <- c(
  "H" = "#1b9e77",   # Humid - green
  "A" = "#d95f02",   # Arid - orange
  "SA" = "#7570b3",  # Semi-Arid - purple
  "SH" = "#e7298a"   # Sub-Humid - pink
)

# ============================================================================
# PART 1: MAIN COMPARISON PLOTS (16 figures)
# ============================================================================

cat("\n=================================================================\n")
cat("PART 1: MAIN COMPARISON PLOTS (16 figures)\n")
cat("=================================================================\n\n")

plot_ensemble_vs_grace <- function(data,
                                   metric = "amp",
                                   category_name = "All Basins",
                                   metric_label = "Amplitude",
                                   output_filename = "amplitude_all",
                                   top_n = 50,
                                   show_irrigation = FALSE) {

  # Define column names based on metric
  grace_col <- paste0(metric, "_grace")
  q5_col <- paste0("q5_", metric)
  q50_col <- paste0("q50_", metric)
  q95_col <- paste0("q95_", metric)
  inside_col <- paste0(metric, "_inside")

  # Select and order data
  plot_data <- data %>%
    slice_head(n = min(top_n, nrow(data)))

  # Create basin labels
  if (show_irrigation) {
    plot_data <- plot_data %>%
      mutate(basin_label = factor(basin_label, levels = rev(basin_label)))
  } else {
    plot_data <- plot_data %>%
      mutate(basin_label = factor(River, levels = rev(River)))
  }

  # Create plot
  p <- ggplot(plot_data, aes(y = basin_label)) +
    # Ensemble range (q5 to q95)
    geom_linerange(
      aes(xmin = .data[[q5_col]],
          xmax = .data[[q95_col]],
          color = .data[[inside_col]]),
      linewidth = 1.5,
      alpha = 0.6
    ) +
    # Ensemble median (q50)
    geom_point(
      aes(x = .data[[q50_col]],
          color = .data[[inside_col]]),
      size = 3,
      shape = 21,
      fill = "white",
      stroke = 1.5
    ) +
    # GRACE observation
    geom_point(
      aes(x = .data[[grace_col]],
          fill = .data[[inside_col]]),
      size = 5,
      shape = 23,
      color = "black",
      stroke = 0.8
    ) +
    # Color scale
    scale_color_manual(
      values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
      labels = c("TRUE" = "GRACE inside 5-95%", "FALSE" = "GRACE outside 5-95%"),
      name = "Coverage"
    ) +
    scale_fill_manual(
      values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
      labels = c("TRUE" = "GRACE inside 5-95%", "FALSE" = "GRACE outside 5-95%"),
      name = "Coverage"
    ) +
    # Labels
    labs(
      title = paste0(category_name, ": ", metric_label),
      subtitle = paste0("GRACE observations vs CESM2 ensemble (n=", nrow(plot_data), " basins)"),
      x = paste0(metric_label, " (mm)"),
      y = if(show_irrigation) "Basin (Irrigation %)" else "Basin",
      caption = "Line: CESM2 5-95% envelope | Circle: CESM2 median | Diamond: GRACE observation"
    ) +
    # Theme
    theme_clean(base_size = 16) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 12),
      axis.title = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 18, hjust = 0),
      plot.subtitle = element_text(size = 13, hjust = 0, color = "gray30"),
      plot.caption = element_text(size = 10, hjust = 0, color = "gray50"),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    )

  save_figure(p, output_filename, "01_main_comparisons", width = 12, height = 10)

  return(p)
}

# Category 1: Top 50 Irrigated Basins
cat("Category 1: Top 50 Irrigated Basins\n")
plot_ensemble_vs_grace(top50_irrigated, "amp", "Top 50 Irrigated Basins",
                       "Amplitude", "amplitude_top50irrigated", 50, TRUE)
plot_ensemble_vs_grace(top50_irrigated, "sd", "Top 50 Irrigated Basins",
                       "Standard Deviation", "sd_top50irrigated", 50, TRUE)
plot_ensemble_vs_grace(top50_irrigated, "min", "Top 50 Irrigated Basins",
                       "Minimum (Drought Depth)", "min_top50irrigated", 50, TRUE)
plot_ensemble_vs_grace(top50_irrigated, "max", "Top 50 Irrigated Basins",
                       "Maximum (Pluvial Height)", "max_top50irrigated", 50, TRUE)

# Category 2: Humid Basins
cat("\nCategory 2: Humid Basins\n")
plot_ensemble_vs_grace(humid_basins, "amp", "Humid Basins",
                       "Amplitude", "amplitude_humid", 50, FALSE)
plot_ensemble_vs_grace(humid_basins, "sd", "Humid Basins",
                       "Standard Deviation", "sd_humid", 50, FALSE)
plot_ensemble_vs_grace(humid_basins, "min", "Humid Basins",
                       "Minimum (Drought Depth)", "min_humid", 50, FALSE)
plot_ensemble_vs_grace(humid_basins, "max", "Humid Basins",
                       "Maximum (Pluvial Height)", "max_humid", 50, FALSE)

# Category 3: Arid Basins
cat("\nCategory 3: Arid Basins\n")
plot_ensemble_vs_grace(arid_basins, "amp", "Arid Basins",
                       "Amplitude", "amplitude_arid", nrow(arid_basins), FALSE)
plot_ensemble_vs_grace(arid_basins, "sd", "Arid Basins",
                       "Standard Deviation", "sd_arid", nrow(arid_basins), FALSE)
plot_ensemble_vs_grace(arid_basins, "min", "Arid Basins",
                       "Minimum (Drought Depth)", "min_arid", nrow(arid_basins), FALSE)
plot_ensemble_vs_grace(arid_basins, "max", "Arid Basins",
                       "Maximum (Pluvial Height)", "max_arid", nrow(arid_basins), FALSE)

# Category 4: Semi-Arid Basins
cat("\nCategory 4: Semi-Arid Basins\n")
plot_ensemble_vs_grace(semiarid_basins, "amp", "Semi-Arid Basins",
                       "Amplitude", "amplitude_semiarid", 50, FALSE)
plot_ensemble_vs_grace(semiarid_basins, "sd", "Semi-Arid Basins",
                       "Standard Deviation", "sd_semiarid", 50, FALSE)
plot_ensemble_vs_grace(semiarid_basins, "min", "Semi-Arid Basins",
                       "Minimum (Drought Depth)", "min_semiarid", 50, FALSE)
plot_ensemble_vs_grace(semiarid_basins, "max", "Semi-Arid Basins",
                       "Maximum (Pluvial Height)", "max_semiarid", 50, FALSE)

cat("\n✓ Part 1 complete: 16 figures saved\n")

# ============================================================================
# PART 2: PERFORMANCE SUMMARY PLOTS (13 figures)
# ============================================================================

cat("\n=================================================================\n")
cat("PART 2: PERFORMANCE SUMMARY PLOTS (13 figures)\n")
cat("=================================================================\n\n")

# 2.1 Reliability Scores (u-scores) - 4 figures
cat("2.1 Reliability Scores (u-scores)\n")

for (metric in c("amp", "sd", "min", "max")) {
  u_col <- paste0("u_", metric)
  metric_label <- case_when(
    metric == "amp" ~ "Amplitude",
    metric == "sd" ~ "Standard Deviation",
    metric == "min" ~ "Minimum",
    metric == "max" ~ "Maximum"
  )

  plot_data <- cesm_data %>%
    arrange(.data[[u_col]]) %>%
    slice_head(n = 50) %>%
    mutate(River = factor(River, levels = River))

  p <- ggplot(plot_data, aes(x = .data[[u_col]], y = River, fill = Arid)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = 0.8) +
    scale_fill_manual(values = climate_colors, name = "Climate") +
    labs(
      title = paste0("Reliability Score (u): ", metric_label),
      subtitle = "Top 50 basins with lowest u-scores (worst calibration)",
      x = "u-score (ideal = 0.5 for uniform distribution)",
      y = "Basin",
      caption = "Lower u-scores indicate GRACE below ensemble; higher indicates GRACE above ensemble"
    ) +
    theme_clean(base_size = 16) +
    theme(
      legend.position = "right",
      axis.text.y = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 13, color = "gray30")
    )

  save_figure(p, paste0("u_score_", metric), "02_performance_summary")
}

# 2.2 Spread Scores (S) - 4 figures
cat("\n2.2 Spread Scores (S)\n")

for (metric in c("amp", "sd", "min", "max")) {
  S_col <- paste0("S_", metric)
  metric_label <- case_when(
    metric == "amp" ~ "Amplitude",
    metric == "sd" ~ "Standard Deviation",
    metric == "min" ~ "Minimum",
    metric == "max" ~ "Maximum"
  )

  plot_data <- cesm_data %>%
    arrange(desc(abs(.data[[S_col]]))) %>%
    slice_head(n = 50) %>%
    mutate(River = factor(River, levels = River))

  p <- ggplot(plot_data, aes(x = .data[[S_col]], y = River, fill = Arid)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
    scale_fill_manual(values = climate_colors, name = "Climate") +
    labs(
      title = paste0("Spread Score (S): ", metric_label),
      subtitle = "Top 50 basins with largest spread deviation",
      x = "S-score (0 = perfect spread)",
      y = "Basin",
      caption = "Negative: under-dispersed (too narrow) | Positive: over-dispersed (too wide)"
    ) +
    theme_clean(base_size = 16) +
    theme(
      legend.position = "right",
      axis.text.y = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 18)
    )

  save_figure(p, paste0("S_score_", metric), "02_performance_summary")
}

# 2.3 CRPS Scores - 4 figures
cat("\n2.3 CRPS Scores\n")

for (metric in c("amp", "sd", "min", "max")) {
  nCRPS_col <- paste0("nCRPS_", metric)
  metric_label <- case_when(
    metric == "amp" ~ "Amplitude",
    metric == "sd" ~ "Standard Deviation",
    metric == "min" ~ "Minimum",
    metric == "max" ~ "Maximum"
  )

  plot_data <- cesm_data %>%
    arrange(desc(.data[[nCRPS_col]])) %>%
    slice_head(n = 50) %>%
    mutate(River = factor(River, levels = River))

  p <- ggplot(plot_data, aes(x = .data[[nCRPS_col]], y = River, fill = Arid)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = climate_colors, name = "Climate") +
    labs(
      title = paste0("Normalized CRPS: ", metric_label),
      subtitle = "Top 50 basins with highest CRPS (worst performance)",
      x = "Normalized CRPS (lower is better)",
      y = "Basin",
      caption = "CRPS = Continuous Ranked Probability Score (ensemble skill)"
    ) +
    theme_clean(base_size = 16) +
    theme(
      legend.position = "right",
      axis.text.y = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 18)
    )

  save_figure(p, paste0("nCRPS_", metric), "02_performance_summary")
}

# 2.4 Combined Performance Dashboard - 1 figure
cat("\n2.4 Combined Performance Dashboard\n")

perf_long <- cesm_data %>%
  select(River, Arid, u_amp, u_sd, u_min, u_max,
         S_amp, S_sd, S_min, S_max) %>%
  pivot_longer(
    cols = c(starts_with("u_"), starts_with("S_")),
    names_to = c("score_type", "metric"),
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, levels = c("amp", "sd", "min", "max"),
                   labels = c("Amplitude", "SD", "Min", "Max")),
    score_type = factor(score_type, levels = c("u", "S"),
                       labels = c("Reliability (u)", "Spread (S)"))
  )

p <- ggplot(perf_long, aes(x = value, fill = Arid)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_grid(score_type ~ metric, scales = "free") +
  scale_fill_manual(values = climate_colors, name = "Climate") +
  labs(
    title = "Performance Dashboard: All Metrics",
    subtitle = "Distribution of reliability and spread scores across basins",
    x = "Score Value",
    y = "Count"
  ) +
  theme_clean(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18),
    strip.text = element_text(face = "bold", size = 12)
  )

save_figure(p, "performance_dashboard", "02_performance_summary", width = 14, height = 10)

cat("\n✓ Part 2 complete: 13 figures saved\n")

# Save progress marker
cat("\n[Progress checkpoint: Parts 1-2 completed]\n")
cat("Continuing to Part 3...\n")
