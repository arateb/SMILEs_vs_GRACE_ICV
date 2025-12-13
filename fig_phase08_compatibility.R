#!/usr/bin/env Rscript
# ==============================================================================
# FIGURE: PHASE 08 COMPATIBILITY ANALYSIS
# ==============================================================================
#
# Comprehensive visualization of 10-metric compatibility analysis:
# - Compatibility index distributions (C_b) for CESM2 and IPSL
# - Mahalanobis distance distributions
# - Metric-by-metric comparison (GRACE vs model medians)
# - Basin-level compatibility classification
# - Geographic patterns of incompatibility
#
# Author: Claude Code
# Date: November 2025
# ==============================================================================

library(data.table)
library(ggplot2)
library(patchwork)
library(sf)
library(rnaturalearth)
library(scales)

# Set output directory
fig_dir <- "outputs/figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading Phase 08 compatibility data...\n")

# Compatibility results
compat <- readRDS("outputs/phase08_compatibility_basin.rds")

# Basin attributes with geometry
attrs <- readRDS("data/Enhanced_GGFO_MMLEs_Nov2025.rds")$attrs

cat("  Loaded", nrow(compat), "basins\n")
cat("  CESM2 compatibility computed for", sum(!is.na(compat$C_b_cesm)), "basins\n")
cat("  IPSL compatibility computed for", sum(!is.na(compat$C_b_ipsl)), "basins\n\n")

# ==============================================================================
# THEME SETTINGS
# ==============================================================================

theme_paper <- function() {
  theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, color = "gray30"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9)
    )
}

# Color palette for compatibility classes
compat_colors <- c(
  "compatible" = "#2c7bb6",           # Blue
  "marginal" = "#fdae61",             # Orange
  "incompatible_outlier" = "#d7191c" # Red
)

# ==============================================================================
# FIGURE 1: COMPATIBILITY INDEX DISTRIBUTIONS
# ==============================================================================

cat("Creating Figure 1: Compatibility index distributions...\n")

# Prepare data for plotting
compat_long <- rbind(
  data.table(
    basin_id = compat$basin_id,
    basin_name = compat$basin_name,
    model = "CESM2",
    C_b = compat$C_b_cesm,
    compat_class = compat$compat_class_cesm
  ),
  data.table(
    basin_id = compat$basin_id,
    basin_name = compat$basin_name,
    model = "IPSL",
    C_b = compat$C_b_ipsl,
    compat_class = compat$compat_class_ipsl
  )
)

# Remove NA values
compat_long <- compat_long[!is.na(C_b)]

# Panel A: Histogram with compatibility zones
p1a <- ggplot(compat_long, aes(x = C_b, fill = compat_class)) +
  # Compatibility zones as rectangles
  annotate("rect", xmin = 0, xmax = 0.05, ymin = 0, ymax = Inf,
           fill = "#d7191c", alpha = 0.1) +
  annotate("rect", xmin = 0.05, xmax = 0.10, ymin = 0, ymax = Inf,
           fill = "#fdae61", alpha = 0.1) +
  annotate("rect", xmin = 0.10, xmax = 0.90, ymin = 0, ymax = Inf,
           fill = "#2c7bb6", alpha = 0.05) +
  annotate("rect", xmin = 0.90, xmax = 0.95, ymin = 0, ymax = Inf,
           fill = "#fdae61", alpha = 0.1) +
  annotate("rect", xmin = 0.95, xmax = 1.0, ymin = 0, ymax = Inf,
           fill = "#d7191c", alpha = 0.1) +
  geom_histogram(bins = 30, color = "white", linewidth = 0.2, alpha = 0.8) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  scale_fill_manual(values = compat_colors, name = "Classification") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  facet_wrap(~ model, ncol = 1, scales = "free_y") +
  labs(
    x = "Compatibility Index (C_b)",
    y = "Number of Basins",
    title = "A) Distribution of Compatibility Indices",
    subtitle = "C_b = percentile rank of GRACE Mahalanobis distance in model ensemble"
  ) +
  theme_paper()

# Panel B: Density plot with median lines
p1b <- ggplot(compat_long, aes(x = C_b, color = model, fill = model)) +
  geom_density(alpha = 0.2, linewidth = 0.8) +
  geom_vline(data = compat_long[, .(median_C_b = median(C_b, na.rm = TRUE)), by = model],
             aes(xintercept = median_C_b, color = model),
             linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "gray30", linewidth = 0.5) +
  scale_color_manual(values = c("CESM2" = "#1b9e77", "IPSL" = "#d95f02"), name = "Model") +
  scale_fill_manual(values = c("CESM2" = "#1b9e77", "IPSL" = "#d95f02"), name = "Model") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  labs(
    x = "Compatibility Index (C_b)",
    y = "Density",
    title = "B) Model Comparison",
    subtitle = "GRACE compatibility with CESM2 vs IPSL ensembles"
  ) +
  theme_paper()

# Combine panels
fig1 <- p1a / p1b +
  plot_layout(heights = c(2, 1), guides = "collect") &
  theme(legend.position = "bottom")

# Save
ggsave(
  filename = file.path(fig_dir, "fig08_compatibility_distributions.pdf"),
  plot = fig1,
  width = 8,
  height = 9,
  units = "in"
)

ggsave(
  filename = file.path(fig_dir, "fig08_compatibility_distributions.png"),
  plot = fig1,
  width = 8,
  height = 9,
  units = "in",
  dpi = 300
)

cat("  ✓ Figure 1 saved\n\n")

# ==============================================================================
# FIGURE 2: MAHALANOBIS DISTANCE DISTRIBUTIONS
# ==============================================================================

cat("Creating Figure 2: Mahalanobis distance distributions...\n")

# Prepare data
mahal_long <- rbind(
  data.table(
    basin_id = compat$basin_id,
    basin_name = compat$basin_name,
    model = "CESM2",
    d_mahal = compat$d_mahal_cesm,
    compat_class = compat$compat_class_cesm
  ),
  data.table(
    basin_id = compat$basin_id,
    basin_name = compat$basin_name,
    model = "IPSL",
    d_mahal = compat$d_mahal_ipsl,
    compat_class = compat$compat_class_ipsl
  )
)

mahal_long <- mahal_long[!is.na(d_mahal)]

# Panel A: Histogram by model
p2a <- ggplot(mahal_long, aes(x = d_mahal, fill = model)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity", color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("CESM2" = "#1b9e77", "IPSL" = "#d95f02"), name = "Model") +
  scale_x_continuous(trans = "log10", labels = label_comma()) +
  labs(
    x = "Mahalanobis Distance (log scale)",
    y = "Number of Basins",
    title = "A) Mahalanobis Distance Distributions",
    subtitle = "Distance from GRACE to model ensemble center in 10-metric space"
  ) +
  theme_paper()

# Panel B: Mahalanobis distance by compatibility class
p2b <- ggplot(mahal_long[!is.na(compat_class)],
              aes(x = compat_class, y = d_mahal, fill = compat_class)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_manual(values = compat_colors, name = "Classification") +
  scale_y_continuous(trans = "log10", labels = label_comma()) +
  facet_wrap(~ model, ncol = 2) +
  labs(
    x = "Compatibility Classification",
    y = "Mahalanobis Distance (log scale)",
    title = "B) Distance by Classification"
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine
fig2 <- p2a / p2b +
  plot_layout(heights = c(1, 1.2), guides = "collect") &
  theme(legend.position = "bottom")

# Save
ggsave(
  filename = file.path(fig_dir, "fig08_mahalanobis_distances.pdf"),
  plot = fig2,
  width = 9,
  height = 8,
  units = "in"
)

ggsave(
  filename = file.path(fig_dir, "fig08_mahalanobis_distances.png"),
  plot = fig2,
  width = 9,
  height = 8,
  units = "in",
  dpi = 300
)

cat("  ✓ Figure 2 saved\n\n")

# ==============================================================================
# FIGURE 3: METRIC-BY-METRIC COMPARISON (10 METRICS)
# ==============================================================================

cat("Creating Figure 3: Metric-by-metric comparisons...\n")

# Prepare metric comparison data
metric_names <- c("p_lf", "tau", "H_max", "D_max", "pluvial_ar1", "drought_ar1",
                  "mean_duration", "power_1", "enso_power", "qd_power")

metric_labels <- c(
  "p_lf" = "Low-Freq Power",
  "tau" = "Memory τ (months)",
  "H_max" = "Pluvial Height (mm)",
  "D_max" = "Drought Depth (mm)",
  "pluvial_ar1" = "Pluvial AR(1)",
  "drought_ar1" = "Drought AR(1)",
  "mean_duration" = "Mean Duration (months)",
  "power_1" = "Dominant Power",
  "enso_power" = "ENSO Power",
  "qd_power" = "QD Power"
)

# Build long-format data for all 10 metrics
metric_comparison_list <- list()

for (metric in metric_names) {
  grace_col <- paste0(metric, "_grace")
  cesm_col <- paste0(metric, "_model_median_cesm")
  ipsl_col <- paste0(metric, "_model_median_ipsl")

  metric_comparison_list[[metric]] <- rbind(
    data.table(
      basin_id = compat$basin_id,
      basin_name = compat$basin_name,
      metric = metric_labels[metric],
      model = "CESM2",
      grace_value = compat[[grace_col]],
      model_value = compat[[cesm_col]],
      compat_class = compat$compat_class_cesm
    ),
    data.table(
      basin_id = compat$basin_id,
      basin_name = compat$basin_name,
      metric = metric_labels[metric],
      model = "IPSL",
      grace_value = compat[[grace_col]],
      model_value = compat[[ipsl_col]],
      compat_class = compat$compat_class_ipsl
    )
  )
}

metric_comparison <- rbindlist(metric_comparison_list)
metric_comparison <- metric_comparison[!is.na(grace_value) & !is.na(model_value)]

# Create faceted scatter plot (5x2 grid for 10 metrics)
p3 <- ggplot(metric_comparison, aes(x = grace_value, y = model_value, color = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linewidth = 0.7) +
  scale_color_manual(values = c("CESM2" = "#1b9e77", "IPSL" = "#d95f02"), name = "Model") +
  facet_wrap(~ metric, scales = "free", ncol = 2) +
  labs(
    x = "GRACE Metric Value",
    y = "Model Ensemble Median",
    title = "Metric-by-Metric Comparison: GRACE vs Model Ensembles",
    subtitle = "10 comprehensive metrics | Dashed line = 1:1 reference"
  ) +
  theme_paper() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 7)
  )

# Save
ggsave(
  filename = file.path(fig_dir, "fig08_metric_comparison.pdf"),
  plot = p3,
  width = 10,
  height = 14,
  units = "in"
)

ggsave(
  filename = file.path(fig_dir, "fig08_metric_comparison.png"),
  plot = p3,
  width = 10,
  height = 14,
  units = "in",
  dpi = 300
)

cat("  ✓ Figure 3 saved\n\n")

# ==============================================================================
# FIGURE 4: BASIN-LEVEL COMPATIBILITY MAPS
# ==============================================================================

cat("Creating Figure 4: Geographic patterns of compatibility...\n")

# Prepare spatial data
compat_sf <- merge(
  st_as_sf(attrs),
  compat,
  by.x = "ID",
  by.y = "basin_id",
  all.x = TRUE
)

# World basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# Panel A: CESM2 compatibility
p4a <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80", linewidth = 0.2) +
  geom_sf(data = compat_sf[!is.na(compat_sf$compat_class_cesm), ],
          aes(fill = compat_class_cesm), color = "gray30", linewidth = 0.3) +
  scale_fill_manual(
    values = compat_colors,
    name = "Classification",
    na.value = "gray90"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "A) CESM2 Compatibility",
    subtitle = paste0("Compatible: ", sum(compat_sf$compat_class_cesm == "compatible", na.rm = TRUE), " basins | ",
                     "Incompatible: ", sum(compat_sf$compat_class_cesm == "incompatible_outlier", na.rm = TRUE), " basins")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_line(color = "gray90", linewidth = 0.2),
    legend.position = "bottom"
  )

# Panel B: IPSL compatibility
p4b <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80", linewidth = 0.2) +
  geom_sf(data = compat_sf[!is.na(compat_sf$compat_class_ipsl), ],
          aes(fill = compat_class_ipsl), color = "gray30", linewidth = 0.3) +
  scale_fill_manual(
    values = compat_colors,
    name = "Classification",
    na.value = "gray90"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "B) IPSL Compatibility",
    subtitle = paste0("Compatible: ", sum(compat_sf$compat_class_ipsl == "compatible", na.rm = TRUE), " basins | ",
                     "Incompatible: ", sum(compat_sf$compat_class_ipsl == "incompatible_outlier", na.rm = TRUE), " basins")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_line(color = "gray90", linewidth = 0.2),
    legend.position = "bottom"
  )

# Combine
fig4 <- p4a / p4b +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Save
ggsave(
  filename = file.path(fig_dir, "fig08_compatibility_maps.pdf"),
  plot = fig4,
  width = 12,
  height = 10,
  units = "in"
)

ggsave(
  filename = file.path(fig_dir, "fig08_compatibility_maps.png"),
  plot = fig4,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)

cat("  ✓ Figure 4 saved\n\n")

# ==============================================================================
# FIGURE 5: MODEL AGREEMENT ON INCOMPATIBILITY
# ==============================================================================

cat("Creating Figure 5: Model agreement on incompatibility...\n")

# Create agreement categories
compat$agreement <- ifelse(
  compat$compat_class_cesm == "incompatible_outlier" & compat$compat_class_ipsl == "incompatible_outlier",
  "Both Incompatible",
  ifelse(
    compat$compat_class_cesm == "incompatible_outlier" | compat$compat_class_ipsl == "incompatible_outlier",
    "One Incompatible",
    "Both Compatible/Marginal"
  )
)

# Merge with spatial data
compat_sf <- merge(
  st_as_sf(attrs),
  compat,
  by.x = "ID",
  by.y = "basin_id",
  all.x = TRUE
)

# Agreement colors
agreement_colors <- c(
  "Both Incompatible" = "#d7191c",
  "One Incompatible" = "#fdae61",
  "Both Compatible/Marginal" = "#2c7bb6"
)

p5 <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80", linewidth = 0.2) +
  geom_sf(data = compat_sf[!is.na(compat_sf$agreement), ],
          aes(fill = agreement), color = "gray30", linewidth = 0.3) +
  scale_fill_manual(
    values = agreement_colors,
    name = "Model Agreement"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Model Agreement on Incompatibility",
    subtitle = paste0(
      "Both incompatible: ", sum(compat$agreement == "Both Incompatible", na.rm = TRUE), " | ",
      "One incompatible: ", sum(compat$agreement == "One Incompatible", na.rm = TRUE), " | ",
      "Both compatible: ", sum(compat$agreement == "Both Compatible/Marginal", na.rm = TRUE)
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_line(color = "gray90", linewidth = 0.2),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10)
  )

# Save
ggsave(
  filename = file.path(fig_dir, "fig08_model_agreement.pdf"),
  plot = p5,
  width = 12,
  height = 7,
  units = "in"
)

ggsave(
  filename = file.path(fig_dir, "fig08_model_agreement.png"),
  plot = p5,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

cat("  ✓ Figure 5 saved\n\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("============================================================================\n")
cat("PHASE 08 FIGURES COMPLETE\n")
cat("============================================================================\n\n")

cat("Generated figures:\n")
cat("  1. fig08_compatibility_distributions.{pdf,png}\n")
cat("  2. fig08_mahalanobis_distances.{pdf,png}\n")
cat("  3. fig08_metric_comparison.{pdf,png}\n")
cat("  4. fig08_compatibility_maps.{pdf,png}\n")
cat("  5. fig08_model_agreement.{pdf,png}\n\n")

cat("All figures saved to:", fig_dir, "\n\n")
