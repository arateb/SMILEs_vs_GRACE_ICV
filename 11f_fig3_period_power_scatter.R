# ============================================================================
# FIGURE 3 EXTENDED: Period vs Power Scatter Plots
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. How do period-power relationships compare between GRACE and models?
#   2. What is the distribution of oscillation periods vs their power?
#   3. Are model period-power relationships consistent with observations?
#
# Panels:
#   (a) GRACE: Period vs Power (all basins)
#   (b) CESM2: Period vs Power (all members, all basins)
#   (c) IPSL: Period vs Power (all members, all basins)
#
# Output:
#   outputs/figs/fig3_period_power_scatter.png (400 dpi)
#   outputs/figs/fig3_period_power_scatter.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3 EXTENDED: PERIOD VS POWER SCATTER PLOTS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading wavelet data...\n")
grace_w <- readRDS("outputs/phase04_grace_wavelets.rds")
cesm_w <- readRDS("outputs/phase04_cesm_all_wavelets.rds")
ipsl_w <- readRDS("outputs/phase04_ipsl_all_wavelets.rds")

cat("  GRACE wavelets:", nrow(grace_w), "rows\n")
cat("  CESM2 wavelets:", nrow(cesm_w), "rows\n")
cat("  IPSL wavelets:", nrow(ipsl_w), "rows\n\n")

# ============================================================================
# COMBINE ALL DATA WITH PERIOD BAND CLASSIFICATION
# ============================================================================

cat("Creating combined period vs power scatter with period band classification...\n")

# Extract GRACE period-power pairs
grace_data <- grace_w %>%
  filter(!is.na(dominant_period_1) & !is.na(dominant_power_1)) %>%
  select(basin, period = dominant_period_1, power = dominant_power_1) %>%
  mutate(source = "GRACE (FO)")

# Extract CESM2 period-power pairs
cesm_data <- cesm_w %>%
  filter(!is.na(period_1) & !is.na(power_1)) %>%
  select(basin, member, period = period_1, power = power_1) %>%
  mutate(source = "CESM2")

# Extract IPSL period-power pairs
ipsl_data <- ipsl_w %>%
  filter(!is.na(period_1) & !is.na(power_1)) %>%
  select(basin, member, period = period_1, power = power_1) %>%
  mutate(source = "IPSL")

# Combine all data
all_data <- bind_rows(grace_data, cesm_data, ipsl_data) %>%
  mutate(
    period_band = case_when(
      period >= 2 & period < 4 ~ "ENSO core",
      period >= 4 & period < 8 ~ "Quasi-decadal",
      period >= 8 & period < 30 ~ "Decadal",
      period >= 30 ~ "Multidecadal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period_band))

# Set factor levels for period bands
all_data$period_band <- factor(all_data$period_band,
                               levels = c("ENSO core", "Quasi-decadal", "Decadal", "Multidecadal"))

# Set factor levels for source
all_data$source <- factor(all_data$source,
                          levels = c("GRACE (FO)", "CESM2", "IPSL"))

cat("  Total data points: n=", format(nrow(all_data), big.mark = ","), "\n")
cat("  GRACE:  n=", format(sum(all_data$source == "GRACE (FO)"), big.mark = ","), "\n")
cat("  CESM2:  n=", format(sum(all_data$source == "CESM2"), big.mark = ","), "\n")
cat("  IPSL:   n=", format(sum(all_data$source == "IPSL"), big.mark = ","), "\n")
cat("  Period band counts:\n")
print(table(all_data$period_band))

# Sample model data for visualization (too many points otherwise)
set.seed(42)
model_sample <- all_data %>%
  filter(source != "GRACE (FO)") %>%
  sample_n(min(10000, nrow(.)))

grace_data_plot <- all_data %>%
  filter(source == "GRACE (FO)")

# Combine sampled models with all GRACE
plot_data <- bind_rows(model_sample, grace_data_plot)

# Calculate local density for each model point
model_data_only <- plot_data %>% filter(source != "GRACE (FO)")

# Use MASS::kde2d for density calculation
library(MASS)
kde_result <- kde2d(
  model_data_only$period,
  model_data_only$power,
  n = 100,
  lims = c(range(model_data_only$period, na.rm = TRUE),
           range(model_data_only$power, na.rm = TRUE))
)

# Interpolate density values at each point location using approx
# Create interpolation function manually
interp_density <- function(x_point, y_point, kde_x, kde_y, kde_z) {
  # Find nearest grid points
  x_idx <- which.min(abs(kde_x - x_point))
  y_idx <- which.min(abs(kde_y - y_point))
  return(kde_z[x_idx, y_idx])
}

# Add density to model data
model_data_only$density <- mapply(
  interp_density,
  model_data_only$period,
  model_data_only$power,
  MoreArgs = list(
    kde_x = kde_result$x,
    kde_y = kde_result$y,
    kde_z = kde_result$z
  )
)

# Add shape based on model source (CESM2 vs IPSL)
model_data_only <- model_data_only %>%
  mutate(
    model_shape = case_when(
      source == "CESM2" ~ 21,  # circle for CESM2
      source == "IPSL" ~ 22,   # square for IPSL
      TRUE ~ 21
    )
  )

# Create scatter plot with points colored by density - SEXY VERSION
combined_plot <- ggplot(plot_data, aes(x = period, y = power)) +
  # Add vertical lines for period band boundaries
  geom_vline(xintercept = c(4, 8, 30), linetype = "dashed", color = "grey60", linewidth = 0.5, alpha = 0.6) +
  # Model points - different shapes by model (CESM2=circle, IPSL=square), colored by density
  geom_point(data = model_data_only,
             aes(x = period, y = power, fill = density, shape = factor(model_shape)),
             alpha = 0.85, size = 2.2, color = "grey20", stroke = 0.2) +
  scale_shape_manual(values = c("21" = 21, "22" = 22), guide = "none") +
  # GRACE points - thin hollow triangles in darker gray
  geom_point(data = plot_data %>% filter(source == "GRACE (FO)"),
             aes(x = period, y = power),
             size = 2.5, shape = 2, stroke = 0.5, alpha = 0.7, color = "grey30") +
  scale_fill_gradientn(
    name = "Model\nDensity",
    colors = c("#FFFF00", "#FFD700", "#FFA500", "#FF6600", "#FF4500", "#FF0000", "#DC143C", "#8B0000"),
    values = scales::rescale(c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, 1)),
    na.value = "grey40",
    guide = guide_colorbar(
      barwidth = 0.8,
      barheight = 10,
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  scale_x_continuous(
    name = "Period (years)",
    breaks = c(2, 4, 8, 15, 30, 50),
    limits = c(1.5, 60)
  ) +
  scale_y_continuous(
    name = "Power (mm²)",
    limits = c(0, max(plot_data$power, na.rm = TRUE) * 1.05)
  ) +
  theme_nature(base_size = 9) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.background = element_rect(fill = alpha("white", 0.95), color = "black", linewidth = 0.4),
    legend.title = element_text(size = 8, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 7),
    legend.margin = margin(4, 4, 4, 4),
    axis.title = element_text(size = 9, face = "bold"),
    axis.text = element_text(size = 8)
  ) +
  # Add period band labels at top with subtle background boxes
  annotate("rect", xmin = 2, xmax = 4, ymin = max(plot_data$power, na.rm = TRUE) * 0.95,
           ymax = max(plot_data$power, na.rm = TRUE) * 1.02, fill = alpha("white", 0.7), color = NA) +
  annotate("text", x = 3, y = max(plot_data$power, na.rm = TRUE) * 0.985,
           label = "ENSO\ncore", size = 2.8, hjust = 0.5, vjust = 0.5, color = "grey20", fontface = "bold") +
  annotate("rect", xmin = 4.5, xmax = 7.5, ymin = max(plot_data$power, na.rm = TRUE) * 0.95,
           ymax = max(plot_data$power, na.rm = TRUE) * 1.02, fill = alpha("white", 0.7), color = NA) +
  annotate("text", x = 6, y = max(plot_data$power, na.rm = TRUE) * 0.985,
           label = "Quasi-\ndecadal", size = 2.8, hjust = 0.5, vjust = 0.5, color = "grey20", fontface = "bold") +
  annotate("rect", xmin = 11, xmax = 19, ymin = max(plot_data$power, na.rm = TRUE) * 0.95,
           ymax = max(plot_data$power, na.rm = TRUE) * 1.02, fill = alpha("white", 0.7), color = NA) +
  annotate("text", x = 15, y = max(plot_data$power, na.rm = TRUE) * 0.985,
           label = "Decadal", size = 2.8, hjust = 0.5, vjust = 0.5, color = "grey20", fontface = "bold") +
  annotate("rect", xmin = 35, xmax = 45, ymin = max(plot_data$power, na.rm = TRUE) * 0.95,
           ymax = max(plot_data$power, na.rm = TRUE) * 1.02, fill = alpha("white", 0.7), color = NA) +
  annotate("text", x = 40, y = max(plot_data$power, na.rm = TRUE) * 0.985,
           label = "Multi-\ndecadal", size = 2.8, hjust = 0.5, vjust = 0.5, color = "grey20", fontface = "bold")

cat("  ✓ Combined plot complete (density-based hot colors)\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

cat("Saving figure...\n")

save_figure(combined_plot, "fig3_period_power_scatter", width_mm = 180, height_mm = 120)

# ============================================================================
# EXPORT DATA TO CSV
# ============================================================================

cat("Exporting figure data to CSV...\n")

dir.create("outputs/figure_data", showWarnings = FALSE, recursive = TRUE)

write.csv(all_data, "outputs/figure_data/fig3_period_power_combined.csv", row.names = FALSE)

cat("  ✓ Exported: outputs/figure_data/fig3_period_power_combined.csv\n\n")

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3 PERIOD-POWER SCATTER PLOT COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig3_period_power_scatter.{png,pdf}\n")
cat("  Data: outputs/figure_data/fig3_period_power_combined.csv\n")
cat("  Period bands: ENSO core (2-4yr), Quasi-decadal (4-8yr), Decadal (8-30yr), Multidecadal (>30yr)\n")
cat("  Viridis color scale applied to period bands\n")
cat("============================================================================\n\n")
