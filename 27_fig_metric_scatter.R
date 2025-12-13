# ============================================================================
# FIGURE 27: Individual Metric Comparisons (GRACE vs Model Medians)
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. How do individual metrics compare between GRACE and models?
#   2. Which metrics show strongest systematic biases?
#   3. Are model biases consistent across all metrics?
#
# Panels:
#   (a) Scatter: Low-frequency amplitude (A_LF)
#   (b) Scatter: Pluvial height (H_max)
#   (c) Scatter: Drought depth (|D_max|)
#   (d) Scatter: Persistence timescale (tau)
#
# Output:
#   outputs/figs/fig27_metric_scatter.png (400 dpi)
#   outputs/figs/fig27_metric_scatter.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 27: INDIVIDUAL METRIC COMPARISONS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load Phase 8 compatibility data
cat("Loading Phase 8 compatibility data...\n")
compat <- readRDS("outputs/phase08_compatibility_basin.rds")

cat("  Compatibility data:", nrow(compat), "basins\n\n")

# ============================================================================
# PANEL A: Low-Frequency Amplitude (A_LF)
# ============================================================================

cat("Creating Panel A: Low-frequency amplitude scatter...\n")

# Calculate 1:1 line range
max_A_LF <- max(c(compat$A_LF_grace, compat$A_LF_model_median), na.rm = TRUE)

panel_a <- ggplot(compat, aes(x = A_LF_model_median, y = A_LF_grace)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  # Points
  geom_point(size = 2, alpha = 0.7, color = "#2171B5") +
  scale_x_continuous(
    name = "Model median A_LF (mm)",
    limits = c(0, max_A_LF * 1.05)
  ) +
  scale_y_continuous(
    name = "GRACE A_LF (mm)",
    limits = c(0, max_A_LF * 1.05)
  ) +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1
  ) +
  annotate("text", x = max_A_LF * 0.05, y = max_A_LF * 0.95,
           label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Pluvial Height (H_max)
# ============================================================================

cat("Creating Panel B: Pluvial height scatter...\n")

# Calculate 1:1 line range
max_H <- max(c(compat$H_max_grace, compat$H_max_model_median), na.rm = TRUE)

panel_b <- ggplot(compat, aes(x = H_max_model_median, y = H_max_grace)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  # Points
  geom_point(size = 2, alpha = 0.7, color = "#31A354") +
  scale_x_continuous(
    name = "Model median H_max (mm)",
    limits = c(0, max_H * 1.05)
  ) +
  scale_y_continuous(
    name = "GRACE H_max (mm)",
    limits = c(0, max_H * 1.05)
  ) +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1
  ) +
  annotate("text", x = max_H * 0.05, y = max_H * 0.95,
           label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Drought Depth (|D_max|)
# ============================================================================

cat("Creating Panel C: Drought depth scatter...\n")

# Use absolute values
compat_abs <- compat %>%
  mutate(
    D_max_grace_abs = abs(D_max_grace),
    D_max_model_median_abs = abs(D_max_model_median)
  )

# Calculate 1:1 line range
max_D <- max(c(compat_abs$D_max_grace_abs, compat_abs$D_max_model_median_abs), na.rm = TRUE)

panel_c <- ggplot(compat_abs, aes(x = D_max_model_median_abs, y = D_max_grace_abs)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  # Points
  geom_point(size = 2, alpha = 0.7, color = "#CB181D") +
  scale_x_continuous(
    name = "Model median |D_max| (mm)",
    limits = c(0, max_D * 1.05)
  ) +
  scale_y_continuous(
    name = "GRACE |D_max| (mm)",
    limits = c(0, max_D * 1.05)
  ) +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1
  ) +
  annotate("text", x = max_D * 0.05, y = max_D * 0.95,
           label = "c", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Persistence Timescale (tau)
# ============================================================================

cat("Creating Panel D: Persistence timescale scatter...\n")

# Calculate 1:1 line range
max_tau <- max(c(compat$tau_grace, compat$tau_model_median), na.rm = TRUE)

panel_d <- ggplot(compat, aes(x = tau_model_median, y = tau_grace)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  # Points
  geom_point(size = 2, alpha = 0.7, color = "#6A51A3") +
  scale_x_continuous(
    name = "Model median τ (months)",
    limits = c(0, max_tau * 1.05)
  ) +
  scale_y_continuous(
    name = "GRACE τ (months)",
    limits = c(0, max_tau * 1.05)
  ) +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1
  ) +
  annotate("text", x = max_tau * 0.05, y = max_tau * 0.95,
           label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig27 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig27, "fig27_metric_scatter", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 27 METRIC SCATTER PLOTS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig27_metric_scatter.{png,pdf}\n")
cat("============================================================================\n\n")

# ============================================================================
# Print Metric Correlations
# ============================================================================

cat("Metric correlations (GRACE vs Model Median):\n")
cat("  A_LF:  r =", round(cor(compat$A_LF_grace, compat$A_LF_model_median, use = "complete.obs"), 3), "\n")
cat("  H_max: r =", round(cor(compat$H_max_grace, compat$H_max_model_median, use = "complete.obs"), 3), "\n")
cat("  D_max: r =", round(cor(abs(compat$D_max_grace), abs(compat$D_max_model_median), use = "complete.obs"), 3), "\n")
cat("  tau:   r =", round(cor(compat$tau_grace, compat$tau_model_median, use = "complete.obs"), 3), "\n\n")
