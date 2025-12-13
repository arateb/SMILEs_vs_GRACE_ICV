# ============================================================================
# FIGURE 28: Multivariate Compatibility Methods Explanation
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. Why does multivariate test detect incompatibility when univariate tests don't?
#   2. How does Mahalanobis distance account for correlations between metrics?
#   3. What is the relationship between individual metric ranks and multivariate distance?
#
# Panels:
#   (a) Conceptual diagram: 2D example showing univariate vs multivariate compatibility
#   (b) Scatter: Individual metric percentile ranks vs Mahalanobis distance
#   (c) Heatmap: Correlation structure of the 5 metrics across model ensemble
#   (d) Example basin profiles: Compatible vs incompatible cases
#
# Output:
#   outputs/figs/fig28_compatibility_methods.png (400 dpi)
#   outputs/figs/fig28_compatibility_methods.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 28: COMPATIBILITY METHODS EXPLANATION\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load additional required libraries
suppressPackageStartupMessages({
  library(tidyr)
})

# Load Phase 8 compatibility data
cat("Loading Phase 8 compatibility data...\n")
compat <- readRDS("outputs/phase08_compatibility_basin.rds")

cat("  Compatibility data:", nrow(compat), "basins\n\n")

# ============================================================================
# PANEL A: Real 2D Projection (A_LF vs tau)
# ============================================================================

cat("Creating Panel A: Real 2D projection using A_LF vs tau...\n")

# Load model ensemble metrics (per member)
cat("  Loading model persistence (A_LF, tau per member)...\n")
cesm_pers <- readRDS("outputs/phase05_cesm_persistence.rds")
ipsl_pers <- readRDS("outputs/phase05_ipsl_persistence.rds")

# Combine model ensembles
models_all <- bind_rows(
  cesm_pers %>% select(basin_id, member, A_LF, tau),
  ipsl_pers %>% select(basin_id, member, A_LF, tau)
)

# Normalize A_LF and tau for all model members
# (value - mean) / sd
model_stats <- models_all %>%
  summarize(
    A_LF_mean = mean(A_LF, na.rm = TRUE),
    A_LF_sd = sd(A_LF, na.rm = TRUE),
    tau_mean = mean(tau, na.rm = TRUE),
    tau_sd = sd(tau, na.rm = TRUE)
  )

cat("  Model ensemble stats: A_LF mean =", round(model_stats$A_LF_mean, 2),
    "mm, SD =", round(model_stats$A_LF_sd, 2), "mm\n")
cat("                       tau mean =", round(model_stats$tau_mean, 2),
    "months, SD =", round(model_stats$tau_sd, 2), "months\n")

models_norm <- models_all %>%
  mutate(
    A_LF_norm = (A_LF - model_stats$A_LF_mean) / model_stats$A_LF_sd,
    tau_norm = (tau - model_stats$tau_mean) / model_stats$tau_sd
  ) %>%
  select(basin_id, member, A_LF_norm, tau_norm)

# Normalize GRACE using same model stats
# Use C_b (compatibility index) for classification
# C_b near 0.5 = compatible (typical), C_b near 0 or 1 = incompatible (extreme)

grace_norm <- compat %>%
  mutate(
    A_LF_norm = (A_LF_grace - model_stats$A_LF_mean) / model_stats$A_LF_sd,
    tau_norm = (tau_grace - model_stats$tau_mean) / model_stats$tau_sd,
    # Use C_b: compatible if between 0.05 and 0.95 (within middle 90%)
    is_compatible = !is.na(C_b) & (C_b >= 0.05 & C_b <= 0.95),
    compatibility_label = ifelse(is_compatible, "compatible", "incompatible")
  ) %>%
  select(basin_id, compatibility_label, A_LF_norm, tau_norm)

# Calculate 95% confidence ellipse from model ensemble
library(ellipse)
model_cov <- cov(cbind(models_norm$A_LF_norm, models_norm$tau_norm),
                 use = "complete.obs")
model_center <- c(mean(models_norm$A_LF_norm, na.rm = TRUE),
                  mean(models_norm$tau_norm, na.rm = TRUE))
ell_95 <- ellipse(model_cov, centre = model_center, level = 0.95)
ell_df <- data.frame(x = ell_95[,1], y = ell_95[,2])

# Calculate marginal p05-p95 thresholds
p05_A_LF <- quantile(models_norm$A_LF_norm, 0.025, na.rm = TRUE)
p95_A_LF <- quantile(models_norm$A_LF_norm, 0.975, na.rm = TRUE)
p05_tau <- quantile(models_norm$tau_norm, 0.025, na.rm = TRUE)
p95_tau <- quantile(models_norm$tau_norm, 0.975, na.rm = TRUE)

panel_a <- ggplot() +
  # Model ensemble members (sample 5000 points for performance)
  geom_point(data = models_norm %>% sample_n(min(5000, nrow(models_norm))),
             aes(x = A_LF_norm, y = tau_norm),
             size = 0.8, alpha = 0.15, color = "#6BAED6") +
  # 95% confidence ellipse
  geom_path(data = ell_df, aes(x = x, y = y),
            color = "#2171B5", linewidth = 1, linetype = "dashed") +
  # Marginal ranges (dotted lines at p05-p95)
  geom_vline(xintercept = c(p05_A_LF, p95_A_LF),
             linetype = "dotted", color = "grey40", linewidth = 0.6) +
  geom_hline(yintercept = c(p05_tau, p95_tau),
             linetype = "dotted", color = "grey40", linewidth = 0.6) +
  # GRACE basins (color by compatibility)
  geom_point(data = grace_norm,
             aes(x = A_LF_norm, y = tau_norm,
                 color = compatibility_label),
             size = 1.5, alpha = 0.7, shape = 17) +
  scale_color_manual(
    name = "GRACE basins",
    values = c("compatible" = "#2171B5", "incompatible" = "#CB181D"),
    labels = c("compatible" = "Compatible", "incompatible" = "Incompatible")
  ) +
  scale_x_continuous(
    name = "Amplitude (normalized)",
    limits = c(-3, 5)
  ) +
  scale_y_continuous(
    name = "Persistence (normalized)",
    limits = c(-2, 8)
  ) +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1,
    legend.position = c(0.25, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -2.8, y = 7.5, label = "a",
           size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete (REAL DATA)\n")

# ============================================================================
# PANEL B: Number of Extreme Metrics vs Mahalanobis Distance
# ============================================================================

cat("Creating Panel B: Extreme metrics vs Mahalanobis distance...\n")

# Calculate how many metrics are extreme for each basin
# Extreme = ratio outside [0.8, 1.2] range of model median
# Chi-squared threshold for d_mahal
chi_threshold <- qchisq(0.95, df = 5)

compat_extreme <- compat %>%
  mutate(
    extreme_A_LF = (A_LF_grace / A_LF_model_median < 0.8) | (A_LF_grace / A_LF_model_median > 1.2),
    extreme_H_max = (H_max_grace / H_max_model_median < 0.8) | (H_max_grace / H_max_model_median > 1.2),
    extreme_D_max = (abs(D_max_grace) / abs(D_max_model_median) < 0.8) |
                    (abs(D_max_grace) / abs(D_max_model_median) > 1.2),
    extreme_tau = (tau_grace / tau_model_median < 0.8) | (tau_grace / tau_model_median > 1.2),
    extreme_P_LF = (P_LF_grace - P_LF_model_median < -0.1) | (P_LF_grace - P_LF_model_median > 0.1),
    n_extreme = extreme_A_LF + extreme_H_max + extreme_D_max + extreme_tau + extreme_P_LF,
    # Use C_b for compatibility: compatible if C_b in [0.05, 0.95]
    is_compatible = !is.na(C_b) & (C_b >= 0.05 & C_b <= 0.95),
    compatibility_label = ifelse(is_compatible, "Compatible", "Incompatible")
  )

panel_b <- ggplot(compat_extreme, aes(x = n_extreme, y = C_b,
                                      color = is_compatible)) +
  geom_hline(yintercept = 0.95, linetype = "dashed",
             color = "grey30", linewidth = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dotted",
             color = "grey50", linewidth = 0.4) +
  geom_jitter(size = 2, alpha = 0.7, width = 0.15, height = 0.01) +
  scale_color_manual(
    name = "Compatibility",
    values = c("TRUE" = "#2171B5", "FALSE" = "#CB181D"),
    labels = c("TRUE" = "Compatible", "FALSE" = "Incompatible"),
    drop = FALSE  # Show both levels even if one is empty
  ) +
  scale_x_continuous(
    name = "Number of extreme metrics (±20% from model median)",
    breaks = 0:5,
    limits = c(-0.5, 5.5)
  ) +
  scale_y_continuous(
    name = "Compatibility index C_b",
    limits = c(0, 1.02),
    breaks = c(0, 0.25, 0.5, 0.75, 0.95, 1.0)
  ) +
  # Add annotations
  annotate("text", x = 2.5, y = 0.96,
           label = "Threshold (C_b = 0.95)",
           hjust = 0.5, vjust = -0.3, size = 2.5, color = "grey30") +
  annotate("text", x = 0.5, y = 0.5,
           label = "Typical",
           hjust = 0, vjust = -0.5, size = 2.5, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = c(0.25, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -0.3, y = 1.02,
           label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: GRACE vs Model Ratio Distributions
# ============================================================================

cat("Creating Panel C: GRACE vs model ratio distributions...\n")

# Calculate ratios for each metric (GRACE/Model)
# Ratio > 1: GRACE larger than models (models underestimate)
# Ratio < 1: GRACE smaller than models (models overestimate)
ratio_data <- compat %>%
  mutate(
    ratio_A_LF = A_LF_grace / A_LF_model_median,
    ratio_H_max = H_max_grace / H_max_model_median,
    ratio_D_max = abs(D_max_grace) / abs(D_max_model_median),
    ratio_tau = tau_grace / tau_model_median
  ) %>%
  select(basin_id, starts_with("ratio_")) %>%
  pivot_longer(cols = starts_with("ratio_"), names_to = "metric", values_to = "ratio") %>%
  mutate(
    metric = case_when(
      metric == "ratio_A_LF" ~ "A_LF",
      metric == "ratio_H_max" ~ "H_max",
      metric == "ratio_D_max" ~ "D_max",
      metric == "ratio_tau" ~ "tau"
    )
  )

# Factor ordering
ratio_data$metric <- factor(ratio_data$metric,
                            levels = c("A_LF", "H_max", "D_max", "tau"))

# Calculate median values for annotation
ratio_medians <- ratio_data %>%
  group_by(metric) %>%
  summarize(median_ratio = median(ratio, na.rm = TRUE), .groups = "drop")

panel_c <- ggplot(ratio_data, aes(x = metric, y = ratio, fill = metric)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.7) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.alpha = 0.5, coef = 1.5) +
  # Add median value labels
  geom_text(data = ratio_medians,
            aes(x = metric, y = median_ratio, label = sprintf("%.2f", median_ratio)),
            size = 2.5, fontface = "bold", color = "white", vjust = 0.5) +
  scale_fill_manual(
    values = c("A_LF" = "#2171B5", "H_max" = "#31A354",
               "D_max" = "#CB181D", "tau" = "#6A51A3")
  ) +
  scale_x_discrete(
    name = "",
    labels = c("A_LF" = "Amplitude", "H_max" = "Pluvial height",
               "D_max" = "Drought depth", "tau" = "Persistence")
  ) +
  scale_y_log10(
    name = "GRACE / Model median ratio",
    breaks = c(0.5, 1, 2, 4),
    labels = c("0.5", "1", "2", "4")
  ) +
  annotation_logticks(sides = "l", size = 0.25, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  annotate("text", x = 0.6, y = max(ratio_data$ratio, na.rm = TRUE) * 0.9,
           label = "c", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Example Basin Profiles (Compatible vs Incompatible)
# ============================================================================

cat("Creating Panel D: Example basin profiles...\n")

# Find example basins
example_compatible <- compat_extreme %>%
  filter(is_compatible) %>%
  arrange(d_mahal) %>%
  slice(1) %>%
  pull(basin_id)

# If no compatible basins, use least incompatible
if (length(example_compatible) == 0) {
  example_compatible <- compat_extreme %>%
    arrange(d_mahal) %>%
    slice(1) %>%
    pull(basin_id)
}

example_incompatible <- compat_extreme %>%
  filter(!is_compatible) %>%
  arrange(desc(d_mahal)) %>%
  slice(1) %>%
  pull(basin_id)

# Create normalized metric profiles
profile_data <- compat %>%
  filter(basin_id %in% c(example_compatible, example_incompatible)) %>%
  select(basin_id, basin_name, compatibility_class,
         A_LF_grace, H_max_grace, D_max_grace, tau_grace, P_LF_grace,
         A_LF_model_median, H_max_model_median, D_max_model_median,
         tau_model_median, P_LF_model_median) %>%
  pivot_longer(cols = contains("_grace") | contains("_model"),
               names_to = "variable", values_to = "value") %>%
  mutate(
    metric = case_when(
      grepl("A_LF", variable) ~ "A_LF",
      grepl("H_max", variable) ~ "H_max",
      grepl("D_max", variable) ~ "D_max",
      grepl("tau", variable) ~ "tau",
      grepl("P_LF", variable) ~ "P_LF"
    ),
    source = ifelse(grepl("grace", variable), "GRACE", "Model median")
  ) %>%
  # Normalize by model median for each metric
  group_by(metric) %>%
  mutate(value_norm = value / mean(value[source == "Model median"], na.rm = TRUE)) %>%
  ungroup()

# Factor ordering
profile_data$metric <- factor(profile_data$metric,
                              levels = c("A_LF", "H_max", "D_max", "tau", "P_LF"))

panel_d <- ggplot(profile_data, aes(x = metric, y = value_norm,
                                    group = interaction(basin_id, source),
                                    color = source,
                                    linetype = compatibility_class)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(
    name = "Data source",
    values = c("GRACE" = "#CB181D", "Model median" = "#2171B5")
  ) +
  scale_linetype_manual(
    name = "Class",
    values = c("compatible" = "solid", "incompatible_outlier" = "dashed"),
    labels = c("compatible" = "Compatible", "incompatible_outlier" = "Incompatible")
  ) +
  scale_x_discrete(
    name = "",
    labels = c("A_LF" = "Amplitude", "H_max" = "Pluvial height",
               "D_max" = "Drought depth", "tau" = "Persistence", "P_LF" = "Power fraction")
  ) +
  scale_y_continuous(
    name = "Normalized value (model median = 1)",
    breaks = seq(0, 3, 0.5)
  ) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.key.width = unit(8, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = 0.6, y = max(profile_data$value_norm, na.rm = TRUE) * 0.98,
           label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig28 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig28, "fig28_compatibility_methods", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 28 COMPATIBILITY METHODS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig28_compatibility_methods.{png,pdf}\n")
cat("============================================================================\n\n")

# ============================================================================
# Print Key Statistics
# ============================================================================

cat("Key statistics for methods explanation:\n\n")

cat("GRACE/Model ratio summary (ratio > 1 = models underestimate, < 1 = models overestimate):\n")
cat("  A_LF:  median = ", round(median(ratio_data %>% filter(metric == "A_LF") %>% pull(ratio), na.rm = TRUE), 2), "\n", sep = "")
cat("  H_max: median = ", round(median(ratio_data %>% filter(metric == "H_max") %>% pull(ratio), na.rm = TRUE), 2), "\n", sep = "")
cat("  D_max: median = ", round(median(ratio_data %>% filter(metric == "D_max") %>% pull(ratio), na.rm = TRUE), 2), "\n", sep = "")
cat("  tau:   median = ", round(median(ratio_data %>% filter(metric == "tau") %>% pull(ratio), na.rm = TRUE), 2), "\n\n", sep = "")

if (length(example_compatible) > 0) {
  cat("Example basins:\n")
  cat("  Compatible:   ",
      compat_extreme %>% filter(basin_id == example_compatible) %>% pull(basin_name),
      " (d_mahal = ",
      round(compat_extreme %>% filter(basin_id == example_compatible) %>% pull(d_mahal), 2),
      ")\n", sep = "")
  cat("  Incompatible: ",
      compat_extreme %>% filter(basin_id == example_incompatible) %>% pull(basin_name),
      " (d_mahal = ",
      round(compat_extreme %>% filter(basin_id == example_incompatible) %>% pull(d_mahal), 2),
      ")\n\n", sep = "")
} else {
  cat("Example basins:\n")
  cat("  No compatible basins found (all 184 basins are incompatible)\n")
  cat("  Most incompatible: ",
      compat_extreme %>% filter(basin_id == example_incompatible) %>% pull(basin_name),
      " (d_mahal = ",
      round(compat_extreme %>% filter(basin_id == example_incompatible) %>% pull(d_mahal), 2),
      ")\n\n", sep = "")
}

cat("Relationship between number of extreme metrics and compatibility:\n")

n_extreme_compatible <- compat_extreme %>% filter(is_compatible) %>% pull(n_extreme)
n_extreme_incompatible <- compat_extreme %>% filter(!is_compatible) %>% pull(n_extreme)

if (length(n_extreme_compatible) > 0) {
  cat("  Compatible basins:   n_extreme = ",
      round(mean(n_extreme_compatible, na.rm = TRUE), 2),
      " ± ",
      round(sd(n_extreme_compatible, na.rm = TRUE), 2),
      " (n = ", length(n_extreme_compatible), " basins)\n", sep = "")
} else {
  cat("  Compatible basins:   No compatible basins\n")
}

cat("  Incompatible basins: n_extreme = ",
    round(mean(n_extreme_incompatible, na.rm = TRUE), 2),
    " ± ",
    round(sd(n_extreme_incompatible, na.rm = TRUE), 2),
    " (n = ", length(n_extreme_incompatible), " basins)\n\n", sep = "")
