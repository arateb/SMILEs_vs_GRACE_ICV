# ============================================================================
# FIGURE 3 EXTENDED: Period Analysis - All Modes and Climate Classes
# ============================================================================
#
# Additional analyses beyond basic period bands:
#   1. ALL period distributions (not just dominant) for models only
#   2. Period vs Power scatter for all model oscillations
#   3. Period distributions by climate class (Arid, Semi-Arid, Humid, Semi-Humid)
#
# Panels:
#   (e) Histogram: ALL periods from models (period_1, period_2, period_3)
#   (f) Scatter: Period vs Power for all model oscillations
#   (g) Faceted histogram: Period distributions by climate class
#
# Output:
#   outputs/figs/fig3_period_extended.png (400 dpi)
#   outputs/figs/fig3_period_extended.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3 EXTENDED: ALL PERIOD MODES AND CLIMATE CLASSES\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load tidyr for complete()
suppressPackageStartupMessages({
  library(tidyr)
})

# Load data
cat("Loading wavelet data...\n")
cesm_w <- readRDS("outputs/phase04_cesm_all_wavelets.rds")
ipsl_w <- readRDS("outputs/phase04_ipsl_all_wavelets.rds")

# Load basin attributes for climate classification
attrs <- readRDS("outputs/basin_attributes.rds")
attrs_lookup <- data.frame(
  basin = 1:nrow(attrs),
  climate = attrs$climate
)

cat("  CESM wavelets:", nrow(cesm_w), "rows\n")
cat("  IPSL wavelets:", nrow(ipsl_w), "rows\n")
cat("  Climate classes:", paste(names(table(attrs$climate)), collapse=", "), "\n\n")

# ============================================================================
# PANEL E: ALL Periods Distribution (Models Only)
# ============================================================================

cat("Creating Panel E: ALL periods distribution (models only)...\n")

# Gather ALL periods (period_1, period_2, period_3) from CESM2
cesm_all_periods <- bind_rows(
  cesm_w %>%
    filter(!is.na(period_1)) %>%
    select(basin, member, period = period_1) %>%
    mutate(model = "CESM2"),
  cesm_w %>%
    filter(!is.na(period_2)) %>%
    select(basin, member, period = period_2) %>%
    mutate(model = "CESM2"),
  cesm_w %>%
    filter(!is.na(period_3)) %>%
    select(basin, member, period = period_3) %>%
    mutate(model = "CESM2")
)

# Gather ALL periods from IPSL
ipsl_all_periods <- bind_rows(
  ipsl_w %>%
    filter(!is.na(period_1)) %>%
    select(basin, member, period = period_1) %>%
    mutate(model = "IPSL"),
  ipsl_w %>%
    filter(!is.na(period_2)) %>%
    select(basin, member, period = period_2) %>%
    mutate(model = "IPSL"),
  ipsl_w %>%
    filter(!is.na(period_3)) %>%
    select(basin, member, period = period_3) %>%
    mutate(model = "IPSL")
)

# Combine
all_periods <- bind_rows(cesm_all_periods, ipsl_all_periods)

cat("  Total periods:\n")
cat("    CESM2:", nrow(cesm_all_periods), "\n")
cat("    IPSL:", nrow(ipsl_all_periods), "\n")
cat("    Combined:", nrow(all_periods), "\n")

panel_e <- ggplot(all_periods, aes(x = period, fill = model)) +
  geom_density(alpha = 0.5, linewidth = 0.6) +
  scale_fill_manual(
    name = "Model",
    values = c("CESM2" = "#0072B2", "IPSL" = "#FF6B6B")
  ) +
  scale_x_continuous(
    name = "Period (years) - ALL modes",
    breaks = c(2, 4, 6, 8, 10, 15, 20, 30),
    limits = c(1.5, 35)
  ) +
  scale_y_continuous(name = "Density") +
  # Add vertical lines for band boundaries
  geom_vline(xintercept = c(4, 8, 30), linetype = "dashed", color = "grey50", linewidth = 0.3) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 2, y = Inf, label = "e",
           size = 5, fontface = "bold", hjust = 0, vjust = 1.5)

cat("  ✓ Panel E complete\n")

# ============================================================================
# PANEL F: Period vs Power Scatter (All Model Oscillations)
# ============================================================================

cat("Creating Panel F: Period vs Power scatter (all oscillations)...\n")

# Gather ALL period-power pairs from CESM2
cesm_period_power <- bind_rows(
  cesm_w %>%
    filter(!is.na(period_1) & !is.na(power_1)) %>%
    select(basin, member, period = period_1, power = power_1, band = band_1),
  cesm_w %>%
    filter(!is.na(period_2) & !is.na(power_2)) %>%
    select(basin, member, period = period_2, power = power_2, band = band_2),
  cesm_w %>%
    filter(!is.na(period_3) & !is.na(power_3)) %>%
    select(basin, member, period = period_3, power = power_3, band = band_3)
) %>%
  mutate(model = "CESM2")

# Gather ALL period-power pairs from IPSL
ipsl_period_power <- bind_rows(
  ipsl_w %>%
    filter(!is.na(period_1) & !is.na(power_1)) %>%
    select(basin, member, period = period_1, power = power_1, band = band_1),
  ipsl_w %>%
    filter(!is.na(period_2) & !is.na(power_2)) %>%
    select(basin, member, period = period_2, power = power_2, band = band_2),
  ipsl_w %>%
    filter(!is.na(period_3) & !is.na(power_3)) %>%
    select(basin, member, period = period_3, power = power_3, band = band_3)
) %>%
  mutate(model = "IPSL")

# Combine model data
all_period_power <- bind_rows(cesm_period_power, ipsl_period_power)

# Add GRACE power data for comparison - ALL modes (period_1, period_2, period_3)
grace_w_full <- readRDS("outputs/phase04_grace_wavelets.rds")

grace_w_power <- bind_rows(
  grace_w_full %>%
    filter(!is.na(dominant_period_1) & !is.na(dominant_power_1)) %>%
    select(basin, period = dominant_period_1, power = dominant_power_1, band = band_class_1) %>%
    mutate(model = "GRACE", member = NA_integer_),
  grace_w_full %>%
    filter(!is.na(dominant_period_2) & !is.na(dominant_power_2)) %>%
    select(basin, period = dominant_period_2, power = dominant_power_2, band = band_class_2) %>%
    mutate(model = "GRACE", member = NA_integer_),
  grace_w_full %>%
    filter(!is.na(dominant_period_3) & !is.na(dominant_power_3)) %>%
    select(basin, period = dominant_period_3, power = dominant_power_3, band = band_class_3) %>%
    mutate(model = "GRACE", member = NA_integer_)
)

cat("  GRACE periods: n=", nrow(grace_w_power), "\n", sep="")

# Combine with model data
all_period_power_with_grace <- bind_rows(all_period_power, grace_w_power)

# Sample ONLY models for visualization, keep ALL GRACE points
set.seed(42)
model_data_sample <- all_period_power %>%
  sample_n(min(20000, nrow(all_period_power)))

cat("  Sampled", nrow(model_data_sample), "model points from", nrow(all_period_power), "total\n")
cat("  Keeping ALL", nrow(grace_w_power), "GRACE points\n")

panel_f <- ggplot() +
  # Plot models first (background)
  geom_point(data = model_data_sample,
             aes(x = period, y = power, color = model),
             alpha = 0.2, size = 0.6) +
  # Plot GRACE on top (foreground) - larger and more visible
  geom_point(data = grace_w_power,
             aes(x = period, y = power, color = model),
             alpha = 0.8, size = 1.5) +
  scale_color_manual(
    name = "Dataset",
    values = c("GRACE" = "#E69F00", "CESM2" = "#0072B2", "IPSL" = "#FF6B6B")
  ) +
  scale_x_continuous(
    name = "Period (years)",
    breaks = c(2, 4, 8, 15, 30),
    limits = c(1.5, 35)
  ) +
  scale_y_continuous(
    name = "Power (mm²)",
    breaks = c(0, 5, 10, 15, 20),
    limits = c(0, 20)
  ) +
  # Add vertical lines for band boundaries
  geom_vline(xintercept = c(4, 8, 30), linetype = "dashed", color = "grey50", linewidth = 0.3) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 2, y = 19, label = "f",
           size = 5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel F complete\n")

# ============================================================================
# PANEL G: Overall Period Band Distribution (GRACE, CESM2, IPSL)
# ============================================================================

cat("Creating Panel G: Overall period band distribution...\n")

# Load GRACE wavelets
grace_w <- readRDS("outputs/phase04_grace_wavelets.rds")

# GRACE period bands
grace_bands <- grace_w %>%
  filter(!is.na(dominant_period_1)) %>%
  mutate(
    band_class = case_when(
      dominant_period_1 >= 2 & dominant_period_1 < 4 ~ "ENSO_core",
      dominant_period_1 >= 4 & dominant_period_1 < 8 ~ "Quasi-decadal",
      TRUE ~ NA_character_
    ),
    source = "GRACE (FO)"
  ) %>%
  filter(!is.na(band_class)) %>%
  count(source, band_class)

# CESM2 period bands
cesm_bands <- cesm_period_power %>%
  mutate(
    band_class = case_when(
      period >= 2 & period < 4 ~ "ENSO_core",
      period >= 4 & period < 8 ~ "Quasi-decadal",
      period >= 8 & period < 30 ~ "Decadal",
      period >= 30 ~ "Multidecadal",
      TRUE ~ NA_character_
    ),
    source = "CESM2"
  ) %>%
  filter(!is.na(band_class)) %>%
  count(source, band_class)

# IPSL period bands
ipsl_bands <- ipsl_period_power %>%
  mutate(
    band_class = case_when(
      period >= 2 & period < 4 ~ "ENSO_core",
      period >= 4 & period < 8 ~ "Quasi-decadal",
      period >= 8 & period < 30 ~ "Decadal",
      period >= 30 ~ "Multidecadal",
      TRUE ~ NA_character_
    ),
    source = "IPSL"
  ) %>%
  filter(!is.na(band_class)) %>%
  count(source, band_class)

# Combine all sources
all_bands <- bind_rows(grace_bands, cesm_bands, ipsl_bands) %>%
  # Ensure all combinations exist
  complete(source, band_class, fill = list(n = 0)) %>%
  group_by(source) %>%
  mutate(
    proportion = n / sum(n) * 100
  ) %>%
  ungroup()

# Set factor levels
all_bands$source <- factor(all_bands$source, levels = c("GRACE (FO)", "CESM2", "IPSL"))
all_bands$band_class <- factor(all_bands$band_class,
                               levels = c("ENSO_core", "Quasi-decadal", "Decadal", "Multidecadal"))

cat("  Band proportions:\n")
print(all_bands)

panel_g <- ggplot(all_bands, aes(x = source, y = proportion, fill = band_class)) +
  geom_col(position = "stack", color = "black", linewidth = 0.3) +
  scale_fill_manual(
    name = "Period class",
    values = c(
      "ENSO_core" = "#196F3D",
      "Quasi-decadal" = "#45B39D",
      "Decadal" = "#85C1E2",
      "Multidecadal" = "#5D6D7E"
    ),
    labels = c(
      "ENSO_core" = "ENSO (2-4yr)",
      "Quasi-decadal" = "Quasi-decadal (4-8yr)",
      "Decadal" = "Decadal (8-30yr)",
      "Multidecadal" = "Multidecadal (>30yr)"
    )
  ) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Proportion (%)", limits = c(0, 100), expand = c(0, 0)) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    axis.text.x = element_text(size = 7, angle = 0)
  ) +
  annotate("text", x = 0.6, y = 97, label = "g",
           size = 5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel G complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: 3 panels stacked vertically
fig3_extended <- panel_e / panel_f / panel_g +
  plot_layout(heights = c(1, 1, 1.2))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig3_extended, "fig3_period_extended", width_mm = 180, height_mm = 200)

# ============================================================================
# EXPORT DATA TO CSV
# ============================================================================

cat("Exporting figure data to CSV...\n")

dir.create("outputs/figure_data", showWarnings = FALSE, recursive = TRUE)

# Panel E data (all period distributions)
write.csv(all_periods, "outputs/figure_data/fig3e_all_periods_models.csv", row.names = FALSE)

# Panel F data (period vs power with GRACE)
write.csv(all_period_power_with_grace, "outputs/figure_data/fig3f_period_power_all.csv", row.names = FALSE)

# Panel G data (overall period band distribution)
write.csv(all_bands, "outputs/figure_data/fig3g_period_bands_overall.csv", row.names = FALSE)

cat("  ✓ Exported: outputs/figure_data/fig3{e,f,g}_*.csv\n\n")

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3 EXTENDED COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig3_period_extended.{png,pdf}\n")
cat("  Data: outputs/figure_data/fig3{e,f,g}_*.csv\n")
cat("============================================================================\n\n")
