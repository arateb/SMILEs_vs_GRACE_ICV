# ============================================================================
# FIGURE 18: Persistence-Period Relationships
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. Is persistence timescale related to dominant period?
#   2. Do basins with longer periods show longer persistence?
#   3. Is there a relationship between low-frequency power and persistence?
#   4. How do these relationships differ between models and observations?
#
# Panels:
#   (a) Scatter: Persistence vs dominant period (GRACE)
#   (b) Scatter: Persistence vs dominant period (CESM2 median)
#   (c) Scatter: Low-freq power vs persistence (GRACE vs CESM2)
#   (d) Scatter: Low-freq power vs persistence (GRACE vs IPSL)
#
# Output:
#   outputs/figs/fig18_persistence_period.png (400 dpi)
#   outputs/figs/fig18_persistence_period.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 18: PERSISTENCE-PERIOD RELATIONSHIPS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading data...\n")
persistence <- readRDS("outputs/phase05_persistence_summary.rds")
wavelet <- readRDS("outputs/phase04_wavelet_summary.rds")
grace_w <- readRDS("outputs/phase04_grace_wavelets.rds")

cat("  Persistence:", nrow(persistence), "basins\n")
cat("  Wavelet summary:", nrow(wavelet), "basins\n")
cat("  GRACE wavelets:", nrow(grace_w), "basins\n\n")

# ============================================================================
# MERGE DATASETS
# ============================================================================

cat("Merging persistence and period data...\n")

# Merge persistence with GRACE dominant periods
grace_pers_period <- grace_w %>%
  select(basin_id = basin, basin_name, dominant_period_1, dominant_power_1) %>%
  left_join(persistence, by = c("basin_id", "basin_name"))

# Get CESM/IPSL median periods from full wavelet data
cesm_w <- readRDS("outputs/phase04_cesm_all_wavelets.rds")
ipsl_w <- readRDS("outputs/phase04_ipsl_all_wavelets.rds")

cesm_period_medians <- cesm_w %>%
  filter(!is.na(period_1)) %>%
  group_by(basin, basin_name) %>%
  summarise(
    period_p50_cesm = median(period_1, na.rm = TRUE),
    .groups = "drop"
  )

ipsl_period_medians <- ipsl_w %>%
  filter(!is.na(period_1)) %>%
  group_by(basin, basin_name) %>%
  summarise(
    period_p50_ipsl = median(period_1, na.rm = TRUE),
    .groups = "drop"
  )

pers_period_combined <- persistence %>%
  left_join(cesm_period_medians, by = c("basin_id" = "basin", "basin_name")) %>%
  left_join(ipsl_period_medians, by = c("basin_id" = "basin", "basin_name"))

cat("  ✓ Data merged\n\n")

# ============================================================================
# PANEL A: Persistence vs Period (GRACE)
# ============================================================================

cat("Creating Panel A: Persistence vs period (GRACE)...\n")

grace_plot <- grace_pers_period %>%
  filter(!is.na(tau_grace) & !is.na(dominant_period_1))

# Compute correlation
cor_grace <- cor(grace_plot$dominant_period_1, grace_plot$tau_grace,
                 use = "complete.obs", method = "spearman")

panel_a <- ggplot(grace_plot, aes(x = dominant_period_1, y = tau_grace)) +
  geom_point(size = 2.5, alpha = 0.7, color = "#E69F00") +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.8,
              fill = "grey80", alpha = 0.3) +
  scale_x_continuous(
    name = "GRACE dominant period (years)",
    limits = c(2, 8),
    breaks = 2:8
  ) +
  scale_y_log10(
    name = "GRACE persistence (months)",
    breaks = c(1, 3, 10, 30, 100),
    limits = c(1, 150)
  ) +
  annotation_logticks(sides = "l", size = 0.3, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 2.2, y = 120, size = 3,
           label = sprintf("a\nSpearman ρ = %.2f", cor_grace),
           hjust = 0, vjust = 1, fontface = "bold")

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Persistence vs Period (CESM2)
# ============================================================================

cat("Creating Panel B: Persistence vs period (CESM2)...\n")

cesm_plot <- pers_period_combined %>%
  filter(!is.na(tau_p50_cesm) & !is.na(period_p50_cesm))

cor_cesm <- cor(cesm_plot$period_p50_cesm, cesm_plot$tau_p50_cesm,
                use = "complete.obs", method = "spearman")

panel_b <- ggplot(cesm_plot, aes(x = period_p50_cesm, y = tau_p50_cesm)) +
  geom_point(size = 2.5, alpha = 0.7, color = "#0072B2") +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.8,
              fill = "grey80", alpha = 0.3) +
  scale_x_continuous(
    name = "CESM2 dominant period (years, median)",
    limits = c(2, 8),
    breaks = 2:8
  ) +
  scale_y_log10(
    name = "CESM2 persistence (months, median)",
    breaks = c(1, 3, 10, 30, 100),
    limits = c(10, 150)
  ) +
  annotation_logticks(sides = "l", size = 0.3, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 2.2, y = 120, size = 3,
           label = sprintf("b\nSpearman ρ = %.2f", cor_cesm),
           hjust = 0, vjust = 1, fontface = "bold")

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Low-Freq Power vs Persistence (CESM2)
# ============================================================================

cat("Creating Panel C: Low-freq power vs persistence (CESM2)...\n")

power_pers_cesm <- wavelet %>%
  left_join(persistence %>% select(basin_id, tau_grace, tau_p50_cesm),
            by = "basin_id") %>%
  filter(!is.na(P_LF_grace) & !is.na(tau_grace) &
         !is.na(A_LF_p50_cesm) & !is.na(tau_p50_cesm))

# Compute directional classification
power_pers_cesm <- power_pers_cesm %>%
  mutate(
    power_direction = case_when(
      P_LF_grace > A_LF_p95_cesm ~ 'above_p95',
      P_LF_grace < A_LF_p05_cesm ~ 'below_p05',
      TRUE ~ 'within'
    )
  )

cor_power_cesm <- cor(log10(power_pers_cesm$A_LF_p50_cesm),
                      log10(power_pers_cesm$tau_p50_cesm),
                      use = "complete.obs", method = "spearman")

panel_c <- ggplot(power_pers_cesm, aes(x = A_LF_p50_cesm, y = tau_p50_cesm)) +
  geom_point(aes(fill = power_direction), shape = 21, size = 2.5,
             color = "black", stroke = 0.3, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.8,
              fill = "grey80", alpha = 0.3) +
  scale_fill_directional(name = "LF power\ndispersion") +
  scale_x_log10(
    name = "CESM2 low-freq power (mm, median)",
    breaks = c(10, 30, 100, 300),
    limits = c(5, 500)
  ) +
  scale_y_log10(
    name = "CESM2 persistence (months, median)",
    breaks = c(10, 30, 100),
    limits = c(10, 150)
  ) +
  annotation_logticks(size = 0.3, color = "grey50") +
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
  guides(fill = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  annotate("text", x = 6, y = 120, size = 3,
           label = sprintf("c\nSpearman ρ = %.2f", cor_power_cesm),
           hjust = 0, vjust = 1, fontface = "bold")

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Low-Freq Power vs Persistence (IPSL)
# ============================================================================

cat("Creating Panel D: Low-freq power vs persistence (IPSL)...\n")

power_pers_ipsl <- wavelet %>%
  left_join(persistence %>% select(basin_id, tau_grace, tau_p50_ipsl),
            by = "basin_id") %>%
  filter(!is.na(P_LF_grace) & !is.na(tau_grace) &
         !is.na(A_LF_p50_ipsl) & !is.na(tau_p50_ipsl))

# Compute directional classification
power_pers_ipsl <- power_pers_ipsl %>%
  mutate(
    power_direction = case_when(
      P_LF_grace > A_LF_p95_ipsl ~ 'above_p95',
      P_LF_grace < A_LF_p05_ipsl ~ 'below_p05',
      TRUE ~ 'within'
    )
  )

cor_power_ipsl <- cor(log10(power_pers_ipsl$A_LF_p50_ipsl),
                      log10(power_pers_ipsl$tau_p50_ipsl),
                      use = "complete.obs", method = "spearman")

panel_d <- ggplot(power_pers_ipsl, aes(x = A_LF_p50_ipsl, y = tau_p50_ipsl)) +
  geom_point(aes(fill = power_direction), shape = 21, size = 2.5,
             color = "black", stroke = 0.3, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.8,
              fill = "grey80", alpha = 0.3) +
  scale_fill_directional(name = "LF power\ndispersion") +
  scale_x_log10(
    name = "IPSL low-freq power (mm, median)",
    breaks = c(10, 30, 100, 300),
    limits = c(5, 500)
  ) +
  scale_y_log10(
    name = "IPSL persistence (months, median)",
    breaks = c(10, 30, 100),
    limits = c(10, 150)
  ) +
  annotation_logticks(size = 0.3, color = "grey50") +
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
  guides(fill = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  annotate("text", x = 6, y = 120, size = 3,
           label = sprintf("d\nSpearman ρ = %.2f", cor_power_ipsl),
           hjust = 0, vjust = 1, fontface = "bold")

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig6 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig6, "fig18_persistence_period", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 18 PERSISTENCE-PERIOD RELATIONSHIPS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig18_persistence_period.{png,pdf}\n")
cat("============================================================================\n\n")
