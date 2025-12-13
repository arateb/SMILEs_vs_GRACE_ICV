# ============================================================================
# PERSISTENCE SCATTER PLOTS - CESM2 and IPSL
# ============================================================================
#
# Scatter plots comparing GRACE persistence timescale vs model ensembles
# with directional coloring (within/above/below p95/p05)
#
# Panels:
#   (a) CESM2 persistence timescale (tau) scatter
#   (b) IPSL persistence timescale (tau) scatter
#   (c) CESM2 lag-1 autocorrelation scatter
#   (d) IPSL lag-1 autocorrelation scatter
#
# Colors:
#   - GRACE: #E69F00 (orange)
#   - CESM2: #0072B2 (blue)
#   - IPSL: #FF6B6B (light red)
#   - Directional: Light blue (within), Dark gray (above p95), Light gray (below p05)
#
# Output:
#   outputs/figs/fig_persistence_scatters.png (400 dpi)
#   outputs/figs/fig_persistence_scatters.pdf (vector)
#   outputs/figure_data/fig_persistence_scatters.csv
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("PERSISTENCE SCATTER PLOTS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading persistence data...\n")
pers <- readRDS("outputs/phase05_persistence_summary.rds")

# Rename bd_id to bd_ID
names(pers)[names(pers) == "bd_id"] <- "bd_ID"

cat("  Basins:", nrow(pers), "\n\n")

# ============================================================================
# PANEL A: CESM2 Persistence Timescale Scatter
# ============================================================================

cat("Creating Panel A: CESM2 persistence timescale scatter...\n")

# Add directional classification
pers <- pers %>%
  mutate(
    cesm_tau_direction = case_when(
      tau_grace > tau_p95_cesm ~ 'above_p95',
      tau_grace < tau_p05_cesm ~ 'below_p05',
      TRUE ~ 'within'
    ),
    ipsl_tau_direction = case_when(
      tau_grace > tau_p95_ipsl ~ 'above_p95',
      tau_grace < tau_p05_ipsl ~ 'below_p05',
      TRUE ~ 'within'
    )
  )

pers_cesm <- pers %>%
  filter(!is.na(tau_grace) & !is.na(tau_p50_cesm))

cesm_tau_counts <- table(pers_cesm$cesm_tau_direction)
cat("  CESM2 tau counts: within=", cesm_tau_counts["within"],
    ", above_p95=", cesm_tau_counts["above_p95"],
    ", below_p05=", cesm_tau_counts["below_p05"], "\n", sep="")

panel_a <- ggplot(pers_cesm, aes(x = tau_p50_cesm, y = tau_grace, fill = cesm_tau_direction)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = tau_p05_cesm, xmax = tau_p95_cesm, color = cesm_tau_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_log10(
    name = "CESM2 persistence (months, median)",
    breaks = c(1, 3, 10, 30),
    labels = c("1", "3", "10", "30"),
    limits = c(0.8, 50)
  ) +
  scale_y_log10(
    name = "GRACE persistence (months)",
    breaks = c(1, 3, 10, 30),
    labels = c("1", "3", "10", "30"),
    limits = c(0.8, 50)
  ) +
  annotation_logticks(sides = "bl", size = 0.3, color = "grey50") +
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
  guides(
    fill = guide_legend(override.aes = list(size = 3, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 1, y = 40,
           label = sprintf("a\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          cesm_tau_counts["within"], cesm_tau_counts["above_p95"], cesm_tau_counts["below_p05"]),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: IPSL Persistence Timescale Scatter
# ============================================================================

cat("Creating Panel B: IPSL persistence timescale scatter...\n")

pers_ipsl <- pers %>%
  filter(!is.na(tau_grace) & !is.na(tau_p50_ipsl))

ipsl_tau_counts <- table(pers_ipsl$ipsl_tau_direction)
cat("  IPSL tau counts: within=", ipsl_tau_counts["within"],
    ", above_p95=", ipsl_tau_counts["above_p95"],
    ", below_p05=", ipsl_tau_counts["below_p05"], "\n", sep="")

panel_b <- ggplot(pers_ipsl, aes(x = tau_p50_ipsl, y = tau_grace, fill = ipsl_tau_direction)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = tau_p05_ipsl, xmax = tau_p95_ipsl, color = ipsl_tau_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_log10(
    name = "IPSL persistence (months, median)",
    breaks = c(1, 3, 10, 30),
    labels = c("1", "3", "10", "30"),
    limits = c(0.8, 50)
  ) +
  scale_y_log10(
    name = "GRACE persistence (months)",
    breaks = c(1, 3, 10, 30),
    labels = c("1", "3", "10", "30"),
    limits = c(0.8, 50)
  ) +
  annotation_logticks(sides = "bl", size = 0.3, color = "grey50") +
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
  guides(
    fill = guide_legend(override.aes = list(size = 3, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 1, y = 40,
           label = sprintf("b\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          ipsl_tau_counts["within"], ipsl_tau_counts["above_p95"], ipsl_tau_counts["below_p05"]),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: CESM2 Lag-1 ACF Scatter
# ============================================================================

cat("Creating Panel C: CESM2 lag-1 autocorrelation scatter...\n")

# Add directional classification for ACF
pers <- pers %>%
  mutate(
    cesm_acf_direction = case_when(
      lag1_acf_grace > lag1_acf_p95_cesm ~ 'above_p95',
      lag1_acf_grace < lag1_acf_p05_cesm ~ 'below_p05',
      TRUE ~ 'within'
    ),
    ipsl_acf_direction = case_when(
      lag1_acf_grace > lag1_acf_p95_ipsl ~ 'above_p95',
      lag1_acf_grace < lag1_acf_p05_ipsl ~ 'below_p05',
      TRUE ~ 'within'
    )
  )

pers_cesm_acf <- pers %>%
  filter(!is.na(lag1_acf_grace) & !is.na(lag1_acf_p50_cesm))

cesm_acf_counts <- table(pers_cesm_acf$cesm_acf_direction)
cat("  CESM2 ACF counts: within=", cesm_acf_counts["within"],
    ", above_p95=", cesm_acf_counts["above_p95"],
    ", below_p05=", cesm_acf_counts["below_p05"], "\n", sep="")

panel_c <- ggplot(pers_cesm_acf, aes(x = lag1_acf_p50_cesm, y = lag1_acf_grace, fill = cesm_acf_direction)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lag1_acf_p05_cesm, xmax = lag1_acf_p95_cesm, color = cesm_acf_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_continuous(
    name = "CESM2 lag-1 ACF (median)",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  scale_y_continuous(
    name = "GRACE lag-1 ACF",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
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
  guides(
    fill = guide_legend(override.aes = list(size = 3, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 0.05, y = 0.95,
           label = sprintf("c\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          cesm_acf_counts["within"], cesm_acf_counts["above_p95"], cesm_acf_counts["below_p05"]),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: IPSL Lag-1 ACF Scatter
# ============================================================================

cat("Creating Panel D: IPSL lag-1 autocorrelation scatter...\n")

pers_ipsl_acf <- pers %>%
  filter(!is.na(lag1_acf_grace) & !is.na(lag1_acf_p50_ipsl))

ipsl_acf_counts <- table(pers_ipsl_acf$ipsl_acf_direction)
cat("  IPSL ACF counts: within=", ipsl_acf_counts["within"],
    ", above_p95=", ipsl_acf_counts["above_p95"],
    ", below_p05=", ipsl_acf_counts["below_p05"], "\n", sep="")

panel_d <- ggplot(pers_ipsl_acf, aes(x = lag1_acf_p50_ipsl, y = lag1_acf_grace, fill = ipsl_acf_direction)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lag1_acf_p05_ipsl, xmax = lag1_acf_p95_ipsl, color = ipsl_acf_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_continuous(
    name = "IPSL lag-1 ACF (median)",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  scale_y_continuous(
    name = "GRACE lag-1 ACF",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
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
  guides(
    fill = guide_legend(override.aes = list(size = 3, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 0.05, y = 0.95,
           label = sprintf("d\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          ipsl_acf_counts["within"], ipsl_acf_counts["above_p95"], ipsl_acf_counts["below_p05"]),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig_pers <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig_pers, "fig_persistence_scatters", width_mm = 180, height_mm = 140)

# ============================================================================
# EXPORT DATA TO CSV
# ============================================================================

cat("Exporting figure data to CSV...\n")

dir.create("outputs/figure_data", showWarnings = FALSE, recursive = TRUE)

pers_export <- pers %>%
  select(
    basin_id = bd_ID,
    basin_name,
    # Persistence timescale
    tau_grace,
    tau_cesm_p05 = tau_p05_cesm,
    tau_cesm_p50 = tau_p50_cesm,
    tau_cesm_p95 = tau_p95_cesm,
    tau_ipsl_p05 = tau_p05_ipsl,
    tau_ipsl_p50 = tau_p50_ipsl,
    tau_ipsl_p95 = tau_p95_ipsl,
    cesm_tau_dispersion = cesm_tau_direction,
    ipsl_tau_dispersion = ipsl_tau_direction,
    # Lag-1 autocorrelation
    lag1_acf_grace,
    lag1_acf_cesm_p05 = lag1_acf_p05_cesm,
    lag1_acf_cesm_p50 = lag1_acf_p50_cesm,
    lag1_acf_cesm_p95 = lag1_acf_p95_cesm,
    lag1_acf_ipsl_p05 = lag1_acf_p05_ipsl,
    lag1_acf_ipsl_p50 = lag1_acf_p50_ipsl,
    lag1_acf_ipsl_p95 = lag1_acf_p95_ipsl,
    cesm_acf_dispersion = cesm_acf_direction,
    ipsl_acf_dispersion = ipsl_acf_direction
  )

write.csv(pers_export, "outputs/figure_data/fig_persistence_scatters.csv", row.names = FALSE)

cat("  ✓ Exported: outputs/figure_data/fig_persistence_scatters.csv\n\n")

cat("\n")
cat("============================================================================\n")
cat("PERSISTENCE SCATTERS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig_persistence_scatters.{png,pdf}\n")
cat("  Data: outputs/figure_data/fig_persistence_scatters.csv\n")
cat("============================================================================\n\n")
