# ============================================================================
# FIGURE 3: Timescale and Persistence - Directional Analysis
# ============================================================================
#
# Following Figure 2 approach:
#   - Directional classification (within/above_p95/below_p05)
#   - Maps + scatter plots with counts
#   - Light blue for within envelope
#
# Panels:
#   (a) Map: CESM2 low-freq power dispersion
#   (b) Map: CESM2 persistence timescale dispersion
#   (c) Scatter: GRACE vs CESM2 low-freq power
#   (d) Scatter: GRACE vs CESM2 persistence timescale
#
# Output:
#   outputs/figs/fig3_timescale_directional.png (400 dpi)
#   outputs/figs/fig3_timescale_directional.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3: TIMESCALE AND PERSISTENCE (DIRECTIONAL)\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading data...\n")
persistence <- readRDS("outputs/phase05_persistence_summary.rds")
wavelet <- readRDS("outputs/phase04_wavelet_summary.rds")

# Load basin shapefile
basins_shp <- st_read("/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)
basins_shp <- st_simplify(basins_shp, dTolerance = 0.1, preserveTopology = TRUE)

# Load country borders
sf::sf_use_s2(FALSE)
world_borders <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
sf::sf_use_s2(TRUE)

cat("  Persistence results:", nrow(persistence), "\n")
cat("  Wavelet results:", nrow(wavelet), "\n\n")

# ============================================================================
# Add directional classifications
# ============================================================================

cat("Computing directional classifications...\n")

# Wavelet (low-frequency power)
wavelet <- wavelet %>%
  mutate(
    cesm_lf_direction = case_when(
      P_LF_grace > A_LF_p95_cesm ~ 'above_p95',
      P_LF_grace < A_LF_p05_cesm ~ 'below_p05',
      TRUE ~ 'within'
    ),
    ipsl_lf_direction = case_when(
      P_LF_grace > A_LF_p95_ipsl ~ 'above_p95',
      P_LF_grace < A_LF_p05_ipsl ~ 'below_p05',
      TRUE ~ 'within'
    )
  )

# Persistence
persistence <- persistence %>%
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

# bd_ID column exists from consolidation script for wavelet
# Persistence has bd_id (lowercase d) - rename to match
names(persistence)[names(persistence) == "bd_id"] <- "bd_ID"

# Verify both have bd_ID
if (!"bd_ID" %in% names(wavelet)) {
  stop("bd_ID column missing from wavelet data!")
}
if (!"bd_ID" %in% names(persistence)) {
  stop("bd_ID column missing from persistence data!")
}

cat("  ✓ Classifications complete\n\n")

# ============================================================================
# PANEL A: CESM2 Low-Frequency Power Map
# ============================================================================

cat("Creating Panel A: CESM2 low-frequency power map...\n")

basins_shp_lf <- merge(basins_shp,
                        wavelet[, c("bd_ID", "cesm_lf_direction")],
                        by = "bd_ID", all.x = TRUE)

basins_shp_lf$cesm_lf_direction <- factor(
  basins_shp_lf$cesm_lf_direction,
  levels = c("within", "above_p95", "below_p05")
)

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_lf %>% filter(!is.na(cesm_lf_direction)),
          aes(fill = cesm_lf_direction),
          color = "black", linewidth = 0.1) +
  scale_fill_directional(name = "CESM2\nLow-freq") +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: CESM2 Persistence Timescale Map
# ============================================================================

cat("Creating Panel B: CESM2 persistence timescale map...\n")

basins_shp_tau <- merge(basins_shp,
                         persistence[, c("bd_ID", "cesm_tau_direction")],
                         by = "bd_ID", all.x = TRUE)

basins_shp_tau$cesm_tau_direction <- factor(
  basins_shp_tau$cesm_tau_direction,
  levels = c("within", "above_p95", "below_p05")
)

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_tau %>% filter(!is.na(cesm_tau_direction)),
          aes(fill = cesm_tau_direction),
          color = "black", linewidth = 0.1) +
  scale_fill_directional(name = "CESM2\nPersistence") +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Low-Frequency Power Scatter
# ============================================================================

cat("Creating Panel C: Low-frequency power scatter...\n")

wavelet_plot <- wavelet %>%
  filter(!is.na(P_LF_grace) & !is.na(A_LF_p50_cesm))

lf_counts <- table(wavelet_plot$cesm_lf_direction)
cat("  LF counts: within=", lf_counts["within"],
    ", above_p95=", lf_counts["above_p95"],
    ", below_p05=", lf_counts["below_p05"], "\n", sep="")

panel_c <- ggplot(wavelet_plot, aes(x = A_LF_p50_cesm, y = P_LF_grace)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = A_LF_p05_cesm, xmax = A_LF_p95_cesm, color = cesm_lf_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(aes(fill = cesm_lf_direction),
             shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_log10(
    name = "CESM2 low-freq power (mm, median)",
    breaks = c(10, 30, 100, 300),
    labels = c("10", "30", "100", "300"),
    limits = c(5, 500)
  ) +
  scale_y_log10(
    name = "GRACE low-freq power (mm)",
    breaks = c(10, 30, 100, 300),
    labels = c("10", "30", "100", "300"),
    limits = c(5, 500)
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
  annotate("text", x = 6, y = 400,
           label = sprintf("c\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          lf_counts["within"], lf_counts["above_p95"], lf_counts["below_p05"]),
           size = 3, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Persistence Timescale Scatter
# ============================================================================

cat("Creating Panel D: Persistence timescale scatter...\n")

persistence_plot <- persistence %>%
  filter(!is.na(tau_grace) & !is.na(tau_p50_cesm))

tau_counts <- table(persistence_plot$cesm_tau_direction)
cat("  Tau counts: within=", tau_counts["within"],
    ", above_p95=", tau_counts["above_p95"],
    ", below_p05=", tau_counts["below_p05"], "\n", sep="")

panel_d <- ggplot(persistence_plot, aes(x = tau_p50_cesm, y = tau_grace)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = tau_p05_cesm, xmax = tau_p95_cesm, color = cesm_tau_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(aes(fill = cesm_tau_direction),
             shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
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
           label = sprintf("d\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          tau_counts["within"], tau_counts["above_p95"], tau_counts["below_p05"]),
           size = 3, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: A and B on top (side by side), C and D below (side by side)
fig3 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1.2, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig3, "fig3_timescale_directional", width_mm = 180, height_mm = 150)

# ============================================================================
# EXPORT DATA TO CSV
# ============================================================================

cat("Exporting figure data to CSV...\n")

dir.create("outputs/figure_data", showWarnings = FALSE, recursive = TRUE)

# Merge wavelet and persistence data
# Don't use suffix to avoid column renaming issues
fig3_data_merged <- wavelet %>%
  left_join(persistence, by = c("bd_ID", "basin_name"))

# Select and rename columns explicitly
fig3_data <- fig3_data_merged %>%
  select(
    basin_id = bd_ID,
    basin_name,
    # Low-frequency power (from wavelet)
    lf_grace = P_LF_grace,
    lf_cesm_p05 = A_LF_p05_cesm.x,
    lf_cesm_p50 = A_LF_p50_cesm.x,
    lf_cesm_p95 = A_LF_p95_cesm.x,
    lf_ipsl_p05 = A_LF_p05_ipsl.x,
    lf_ipsl_p50 = A_LF_p50_ipsl.x,
    lf_ipsl_p95 = A_LF_p95_ipsl.x,
    cesm_lf_dispersion = cesm_lf_direction,
    ipsl_lf_dispersion = ipsl_lf_direction,
    # Persistence timescale (from persistence)
    tau_grace,
    tau_cesm_p05 = tau_p05_cesm,
    tau_cesm_p50 = tau_p50_cesm,
    tau_cesm_p95 = tau_p95_cesm,
    tau_ipsl_p05 = tau_p05_ipsl,
    tau_ipsl_p50 = tau_p50_ipsl,
    tau_ipsl_p95 = tau_p95_ipsl,
    cesm_tau_dispersion = cesm_tau_direction,
    ipsl_tau_dispersion = ipsl_tau_direction
  )

write.csv(fig3_data, "outputs/figure_data/fig3_timescale_directional.csv",
          row.names = FALSE)

cat("  ✓ Exported: outputs/figure_data/fig3_timescale_directional.csv\n")

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3 DIRECTIONAL COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig3_timescale_directional.{png,pdf}\n")
cat("  Data: outputs/figure_data/fig3_timescale_directional.csv\n")
cat("============================================================================\n\n")
