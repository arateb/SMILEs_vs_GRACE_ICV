# ============================================================================
# FIGURE 2 EXTENDED: Dispersion Maps - CESM2 and IPSL
# ============================================================================
#
# 8-panel figure showing both CESM2 and IPSL:
#   Row 1: CESM2 amplitude map | IPSL amplitude map
#   Row 2: CESM2 variance map | IPSL variance map
#   Row 3: CESM2 amplitude scatter | IPSL amplitude scatter
#   Row 4: CESM2 variance scatter | IPSL variance scatter
#
# Color scheme:
#   - Light Blue: Within [p05, p95] envelope (captured)
#   - Dark Gray: Above p95 (under-dispersed)
#   - Light Gray: Below p05 (over-dispersed)
#
# Output:
#   outputs/figs/fig2_extended_dispersion.png (400 dpi)
#   outputs/figs/fig2_extended_dispersion.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 2 EXTENDED: DISPERSION MAPS (CESM2 AND IPSL)\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading data...\n")
disp <- readRDS("outputs/dispersion_summary.rds")

# Rename bd_id to bd_ID to match shapefile column name
names(disp)[names(disp) == "bd_id"] <- "bd_ID"

# Load basin shapefile
cat("Loading basin shapefile...\n")
basins_shp <- st_read("/Volumes/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)
basins_shp <- st_simplify(basins_shp, dTolerance = 0.1, preserveTopology = TRUE)

# Load country borders
sf::sf_use_s2(FALSE)
world_borders <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
sf::sf_use_s2(TRUE)

cat("  Basins:", nrow(disp), "\n\n")

# ============================================================================
# PANEL A: CESM2 Amplitude Map
# ============================================================================

cat("Creating Panel A: CESM2 amplitude map...\n")

basins_shp_cesm_amp <- merge(basins_shp,
                              disp[, c("bd_ID", "cesm_amplitude_direction")],
                              by = "bd_ID", all.x = TRUE)

basins_shp_cesm_amp$cesm_amplitude_direction <- factor(
  basins_shp_cesm_amp$cesm_amplitude_direction,
  levels = c("within", "above_p95", "below_p05")
)

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_cesm_amp %>% filter(!is.na(cesm_amplitude_direction)),
          aes(fill = cesm_amplitude_direction),
          color = "black", linewidth = 0.1) +
  scale_fill_directional(name = "CESM2\nAmplitude") +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 7) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(2.5, "mm"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 4, fontface = "bold", hjust = 0)

# ============================================================================
# PANEL B: IPSL Amplitude Map
# ============================================================================

cat("Creating Panel B: IPSL amplitude map...\n")

basins_shp_ipsl_amp <- merge(basins_shp,
                              disp[, c("bd_ID", "ipsl_amplitude_direction")],
                              by = "bd_ID", all.x = TRUE)

basins_shp_ipsl_amp$ipsl_amplitude_direction <- factor(
  basins_shp_ipsl_amp$ipsl_amplitude_direction,
  levels = c("within", "above_p95", "below_p05")
)

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_ipsl_amp %>% filter(!is.na(ipsl_amplitude_direction)),
          aes(fill = ipsl_amplitude_direction),
          color = "black", linewidth = 0.1) +
  scale_fill_directional(name = "IPSL\nAmplitude") +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 7) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(2.5, "mm"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "b", size = 4, fontface = "bold", hjust = 0)

# ============================================================================
# PANEL C: CESM2 Variance Map
# ============================================================================

cat("Creating Panel C: CESM2 variance map...\n")

basins_shp_cesm_var <- merge(basins_shp,
                              disp[, c("bd_ID", "cesm_variance_direction")],
                              by = "bd_ID", all.x = TRUE)

basins_shp_cesm_var$cesm_variance_direction <- factor(
  basins_shp_cesm_var$cesm_variance_direction,
  levels = c("within", "above_p95", "below_p05")
)

panel_c <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_cesm_var %>% filter(!is.na(cesm_variance_direction)),
          aes(fill = cesm_variance_direction),
          color = "black", linewidth = 0.1) +
  scale_fill_directional(name = "CESM2\nVariance") +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 7) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(2.5, "mm"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "c", size = 4, fontface = "bold", hjust = 0)

# ============================================================================
# PANEL D: IPSL Variance Map
# ============================================================================

cat("Creating Panel D: IPSL variance map...\n")

basins_shp_ipsl_var <- merge(basins_shp,
                              disp[, c("bd_ID", "ipsl_variance_direction")],
                              by = "bd_ID", all.x = TRUE)

basins_shp_ipsl_var$ipsl_variance_direction <- factor(
  basins_shp_ipsl_var$ipsl_variance_direction,
  levels = c("within", "above_p95", "below_p05")
)

panel_d <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_ipsl_var %>% filter(!is.na(ipsl_variance_direction)),
          aes(fill = ipsl_variance_direction),
          color = "black", linewidth = 0.1) +
  scale_fill_directional(name = "IPSL\nVariance") +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 7) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(2.5, "mm"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "d", size = 4, fontface = "bold", hjust = 0)

# ============================================================================
# PANEL E: CESM2 Amplitude Scatter
# ============================================================================

cat("Creating Panel E: CESM2 amplitude scatter...\n")

disp_cesm_amp <- disp %>%
  filter(!is.na(A_grace) & !is.na(A_p50_cesm))

cesm_amp_counts <- table(disp_cesm_amp$cesm_amplitude_direction)
cat("  CESM2 amplitude: within=", cesm_amp_counts["within"],
    ", above_p95=", cesm_amp_counts["above_p95"],
    ", below_p05=", cesm_amp_counts["below_p05"], "\n", sep="")

panel_e <- ggplot(disp_cesm_amp, aes(x = A_p50_cesm, y = A_grace)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = A_p05_cesm, xmax = A_p95_cesm, color = cesm_amplitude_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(aes(fill = cesm_amplitude_direction),
             shape = 21, size = 2, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_log10(
    name = "CESM2 amplitude (mm, median)",
    breaks = c(30, 100, 300, 1000),
    labels = c("30", "100", "300", "1000"),
    limits = c(25, 1500)
  ) +
  scale_y_log10(
    name = "GRACE amplitude (mm)",
    breaks = c(30, 100, 300, 1000),
    labels = c("30", "100", "300", "1000"),
    limits = c(25, 1500)
  ) +
  annotation_logticks(sides = "bl", size = 0.3, color = "grey50") +
  theme_nature(base_size = 7) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(2.5, "mm"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 2.5, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 30, y = 1200,
           label = sprintf("e\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          cesm_amp_counts["within"], cesm_amp_counts["above_p95"], cesm_amp_counts["below_p05"]),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

# ============================================================================
# PANEL F: IPSL Amplitude Scatter
# ============================================================================

cat("Creating Panel F: IPSL amplitude scatter...\n")

disp_ipsl_amp <- disp %>%
  filter(!is.na(A_grace) & !is.na(A_p50_ipsl))

ipsl_amp_counts <- table(disp_ipsl_amp$ipsl_amplitude_direction)
cat("  IPSL amplitude: within=", ipsl_amp_counts["within"],
    ", above_p95=", ipsl_amp_counts["above_p95"],
    ", below_p05=", ipsl_amp_counts["below_p05"], "\n", sep="")

panel_f <- ggplot(disp_ipsl_amp, aes(x = A_p50_ipsl, y = A_grace)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = A_p05_ipsl, xmax = A_p95_ipsl, color = ipsl_amplitude_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(aes(fill = ipsl_amplitude_direction),
             shape = 21, size = 2, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_log10(
    name = "IPSL amplitude (mm, median)",
    breaks = c(30, 100, 300, 1000),
    labels = c("30", "100", "300", "1000"),
    limits = c(25, 1500)
  ) +
  scale_y_log10(
    name = "GRACE amplitude (mm)",
    breaks = c(30, 100, 300, 1000),
    labels = c("30", "100", "300", "1000"),
    limits = c(25, 1500)
  ) +
  annotation_logticks(sides = "bl", size = 0.3, color = "grey50") +
  theme_nature(base_size = 7) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(2.5, "mm"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 2.5, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 30, y = 1200,
           label = sprintf("f\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          ipsl_amp_counts["within"], ipsl_amp_counts["above_p95"], ipsl_amp_counts["below_p05"]),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel F complete\n")

# ============================================================================
# PANEL G: CESM2 Variance Scatter
# ============================================================================

cat("Creating Panel G: CESM2 variance scatter...\n")

disp_cesm_var <- disp %>%
  filter(!is.na(sigma_grace) & !is.na(sigma_p50_cesm))

cesm_var_counts <- table(disp_cesm_var$cesm_variance_direction)
cat("  CESM2 variance: within=", cesm_var_counts["within"],
    ", above_p95=", cesm_var_counts["above_p95"],
    ", below_p05=", cesm_var_counts["below_p05"], "\n", sep="")

panel_g <- ggplot(disp_cesm_var, aes(x = sigma_p50_cesm, y = sigma_grace)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = sigma_p05_cesm, xmax = sigma_p95_cesm, color = cesm_variance_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(aes(fill = cesm_variance_direction),
             shape = 21, size = 2, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_log10(
    name = "CESM2 std dev (mm, median)",
    breaks = c(10, 30, 100, 300, 1000),
    labels = c("10", "30", "100", "300", "1000"),
    limits = c(5, 1500)
  ) +
  scale_y_log10(
    name = "GRACE std dev (mm)",
    breaks = c(10, 30, 100, 300, 1000),
    labels = c("10", "30", "100", "300", "1000"),
    limits = c(5, 1500)
  ) +
  annotation_logticks(sides = "bl", size = 0.3, color = "grey50") +
  theme_nature(base_size = 7) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(2.5, "mm"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 2.5, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 6, y = 1200,
           label = sprintf("g\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          cesm_var_counts["within"], cesm_var_counts["above_p95"], cesm_var_counts["below_p05"]),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel G complete\n")

# ============================================================================
# PANEL H: IPSL Variance Scatter
# ============================================================================

cat("Creating Panel H: IPSL variance scatter...\n")

disp_ipsl_var <- disp %>%
  filter(!is.na(sigma_grace) & !is.na(sigma_p50_ipsl))

ipsl_var_counts <- table(disp_ipsl_var$ipsl_variance_direction)
cat("  IPSL variance: within=", ipsl_var_counts["within"],
    ", above_p95=", ipsl_var_counts["above_p95"],
    ", below_p05=", ipsl_var_counts["below_p05"], "\n", sep="")

panel_h <- ggplot(disp_ipsl_var, aes(x = sigma_p50_ipsl, y = sigma_grace)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = sigma_p05_ipsl, xmax = sigma_p95_ipsl, color = ipsl_variance_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(aes(fill = ipsl_variance_direction),
             shape = 21, size = 2, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_log10(
    name = "IPSL std dev (mm, median)",
    breaks = c(10, 30, 100, 300, 1000),
    labels = c("10", "30", "100", "300", "1000"),
    limits = c(5, 1500)
  ) +
  scale_y_log10(
    name = "GRACE std dev (mm)",
    breaks = c(10, 30, 100, 300, 1000),
    labels = c("10", "30", "100", "300", "1000"),
    limits = c(5, 1500)
  ) +
  annotation_logticks(sides = "bl", size = 0.3, color = "grey50") +
  theme_nature(base_size = 7) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(2.5, "mm"),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 2.5, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 6, y = 1200,
           label = sprintf("h\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          ipsl_var_counts["within"], ipsl_var_counts["above_p95"], ipsl_var_counts["below_p05"]),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel H complete\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("\nAssembling panels...\n")

# 4 rows × 2 columns
fig2_extended <- (panel_a | panel_b) /
  (panel_c | panel_d) /
  (panel_e | panel_f) /
  (panel_g | panel_h) +
  plot_layout(heights = c(1, 1, 1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig2_extended, "fig2_extended_dispersion", width_mm = 180, height_mm = 280)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 2 EXTENDED COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig2_extended_dispersion.{png,pdf}\n")
cat("============================================================================\n\n")
