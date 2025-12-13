# ============================================================================
# FIGURE 2: Dispersion Maps - GRACE vs SMILE Envelopes (DIRECTIONAL)
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. Do SMILEs capture observed GRACE TWS variability across global basins?
#   2. Where do models systematically under-disperse (GRACE > p95)?
#   3. Where do models systematically over-disperse (GRACE < p05)?
#   4. Are amplitude and variance biases spatially coherent?
#
# Panels:
#   (a) Map: CESM2 Amplitude dispersion (within/above_p95/below_p05)
#   (b) Map: CESM2 Variance dispersion (within/above_p95/below_p05)
#   (c) Scatter: GRACE amplitude vs CESM2 envelope with directional colors
#   (d) Scatter: GRACE variance vs CESM2 envelope with directional colors
#
# Color scheme (DIRECTIONAL):
#   - Light Blue: GRACE within [p05, p95] envelope (captured)
#   - Dark Gray: GRACE > p95 (model under-dispersed, too little variability)
#   - Light Gray: GRACE < p05 (model over-dispersed, too much variability)
#
# Output:
#   outputs/figs/fig2_dispersion_maps.png (400 dpi)
#   outputs/figs/fig2_dispersion_maps.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 2: DISPERSION MAPS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading data...\n")
disp <- read.csv("outputs/dispersion_summary.csv")

# Rename bd_id to bd_ID to match shapefile column name
names(disp)[names(disp) == "bd_id"] <- "bd_ID"

# Compute directional classification columns from raw data
# CESM2 amplitude
disp$cesm_amplitude_direction <- ifelse(disp$A_grace > disp$A_p95_cesm, "above_p95",
                                  ifelse(disp$A_grace < disp$A_p05_cesm, "below_p05", "within"))
# CESM2 variance
disp$cesm_variance_direction <- ifelse(disp$sigma_grace > disp$sigma_p95_cesm, "above_p95",
                                 ifelse(disp$sigma_grace < disp$sigma_p05_cesm, "below_p05", "within"))
# IPSL amplitude
disp$ipsl_amplitude_direction <- ifelse(disp$A_grace > disp$A_p95_ipsl, "above_p95",
                                  ifelse(disp$A_grace < disp$A_p05_ipsl, "below_p05", "within"))
# IPSL variance
disp$ipsl_variance_direction <- ifelse(disp$sigma_grace > disp$sigma_p95_ipsl, "above_p95",
                                 ifelse(disp$sigma_grace < disp$sigma_p05_ipsl, "below_p05", "within"))

# Print counts for verification
cat("  CESM2 amplitude: above_p95=", sum(disp$cesm_amplitude_direction == "above_p95", na.rm=T),
    ", within=", sum(disp$cesm_amplitude_direction == "within", na.rm=T),
    ", below_p05=", sum(disp$cesm_amplitude_direction == "below_p05", na.rm=T), "\n", sep="")
cat("  IPSL amplitude: above_p95=", sum(disp$ipsl_amplitude_direction == "above_p95", na.rm=T),
    ", within=", sum(disp$ipsl_amplitude_direction == "within", na.rm=T),
    ", below_p05=", sum(disp$ipsl_amplitude_direction == "below_p05", na.rm=T), "\n", sep="")

# Load basin shapefile
cat("Loading basin shapefile...\n")
basins_shp <- st_read("/Volumes/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)

# Simplify geometries to reduce PDF file size (tolerance = 0.1 degrees ~ 10km)
cat("  Simplifying geometries for smaller file size...\n")
basins_shp <- st_simplify(basins_shp, dTolerance = 0.1, preserveTopology = TRUE)

# Load country borders (simplified, turn off s2)
sf::sf_use_s2(FALSE)
world_borders <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
sf::sf_use_s2(TRUE)

cat("  Basins:", nrow(disp), "\n")
cat("  Shapefile basins:", nrow(basins_shp), "\n\n")

# ============================================================================
# PANEL A: CESM2 Amplitude Dispersion Map
# ============================================================================

cat("Creating Panel A: CESM2 amplitude dispersion map...\n")

# Merge CESM amplitude directional classification onto shapefile
basins_shp_amp_cesm <- merge(basins_shp,
                              disp[, c("bd_ID", "cesm_amplitude_direction")],
                              by = "bd_ID", all.x = TRUE)

# Ensure proper factor levels
basins_shp_amp_cesm$cesm_amplitude_direction <- factor(
  basins_shp_amp_cesm$cesm_amplitude_direction,
  levels = c("within", "above_p95", "below_p05")
)

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_amp_cesm %>% filter(!is.na(cesm_amplitude_direction)),
          aes(fill = cesm_amplitude_direction),
          color = "black", linewidth = 0.1) +
  scale_fill_directional(name = "CESM2\nAmplitude") +
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
# PANEL B: CESM2 Variance Dispersion Map
# ============================================================================

cat("Creating Panel B: CESM2 variance dispersion map...\n")

# Merge CESM variance directional classification onto shapefile
basins_shp_var_cesm <- merge(basins_shp,
                              disp[, c("bd_ID", "cesm_variance_direction")],
                              by = "bd_ID", all.x = TRUE)

# Ensure proper factor levels
basins_shp_var_cesm$cesm_variance_direction <- factor(
  basins_shp_var_cesm$cesm_variance_direction,
  levels = c("within", "above_p95", "below_p05")
)

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_var_cesm %>% filter(!is.na(cesm_variance_direction)),
          aes(fill = cesm_variance_direction),
          color = "black", linewidth = 0.1) +
  scale_fill_directional(name = "CESM2\nVariance") +
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
# PANEL C: GRACE Amplitude vs CESM2 Envelope (Scatter with directional colors)
# ============================================================================

cat("Creating Panel C: GRACE amplitude vs CESM2 scatter...\n")

# Prepare data with directional classification
disp_amp <- disp %>%
  filter(!is.na(A_grace) & !is.na(A_p50_cesm))

# Count basins in each category
amp_counts <- table(disp_amp$cesm_amplitude_direction)
cat("  Amplitude counts: within=", amp_counts["within"],
    ", above_p95=", amp_counts["above_p95"],
    ", below_p05=", amp_counts["below_p05"], "\n", sep="")

panel_c <- ggplot(disp_amp, aes(x = A_p50_cesm, y = A_grace)) +
  # Reference line (1:1)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  # CESM envelope ribbons (p05-p95) as error bars
  geom_errorbarh(aes(xmin = A_p05_cesm, xmax = A_p95_cesm, color = cesm_amplitude_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  # Points colored by directional classification
  geom_point(aes(fill = cesm_amplitude_direction),
             shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
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
  annotate("text", x = 30, y = 1200,
           label = sprintf("c\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          amp_counts["within"], amp_counts["above_p95"], amp_counts["below_p05"]),
           size = 3, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: GRACE Variance vs CESM2 Envelope (Scatter with directional colors)
# ============================================================================

cat("Creating Panel D: GRACE variance vs CESM2 scatter...\n")

# Prepare data with directional classification
disp_var <- disp %>%
  filter(!is.na(sigma_grace) & !is.na(sigma_p50_cesm))

# Count basins in each category
var_counts <- table(disp_var$cesm_variance_direction)
cat("  Variance counts: within=", var_counts["within"],
    ", above_p95=", var_counts["above_p95"],
    ", below_p05=", var_counts["below_p05"], "\n", sep="")

panel_d <- ggplot(disp_var, aes(x = sigma_p50_cesm, y = sigma_grace)) +
  # Reference line (1:1)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  # CESM envelope ribbons (p05-p95) as error bars
  geom_errorbarh(aes(xmin = sigma_p05_cesm, xmax = sigma_p95_cesm, color = cesm_variance_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  # Points colored by directional classification
  geom_point(aes(fill = cesm_variance_direction),
             shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_log10(
    name = "CESM2 variance (mm, median)",
    breaks = c(10, 30, 100, 300),
    labels = c("10", "30", "100", "300"),
    limits = c(8, 500)
  ) +
  scale_y_log10(
    name = "GRACE variance (mm)",
    breaks = c(10, 30, 100, 300),
    labels = c("10", "30", "100", "300"),
    limits = c(8, 500)
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
  annotate("text", x = 10, y = 400,
           label = sprintf("d\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          var_counts["within"], var_counts["above_p95"], var_counts["below_p05"]),
           size = 3, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: A and B on top (side by side), C and D below (side by side)
fig2_maps <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1.2, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

# Create dated output directory
today <- format(Sys.Date(), "%Y%m%d")
output_dir <- paste0("outputs/run_", today)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
cat("Saving to:", output_dir, "\n")

# Save with dated filename
ggsave(file.path(output_dir, paste0("fig2_dispersion_maps_", today, ".png")),
       fig2_maps, width = 180, height = 150, units = "mm", dpi = 500)
ggsave(file.path(output_dir, paste0("fig2_dispersion_maps_", today, ".pdf")),
       fig2_maps, width = 180, height = 150, units = "mm")
cat("  Saved:", file.path(output_dir, paste0("fig2_dispersion_maps_", today, ".{png,pdf}")), "\n")

# Also save to standard location
save_figure(fig2_maps, "fig2_dispersion_maps", width_mm = 180, height_mm = 150)

# ============================================================================
# EXPORT DATA TO CSV
# ============================================================================

cat("Exporting figure data to CSV...\n")

# Create output directory
dir.create("outputs/figure_data", showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "figure_data"), showWarnings = FALSE, recursive = TRUE)

# Export data for all panels
fig2_data <- disp %>%
  select(basin_id, basin_name,
         # Amplitude metrics
         amplitude_grace = A_grace,
         amplitude_cesm_p05 = A_p05_cesm,
         amplitude_cesm_p50 = A_p50_cesm,
         amplitude_cesm_p95 = A_p95_cesm,
         amplitude_ipsl_p05 = A_p05_ipsl,
         amplitude_ipsl_p50 = A_p50_ipsl,
         amplitude_ipsl_p95 = A_p95_ipsl,
         # Variance metrics
         variance_grace = sigma_grace,
         variance_cesm_p05 = sigma_p05_cesm,
         variance_cesm_p50 = sigma_p50_cesm,
         variance_cesm_p95 = sigma_p95_cesm,
         variance_ipsl_p05 = sigma_p05_ipsl,
         variance_ipsl_p50 = sigma_p50_ipsl,
         variance_ipsl_p95 = sigma_p95_ipsl,
         # Directional classifications (CESM2)
         cesm_amplitude_dispersion = cesm_amplitude_direction,
         cesm_variance_dispersion = cesm_variance_direction,
         # Directional classifications (IPSL)
         ipsl_amplitude_dispersion = ipsl_amplitude_direction,
         ipsl_variance_dispersion = ipsl_variance_direction,
         # Return periods
         return_period_cesm = T_A_cesm,
         return_period_ipsl = T_A_ipsl)

write.csv(fig2_data, "outputs/figure_data/fig2_dispersion_maps.csv",
          row.names = FALSE)

cat("  ✓ Exported: outputs/figure_data/fig2_dispersion_maps.csv\n")

cat("\n")
cat("============================================================================\n")
cat("FIGURE 2 DISPERSION MAPS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig2_dispersion_maps.{png,pdf}\n")
cat("  Data: outputs/figure_data/fig2_dispersion_maps.csv\n")
cat("============================================================================\n\n")
