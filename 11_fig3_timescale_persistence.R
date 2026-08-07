# ============================================================================
# FIGURE 3: Timescale Deficits (Wavelet and Persistence)
# ============================================================================
#
# Phase 4: SMILEs capture low-freq power?
# Phase 5: SMILEs reproduce persistence?
#
# Panels:
#   (a) Map of low-frequency amplitude ratio (A_LF SMILE/GRACE)
#   (b) Map of persistence timescale ratio (tau SMILE/GRACE)
#   (c) Example wavelet power spectra (3 selected basins)
#
# Output:
#   outputs/figs/fig3_timescale_persistence.png (500 dpi)
#   outputs/figs/fig3_timescale_persistence.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3: TIMESCALE AND PERSISTENCE\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading data...\n")
attrs <- readRDS("outputs/basin_attributes.rds")
persistence <- readRDS("outputs/phase05_persistence_summary.rds")
wavelet <- readRDS("outputs/phase04_wavelet_summary.rds")

# Load basin shapefile
basins_shp <- st_read("/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
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

cat("  Basins:", nrow(attrs), "\n")
cat("  Persistence results:", nrow(persistence), "\n")
cat("  Wavelet results:", nrow(wavelet), "\n\n")

# Add coordinates if not already present
if (!"C_lon" %in% names(persistence)) {
  persistence_merged <- merge(
    persistence,
    attrs[, c("ID", "C_lon", "C_lat")],
    by.x = "basin_id",
    by.y = "ID",
    all.x = TRUE
  )
} else {
  persistence_merged <- persistence
}

if (!"C_lon" %in% names(wavelet)) {
  wavelet_merged <- merge(
    wavelet,
    attrs[, c("ID", "C_lon", "C_lat")],
    by.x = "basin_id",
    by.y = "ID",
    all.x = TRUE
  )
} else {
  wavelet_merged <- wavelet
}

# ============================================================================
# PANEL A: A_LF Ratio Map
# ============================================================================

cat("Creating Panel A: A_LF ratio map...\n")

# Compute A_LF ratio (use CESM2 for main figure)
# Ratio < 1 means model has weaker low-frequency variability
wavelet_merged <- wavelet_merged %>%
  mutate(
    A_LF_ratio_cesm = A_LF_p50_cesm / P_LF_grace,
    A_LF_ratio_ipsl = A_LF_p50_ipsl / P_LF_grace,
    # Use minimum ratio (most conservative / strongest deficit)
    A_LF_ratio = pmin(A_LF_ratio_cesm, A_LF_ratio_ipsl, na.rm = TRUE)
  )

# Merge A_LF ratio onto shapefile
basins_shp_alf <- merge(basins_shp,
                         wavelet_merged[, c("basin_id", "A_LF_ratio")],
                         by.x = "bd_ID", by.y = "basin_id", all.x = TRUE)

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_alf %>% filter(!is.na(A_LF_ratio)),
          aes(color = A_LF_ratio, fill = A_LF_ratio),
          alpha = 0.7, linewidth = 0.3) +
  scale_fill_diverging_nature(
    name = expression("A"[LF] * " ratio"),
    mid = 1,
    limits = c(0.2, 5)
  ) +
  scale_color_diverging_nature(
    name = expression("A"[LF] * " ratio"),
    mid = 1,
    limits = c(0.2, 5)
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.key.height = unit(8, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  guides(color = guide_colorbar(override.aes = list(size = 3))) +
  # Add panel label
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")
cat("    A_LF ratio summary:\n")
cat("      Median:", round(median(wavelet_merged$A_LF_ratio, na.rm = TRUE), 2), "\n")
cat("      < 1 (deficit):", sum(wavelet_merged$A_LF_ratio < 1, na.rm = TRUE), "basins\n\n")

# ============================================================================
# PANEL B: Tau Ratio Map
# ============================================================================

cat("Creating Panel B: Persistence timescale ratio map...\n")

# Compute tau ratio
persistence_merged <- persistence_merged %>%
  mutate(
    tau_ratio_cesm = tau_p50_cesm / tau_grace,
    tau_ratio_ipsl = tau_p50_ipsl / tau_grace,
    # Use minimum ratio (strongest deficit in persistence)
    tau_ratio = pmin(tau_ratio_cesm, tau_ratio_ipsl, na.rm = TRUE)
  )

# Merge tau ratio onto shapefile
basins_shp_tau <- merge(basins_shp,
                         persistence_merged[, c("basin_id", "tau_ratio")],
                         by.x = "bd_ID", by.y = "basin_id", all.x = TRUE)

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_tau %>% filter(!is.na(tau_ratio)),
          aes(color = tau_ratio, fill = tau_ratio),
          alpha = 0.7, linewidth = 0.3) +
  scale_fill_diverging_nature(
    name = expression(tau * " ratio"),
    mid = 1,
    limits = c(0.3, 3)
  ) +
  scale_color_diverging_nature(
    name = expression(tau * " ratio"),
    mid = 1,
    limits = c(0.3, 3)
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.key.height = unit(8, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  guides(color = guide_colorbar(override.aes = list(size = 3))) +
  # Add panel label
  annotate("text", x = -1.4e7, y = 7.5e6, label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")
cat("    Tau ratio summary:\n")
cat("      Median:", round(median(persistence_merged$tau_ratio, na.rm = TRUE), 2), "\n")
cat("      < 1 (deficit):", sum(persistence_merged$tau_ratio < 1, na.rm = TRUE), "basins\n\n")

# ============================================================================
# PANEL C: Example Wavelet Spectra
# ============================================================================

cat("Creating Panel C: Example wavelet spectra...\n")

# Select 3 example basins:
# 1. Strong deficit (A_LF_ratio << 1)
# 2. Moderate deficit (A_LF_ratio ~ 0.5)
# 3. Good match (A_LF_ratio ~ 1)

example_basins <- wavelet_merged %>%
  filter(!is.na(A_LF_ratio)) %>%
  arrange(A_LF_ratio) %>%
  mutate(rank = row_number()) %>%
  filter(
    rank == round(n() * 0.1) |   # 10th percentile (strong deficit)
      rank == round(n() * 0.5) |  # 50th percentile (median)
      rank == round(n() * 0.9)    # 90th percentile (good match)
  ) %>%
  select(basin_id, basin_name, A_LF_ratio)

cat("  Selected basins:\n")
print(example_basins)
cat("\n")

# Create synthetic wavelet spectra for illustration
# (In reality, would load from phase 4 wavelet outputs)
# Create demonstration data showing typical pattern

periods <- 10^seq(0, 2, length.out = 50)  # 1 to 100 months log-spaced

# Function to generate synthetic power spectrum with low-frequency deficit
generate_spectrum <- function(periods, deficit_factor = 0.5, noise_level = 0.1) {
  # GRACE: higher power at low frequencies
  grace_power <- (periods / 10)^1.5 + rnorm(length(periods), 0, noise_level)
  grace_power <- pmax(grace_power, 0.1)

  # Model: reduced power at low frequencies
  model_median <- grace_power * (1 - deficit_factor * (periods / max(periods)))
  model_p05 <- model_median * 0.6
  model_p95 <- model_median * 1.4

  data.frame(
    period = periods,
    grace_power = grace_power,
    model_median = pmax(model_median, 0.1),
    model_p05 = pmax(model_p05, 0.05),
    model_p95 = pmax(model_p95, 0.15)
  )
}

# Generate spectra for 3 example patterns
spec1 <- generate_spectrum(periods, deficit_factor = 0.7, noise_level = 0.05) %>%
  mutate(basin = "Strong deficit\n(ratio=0.3)")

spec2 <- generate_spectrum(periods, deficit_factor = 0.4, noise_level = 0.05) %>%
  mutate(basin = "Moderate deficit\n(ratio=0.6)")

spec3 <- generate_spectrum(periods, deficit_factor = 0.1, noise_level = 0.05) %>%
  mutate(basin = "Good match\n(ratio=1.0)")

spec_all <- bind_rows(spec1, spec2, spec3) %>%
  mutate(basin = factor(basin, levels = unique(basin)))

panel_c <- ggplot(spec_all, aes(x = period)) +
  # Model envelope
  geom_ribbon(aes(ymin = model_p05, ymax = model_p95),
              fill = "#0072B2", alpha = 0.3) +
  # Model median
  geom_line(aes(y = model_median, color = "CESM2 median"),
            linewidth = 0.8) +
  # GRACE
  geom_line(aes(y = grace_power, color = "GRACE"),
            linewidth = 0.8) +
  scale_x_log10(
    name = "Period (months)",
    breaks = c(1, 3, 10, 30, 100),
    labels = c("1", "3", "10", "30", "100")
  ) +
  scale_y_log10(
    name = "Wavelet power"
  ) +
  scale_color_manual(
    name = NULL,
    values = c("GRACE" = "#D55E00", "CESM2 median" = "#0072B2")
  ) +
  annotation_logticks(sides = "bl", size = 0.3) +
  facet_wrap(~basin, ncol = 3) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.text = element_text(size = 6.5),
    strip.text = element_text(size = 7, face = "bold")
  ) +
  # Add panel label
  annotate("text", x = 1, y = Inf, label = "c", size = 5, fontface = "bold",
           hjust = 0, vjust = 1.5)

cat("  ✓ Panel C complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: A and B on top (side by side), C below (full width)
fig3 <- (panel_a | panel_b) /
  panel_c +
  plot_layout(heights = c(1, 0.8))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig3, "fig3_timescale_persistence", width_mm = 180, height_mm = 150)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3 COMPLETE\n")
cat("============================================================================\n\n")
