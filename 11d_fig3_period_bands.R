# ============================================================================
# FIGURE 3: Period Band Distribution and Power Matching
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. What period bands (ENSO, quasi-decadal) dominate GRACE TWS globally?
#   2. Do models reproduce the observed distribution of period bands?
#   3. When period bands match, do models get the power magnitude correct?
#
# Panels:
#   (a) Map: GRACE dominant period bands (ENSO_core, Quasi-decadal, etc.)
#   (b) Histogram: Period distribution - GRACE vs all CESM/IPSL members
#   (c) Scatter: Power matching for same band class (CESM)
#   (d) Scatter: Power matching for same band class (IPSL)
#
# Period bands:
#   - ENSO_core: 2-4 years
#   - Quasi-decadal: 4-8 years
#   - Decadal: 8-30 years (models only - GRACE too short)
#   - Multidecadal: >30 years (models only)
#
# Output:
#   outputs/figs/fig3_period_bands.png (400 dpi)
#   outputs/figs/fig3_period_bands.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3: PERIOD BAND DISTRIBUTION AND POWER MATCHING\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading wavelet data...\n")
grace_w <- readRDS("outputs/phase04_grace_wavelets.rds")
cesm_w <- readRDS("outputs/phase04_cesm_all_wavelets.rds")
ipsl_w <- readRDS("outputs/phase04_ipsl_all_wavelets.rds")

# Load basin attributes for bd_ID mapping
attrs <- readRDS("outputs/basin_attributes.rds")
attrs_lookup <- data.frame(
  basin = 1:nrow(attrs),
  bd_ID = attrs$ID
)

# Add bd_ID to GRACE data
grace_w <- grace_w %>%
  left_join(attrs_lookup, by = "basin")

cat("  GRACE wavelets:", nrow(grace_w), "basins\n")
cat("  CESM wavelets:", nrow(cesm_w), "rows\n")
cat("  IPSL wavelets:", nrow(ipsl_w), "rows\n\n")

# Load basin shapefile
basins_shp <- st_read("/Volumes/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)
basins_shp <- st_simplify(basins_shp, dTolerance = 0.1, preserveTopology = TRUE)

# Load country borders
sf::sf_use_s2(FALSE)
world_borders <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
sf::sf_use_s2(TRUE)

# ============================================================================
# PANEL A: Map of GRACE Dominant Period Bands
# ============================================================================

cat("Creating Panel A: Map of GRACE dominant period bands...\n")

# Merge GRACE band class onto shapefile
basins_shp_period <- merge(basins_shp,
                            grace_w[, c("bd_ID", "band_class_1")],
                            by = "bd_ID", all.x = TRUE)

# Ensure proper factor levels
basins_shp_period$band_class_1 <- factor(
  basins_shp_period$band_class_1,
  levels = c("ENSO_core", "Quasi-decadal", "Decadal", "Multidecadal")
)

cat("  Band class counts:\n")
print(table(basins_shp_period$band_class_1))

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_period %>% filter(!is.na(band_class_1)),
          aes(fill = band_class_1),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Period\nband",
    values = c(
      "ENSO_core" = "#8B0000",      # Dark red - 2-4yr
      "Quasi-decadal" = "#CD5C5C",  # Medium red - 4-8yr
      "Decadal" = "#009E73",        # Green - 8-30yr
      "Multidecadal" = "#CC79A7"    # Purple - >30yr
    ),
    labels = c(
      "ENSO_core" = "ENSO (2-4yr)",
      "Quasi-decadal" = "Quasi-decadal (4-8yr)",
      "Decadal" = "Decadal (8-30yr)",
      "Multidecadal" = "Multidecadal (>30yr)"
    ),
    drop = FALSE
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.15),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Period Distribution Histogram (ALL periods, all members, all basins)
# ============================================================================

cat("Creating Panel B: Period distribution histogram...\n")

# Prepare period data - ALL members, ALL basins
period_data <- bind_rows(
  grace_w %>%
    filter(!is.na(dominant_period_1)) %>%
    select(period = dominant_period_1) %>%
    mutate(source = "GRACE"),
  cesm_w %>%
    filter(!is.na(period_1)) %>%
    select(period = period_1) %>%
    mutate(source = "CESM2"),
  ipsl_w %>%
    filter(!is.na(period_1)) %>%
    select(period = period_1) %>%
    mutate(source = "IPSL")
)

# Summary stats
cat("  Period summary:\n")
cat("    GRACE: n=", sum(period_data$source == "GRACE"),
    ", median=", round(median(period_data$period[period_data$source == "GRACE"], na.rm=TRUE), 2), "yr\n", sep="")
cat("    CESM2: n=", sum(period_data$source == "CESM2"),
    ", median=", round(median(period_data$period[period_data$source == "CESM2"], na.rm=TRUE), 2), "yr\n", sep="")
cat("    IPSL: n=", sum(period_data$source == "IPSL"),
    ", median=", round(median(period_data$period[period_data$source == "IPSL"], na.rm=TRUE), 2), "yr\n", sep="")

panel_b <- ggplot(period_data, aes(x = period, fill = source)) +
  geom_density(alpha = 0.5, linewidth = 0.6) +
  scale_fill_manual(
    name = "Dataset",
    values = c("GRACE" = "#E69F00", "CESM2" = "#0072B2", "IPSL" = "#FF6B6B"),
    labels = c("GRACE" = "GRACE (obs)", "CESM2" = "CESM2", "IPSL" = "IPSL")
  ) +
  scale_x_continuous(
    name = "Dominant period (years)",
    breaks = c(2, 4, 6, 8, 10, 12, 14),
    limits = c(1.5, 15)
  ) +
  scale_y_continuous(name = "Density") +
  # Add vertical lines for band boundaries
  geom_vline(xintercept = c(4, 8), linetype = "dashed", color = "grey50", linewidth = 0.3) +
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
  annotate("text", x = 1.6, y = Inf, label = "b",
           size = 5, fontface = "bold", hjust = 0, vjust = 1.5)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Power Matching Scatter (CESM2) - Same Band Class
# ============================================================================

cat("Creating Panel C: Power matching scatter (CESM2)...\n")

# Prepare GRACE data with band class
grace_band_class <- grace_w %>%
  filter(!is.na(band_class_1) & !is.na(dominant_power_1)) %>%
  mutate(grace_power = dominant_power_1) %>%
  select(basin, basin_name, band_class = band_class_1, grace_power)

# Prepare CESM data - compute median power per basin per band class
cesm_band_class <- cesm_w %>%
  filter(!is.na(band_1) & !is.na(power_1)) %>%
  group_by(basin, basin_name, band_class = band_1) %>%
  summarise(
    power_p50 = median(power_1, na.rm = TRUE),
    power_p05 = quantile(power_1, 0.05, na.rm = TRUE),
    power_p95 = quantile(power_1, 0.95, na.rm = TRUE),
    n_members = n(),
    .groups = "drop"
  )

# Match GRACE and CESM for same band class
matching_cesm <- grace_band_class %>%
  inner_join(cesm_band_class, by = c("basin", "basin_name", "band_class")) %>%
  filter(!is.na(grace_power) & !is.na(power_p50)) %>%
  mutate(
    power_direction = case_when(
      grace_power > power_p95 ~ 'above_p95',
      grace_power < power_p05 ~ 'below_p05',
      TRUE ~ 'within'
    )
  )

cat("  Matched basins (CESM):", nrow(matching_cesm), "\n")
cat("  Band class counts:\n")
print(table(matching_cesm$band_class))
cat("  Power dispersion counts:\n")
print(table(matching_cesm$power_direction))

panel_c <- ggplot(matching_cesm, aes(x = power_p50, y = grace_power, fill = power_direction)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = power_p05, xmax = power_p95, color = power_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_continuous(
    name = "CESM2 power (mm², median)",
    limits = c(0, 10)
  ) +
  scale_y_continuous(
    name = "GRACE power (mm²)",
    limits = c(0, 10)
  ) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 3, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 0.2, y = 9.5,
           label = sprintf("c\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          sum(matching_cesm$power_direction == "within"),
                          sum(matching_cesm$power_direction == "above_p95"),
                          sum(matching_cesm$power_direction == "below_p05")),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Power Matching Scatter (IPSL) - Same Band Class
# ============================================================================

cat("Creating Panel D: Power matching scatter (IPSL)...\n")

# Prepare IPSL data - compute median power per basin per band class
ipsl_band_class <- ipsl_w %>%
  filter(!is.na(band_1) & !is.na(power_1)) %>%
  group_by(basin, basin_name, band_class = band_1) %>%
  summarise(
    power_p50 = median(power_1, na.rm = TRUE),
    power_p05 = quantile(power_1, 0.05, na.rm = TRUE),
    power_p95 = quantile(power_1, 0.95, na.rm = TRUE),
    n_members = n(),
    .groups = "drop"
  )

# Match GRACE and IPSL for same band class
matching_ipsl <- grace_band_class %>%
  inner_join(ipsl_band_class, by = c("basin", "basin_name", "band_class")) %>%
  filter(!is.na(grace_power) & !is.na(power_p50)) %>%
  mutate(
    power_direction = case_when(
      grace_power > power_p95 ~ 'above_p95',
      grace_power < power_p05 ~ 'below_p05',
      TRUE ~ 'within'
    )
  )

cat("  Matched basins (IPSL):", nrow(matching_ipsl), "\n")
cat("  Band class counts:\n")
print(table(matching_ipsl$band_class))
cat("  Power dispersion counts:\n")
print(table(matching_ipsl$power_direction))

panel_d <- ggplot(matching_ipsl, aes(x = power_p50, y = grace_power, fill = power_direction)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = power_p05, xmax = power_p95, color = power_direction),
                 alpha = 0.4, linewidth = 0.4, height = 0) +
  geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_directional(name = "Dispersion") +
  scale_color_directional(name = "Dispersion") +
  scale_x_continuous(
    name = "IPSL power (mm², median)",
    limits = c(0, 10)
  ) +
  scale_y_continuous(
    name = "GRACE power (mm²)",
    limits = c(0, 10)
  ) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 3, alpha = 1)),
    color = "none"
  ) +
  annotate("text", x = 0.2, y = 9.5,
           label = sprintf("d\nn=%d (within)\nn=%d (above p95)\nn=%d (below p05)",
                          sum(matching_ipsl$power_direction == "within"),
                          sum(matching_ipsl$power_direction == "above_p95"),
                          sum(matching_ipsl$power_direction == "below_p05")),
           size = 2.5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: A on top left, B on top right, C and D below
fig3 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1.2, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig3, "fig3_period_bands", width_mm = 180, height_mm = 150)

# ============================================================================
# EXPORT DATA TO CSV
# ============================================================================

cat("Exporting figure data to CSV...\n")

dir.create("outputs/figure_data", showWarnings = FALSE, recursive = TRUE)

# Panel A data (map)
panel_a_data <- grace_w %>%
  select(basin_id = bd_ID, basin_name,
         dominant_period = dominant_period_1,
         dominant_power = dominant_power_1,
         band_class = band_class_1)

write.csv(panel_a_data, "outputs/figure_data/fig3a_grace_period_bands.csv", row.names = FALSE)

# Panel B data (period distributions)
write.csv(period_data, "outputs/figure_data/fig3b_period_distributions.csv", row.names = FALSE)

# Panel C data (CESM2 power matching)
panel_c_data <- matching_cesm %>%
  select(basin, basin_name, band_class,
         grace_power,
         cesm_power_p05 = power_p05,
         cesm_power_p50 = power_p50,
         cesm_power_p95 = power_p95,
         n_members)

write.csv(panel_c_data, "outputs/figure_data/fig3c_cesm_power_matching.csv", row.names = FALSE)

# Panel D data (IPSL power matching)
panel_d_data <- matching_ipsl %>%
  select(basin, basin_name, band_class,
         grace_power,
         ipsl_power_p05 = power_p05,
         ipsl_power_p50 = power_p50,
         ipsl_power_p95 = power_p95,
         n_members)

write.csv(panel_d_data, "outputs/figure_data/fig3d_ipsl_power_matching.csv", row.names = FALSE)

cat("  ✓ Exported: outputs/figure_data/fig3{a,b,c,d}_*.csv\n\n")

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3 PERIOD BANDS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig3_period_bands.{png,pdf}\n")
cat("  Data: outputs/figure_data/fig3{a,b,c,d}_*.csv\n")
cat("============================================================================\n\n")
