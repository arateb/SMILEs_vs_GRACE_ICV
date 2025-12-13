# ============================================================================
# FIGURE 21: Event Intensity Comparison (GRACE vs Models)
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. How do GRACE-observed event intensities compare to model ensembles?
#   2. Are GRACE events within model envelopes or exceeding them?
#   3. How do event volumes differ spatially?
#
# Panels:
#   (a) Map: GRACE pluvial event volume (km³)
#   (b) Map: CESM2 median pluvial volume (km³)
#   (c) Map: GRACE pluvial percentile in CESM2 ensemble
#   (d) Scatter: GRACE vs CESM2/IPSL pluvial volumes with error bars
#
# Output:
#   outputs/figs/fig21_event_comparison.png (400 dpi)
#   outputs/figs/fig21_event_comparison.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 21: EVENT INTENSITY COMPARISON\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data with volumes
cat("Loading event data with volumes...\n")
events <- readRDS("outputs/phase06_event_summary_corrected.rds")

cat("  Event data:", nrow(events), "basins\n\n")

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
# PANEL A: Map of GRACE Pluvial Volume
# ============================================================================

cat("Creating Panel A: GRACE pluvial event volumes...\n")

# Merge with shapefile
basins_shp_grace <- merge(basins_shp,
                          events %>%
                            select(bd_ID, V_H_max_grace),
                          by = "bd_ID", all.x = TRUE)

# Create discrete categories for volumes (km³)
basins_shp_grace <- basins_shp_grace %>%
  mutate(
    volume_category = cut(V_H_max_grace,
                         breaks = c(0, 50, 150, 300, 600, 3000),
                         labels = c("<50", "50-150", "150-300", "300-600", ">600"),
                         include.lowest = TRUE)
  )

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_grace %>% filter(!is.na(volume_category)),
          aes(fill = volume_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "GRACE\npluvial\nheight\n(km³)",
    values = c(
      "<50" = "#C7E9C0",
      "50-150" = "#74C476",
      "150-300" = "#31A354",
      "300-600" = "#006D2C",
      ">600" = "#00441B"
    ),
    drop = FALSE
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Map of CESM2 Median Pluvial Volume
# ============================================================================

cat("Creating Panel B: CESM2 median pluvial volumes...\n")

# Merge with shapefile
basins_shp_cesm <- merge(basins_shp,
                         events %>%
                           select(bd_ID, V_H_p50_cesm),
                         by = "bd_ID", all.x = TRUE)

# Create discrete categories
basins_shp_cesm <- basins_shp_cesm %>%
  mutate(
    volume_category = cut(V_H_p50_cesm,
                         breaks = c(0, 50, 150, 300, 600, 3000),
                         labels = c("<50", "50-150", "150-300", "300-600", ">600"),
                         include.lowest = TRUE)
  )

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_cesm %>% filter(!is.na(volume_category)),
          aes(fill = volume_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "CESM2\nmedian\nheight\n(km³)",
    values = c(
      "<50" = "#C7E9C0",
      "50-150" = "#74C476",
      "150-300" = "#31A354",
      "300-600" = "#006D2C",
      ">600" = "#00441B"
    ),
    drop = FALSE
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Map of GRACE Percentile in CESM2 Ensemble
# ============================================================================

cat("Creating Panel C: GRACE pluvial percentile in CESM2...\n")

# Merge with shapefile
basins_shp_pct <- merge(basins_shp,
                        events %>%
                          select(bd_ID, percentile_H_cesm),
                        by = "bd_ID", all.x = TRUE)

# Create discrete percentile categories
basins_shp_pct <- basins_shp_pct %>%
  mutate(
    pct_category = cut(percentile_H_cesm,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

panel_c <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_pct %>% filter(!is.na(pct_category)),
          aes(fill = pct_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "GRACE\npercentile\nin CESM2",
    values = c(
      "<5%" = "#B8B8B8",      # Light gray - models overestimate
      "5-25%" = "#C6DBEF",    # Light blue (transition)
      "25-75%" = "#87CEEB",   # Sky blue - within envelope
      "75-95%" = "#6BAED6",   # Medium blue (transition)
      ">95%" = "#505050"      # Dark gray - models underestimate
    ),
    drop = FALSE
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "c", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Scatter Plot with Error Bars
# ============================================================================

cat("Creating Panel D: GRACE vs models scatter plot...\n")

# Prepare data for scatter plot
scatter_data <- events %>%
  select(basin_id, basin_name,
         V_H_max_grace,
         V_H_p05_cesm, V_H_p50_cesm, V_H_p95_cesm,
         V_H_p05_ipsl, V_H_p50_ipsl, V_H_p95_ipsl,
         percentile_H_cesm, percentile_H_ipsl) %>%
  filter(!is.na(V_H_max_grace))

# Create long format for plotting
scatter_long_cesm <- scatter_data %>%
  mutate(model = "CESM2") %>%
  select(basin_id, basin_name, grace = V_H_max_grace,
         p05 = V_H_p05_cesm, p50 = V_H_p50_cesm, p95 = V_H_p95_cesm,
         percentile = percentile_H_cesm,
         model) %>%
  mutate(
    pct_category = cut(percentile,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

scatter_long_ipsl <- scatter_data %>%
  mutate(model = "IPSL") %>%
  select(basin_id, basin_name, grace = V_H_max_grace,
         p05 = V_H_p05_ipsl, p50 = V_H_p50_ipsl, p95 = V_H_p95_ipsl,
         percentile = percentile_H_ipsl,
         model) %>%
  mutate(
    pct_category = cut(percentile,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

scatter_long <- bind_rows(scatter_long_cesm, scatter_long_ipsl)

# Calculate 1:1 line range
max_val <- max(c(scatter_long$grace, scatter_long$p95), na.rm = TRUE)

panel_d <- ggplot(scatter_long, aes(x = p50, y = grace)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  # Points colored by percentile
  geom_point(aes(fill = pct_category), size = 2.5, alpha = 0.8, shape = 21, color = "black", stroke = 0.2) +
  scale_fill_manual(
    name = "GRACE\npercentile",
    values = c(
      "<5%" = "#B8B8B8",
      "5-25%" = "#C6DBEF",
      "25-75%" = "#87CEEB",
      "75-95%" = "#6BAED6",
      ">95%" = "#505050"
    ),
    drop = FALSE
  ) +
  scale_x_log10(
    name = "Model median pluvial height (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  scale_y_log10(
    name = "GRACE pluvial height (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  annotation_logticks(sides = "bl", size = 0.25, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  annotate("text", x = 10^(log10(10) + 0.1), y = 10^(log10(max_val) - 0.1),
           label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig21 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig21, "fig21_event_comparison", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 21 EVENT COMPARISON COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig21_event_comparison.{png,pdf}\n")
cat("============================================================================\n\n")
