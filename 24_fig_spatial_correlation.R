# ============================================================================
# FIGURE 24: Spatial Correlation Structure (GRACE vs Models)
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. How strongly are neighboring basins correlated in GRACE?
#   2. Do models capture observed spatial correlation patterns?
#   3. Which regions show strongest spatial coherence?
#   4. Are model spatial correlations systematically too high or too low?
#
# Panels:
#   (a) Map: GRACE mean neighbor correlation (K=10 nearest basins)
#   (b) Map: CESM2 median neighbor correlation
#   (c) Map: GRACE percentile in CESM2 ensemble
#   (d) Scatter: GRACE vs CESM2/IPSL neighbor correlations
#
# Output:
#   outputs/figs/fig24_spatial_correlation.png (400 dpi)
#   outputs/figs/fig24_spatial_correlation.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 24: SPATIAL CORRELATION STRUCTURE\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load Phase 7 cross-basin correlation results
cat("Loading Phase 7 spatial correlation data...\n")
basin_summary <- readRDS("outputs/phase07_corr_basin_summary.rds")

cat("  Basin summary:", nrow(basin_summary), "basins\n\n")

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
# PANEL A: Map of GRACE Neighbor Correlation
# ============================================================================

cat("Creating Panel A: GRACE neighbor correlation map...\n")

# Merge with shapefile
basins_shp_grace <- merge(basins_shp,
                          basin_summary %>%
                            select(bd_ID = bd_id, mean_neighbor_corr_grace),
                          by = "bd_ID", all.x = TRUE)

# Create discrete categories
basins_shp_grace <- basins_shp_grace %>%
  mutate(
    corr_category = cut(mean_neighbor_corr_grace,
                       breaks = c(-1, 0.2, 0.4, 0.6, 0.8, 1.0),
                       labels = c("<0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", ">0.8"),
                       include.lowest = TRUE)
  )

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_grace %>% filter(!is.na(corr_category)),
          aes(fill = corr_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "GRACE\nneighbor\ncorr.",
    values = c(
      "<0.2" = "#F7FBFF",
      "0.2-0.4" = "#C6DBEF",
      "0.4-0.6" = "#6BAED6",
      "0.6-0.8" = "#2171B5",
      ">0.8" = "#08519C"
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
# PANEL B: Map of CESM2 Median Neighbor Correlation
# ============================================================================

cat("Creating Panel B: CESM2 median neighbor correlation map...\n")

# Merge with shapefile
basins_shp_cesm <- merge(basins_shp,
                         basin_summary %>%
                           select(bd_ID = bd_id, cesm_p50 = mean_neighbor_corr_p50_cesm),
                         by = "bd_ID", all.x = TRUE)

# Create discrete categories
basins_shp_cesm <- basins_shp_cesm %>%
  mutate(
    corr_category = cut(cesm_p50,
                       breaks = c(-1, 0.2, 0.4, 0.6, 0.8, 1.0),
                       labels = c("<0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", ">0.8"),
                       include.lowest = TRUE)
  )

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_cesm %>% filter(!is.na(corr_category)),
          aes(fill = corr_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "CESM2\nmedian\ncorr.",
    values = c(
      "<0.2" = "#F7FBFF",
      "0.2-0.4" = "#C6DBEF",
      "0.4-0.6" = "#6BAED6",
      "0.6-0.8" = "#2171B5",
      ">0.8" = "#08519C"
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

cat("Creating Panel C: GRACE percentile in CESM2...\n")

# Calculate percentiles for CESM2
basin_summary <- basin_summary %>%
  rowwise() %>%
  mutate(
    percentile_cesm = mean(mean_neighbor_corr_grace <= c(mean_neighbor_corr_p05_cesm, mean_neighbor_corr_p50_cesm, mean_neighbor_corr_p95_cesm)) * 100
  ) %>%
  ungroup()

# Merge with shapefile
basins_shp_pct <- merge(basins_shp,
                        basin_summary %>%
                          select(bd_ID = bd_id, percentile_cesm),
                        by = "bd_ID", all.x = TRUE)

# Create discrete percentile categories
basins_shp_pct <- basins_shp_pct %>%
  mutate(
    pct_category = cut(percentile_cesm,
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
# PANEL D: Scatter Plot with Percentile Colors
# ============================================================================

cat("Creating Panel D: GRACE vs models scatter plot...\n")

# Calculate IPSL percentiles
basin_summary <- basin_summary %>%
  rowwise() %>%
  mutate(
    percentile_ipsl = mean(mean_neighbor_corr_grace <= c(mean_neighbor_corr_p05_ipsl, mean_neighbor_corr_p50_ipsl, mean_neighbor_corr_p95_ipsl)) * 100
  ) %>%
  ungroup()

# Prepare data for scatter plot
scatter_data <- basin_summary %>%
  select(basin_id, basin_name,
         grace = mean_neighbor_corr_grace,
         cesm_p05 = mean_neighbor_corr_p05_cesm,
         cesm_p50 = mean_neighbor_corr_p50_cesm,
         cesm_p95 = mean_neighbor_corr_p95_cesm,
         ipsl_p05 = mean_neighbor_corr_p05_ipsl,
         ipsl_p50 = mean_neighbor_corr_p50_ipsl,
         ipsl_p95 = mean_neighbor_corr_p95_ipsl,
         percentile_cesm, percentile_ipsl) %>%
  filter(!is.na(grace))

# Create long format for plotting
scatter_long_cesm <- scatter_data %>%
  mutate(model = "CESM2") %>%
  select(basin_id, basin_name, grace,
         p05 = cesm_p05, p50 = cesm_p50, p95 = cesm_p95,
         percentile = percentile_cesm,
         model) %>%
  mutate(
    pct_category = cut(percentile,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

scatter_long_ipsl <- scatter_data %>%
  mutate(model = "IPSL") %>%
  select(basin_id, basin_name, grace,
         p05 = ipsl_p05, p50 = ipsl_p50, p95 = ipsl_p95,
         percentile = percentile_ipsl,
         model) %>%
  mutate(
    pct_category = cut(percentile,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

scatter_long <- bind_rows(scatter_long_cesm, scatter_long_ipsl)

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
  scale_x_continuous(
    name = "Model median neighbor correlation",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_y_continuous(
    name = "GRACE neighbor correlation",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  annotate("text", x = 0.05, y = 0.95, label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig24 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig24, "fig24_spatial_correlation", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 24 SPATIAL CORRELATION COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig24_spatial_correlation.{png,pdf}\n")
cat("============================================================================\n\n")
