# ============================================================================
# FIGURE 20: Event Characteristics Maps
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. What is the spatial pattern of extreme pluvial events globally?
#   2. How do drought intensities vary across basins?
#   3. What are the typical event durations in different regions?
#   4. How long does recovery take after extreme events?
#
# Panels:
#   (a) Map: Maximum pluvial intensity (I_max_pluvial_grace) from GRACE
#   (b) Map: Maximum drought intensity (I_max_drought_grace) from GRACE
#   (c) Map: Mean event duration (mean_duration_grace) in months
#   (d) Map: Mean recovery time (mean_recovery_grace) in months
#
# Output:
#   outputs/figs/fig20_event_maps.png (400 dpi)
#   outputs/figs/fig20_event_maps.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 20: EVENT CHARACTERISTICS MAPS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data with corrected volumes (HEIGHT/DEPTH not intensity)
cat("Loading event summary data with corrected volumes...\n")
events <- readRDS("outputs/phase06_event_summary_corrected.rds")

cat("  Event data:", nrow(events), "basins\n\n")

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

# bd_ID already in events data (from volumes file)

# ============================================================================
# PANEL A: Map of Maximum Pluvial Height (weighted by area)
# ============================================================================

cat("Creating Panel A: Map of maximum pluvial height volume...\n")

# Merge with shapefile
basins_shp_pluvial <- merge(basins_shp,
                             events %>%
                               select(bd_ID, V_H_max_grace),
                             by = "bd_ID", all.x = TRUE)

# Create discrete categories (volumes in km³)
basins_shp_pluvial <- basins_shp_pluvial %>%
  mutate(
    pluvial_category = cut(V_H_max_grace,
                          breaks = c(0, 50, 150, 300, 600, 3000),
                          labels = c("<50", "50-150", "150-300", "300-600", ">600"),
                          include.lowest = TRUE)
  )

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_pluvial %>% filter(!is.na(pluvial_category)),
          aes(fill = pluvial_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Pluvial\nheight\n(km³)",
    values = c(
      "<50" = "#C7E9C0",       # Light green
      "50-150" = "#74C476",    # Medium green
      "150-300" = "#31A354",   # Dark green
      "300-600" = "#006D2C",   # Very dark green
      ">600" = "#00441B"       # Darkest green
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
# PANEL B: Map of Maximum Drought Depth (weighted by area)
# ============================================================================

cat("Creating Panel B: Map of maximum drought depth volume...\n")

# Merge with shapefile
basins_shp_drought <- merge(basins_shp,
                             events %>%
                               select(bd_ID, V_D_max_grace),
                             by = "bd_ID", all.x = TRUE)

# Create discrete categories (absolute volumes in km³)
basins_shp_drought <- basins_shp_drought %>%
  mutate(
    drought_category = cut(abs(V_D_max_grace),
                          breaks = c(0, 50, 150, 300, 600, 3000),
                          labels = c("<50", "50-150", "150-300", "300-600", ">600"),
                          include.lowest = TRUE)
  )

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_drought %>% filter(!is.na(drought_category)),
          aes(fill = drought_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Drought\ndepth\n(km³)",
    values = c(
      "<50" = "#FCBBA1",       # Light red
      "50-150" = "#FC9272",    # Medium light red
      "150-300" = "#FB6A4A",   # Medium red
      "300-600" = "#DE2D26",   # Dark red
      ">600" = "#A50F15"       # Very dark red
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
# PANEL C: Map of Mean Event Duration
# ============================================================================

cat("Creating Panel C: Map of mean event duration...\n")

# Merge with shapefile
basins_shp_duration <- merge(basins_shp,
                              events %>%
                                select(bd_ID, mean_duration_grace),
                              by = "bd_ID", all.x = TRUE)

# Create discrete categories
basins_shp_duration <- basins_shp_duration %>%
  mutate(
    duration_category = cut(mean_duration_grace,
                           breaks = c(0, 6, 12, 18, 24, 100),
                           labels = c("<6", "6-12", "12-18", "18-24", ">24"),
                           include.lowest = TRUE)
  )

panel_c <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_duration %>% filter(!is.na(duration_category)),
          aes(fill = duration_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Mean\nduration\n(months)",
    values = c(
      "<6" = "#FFFFCC",      # Very light yellow
      "6-12" = "#C7E9B4",    # Light yellow-green
      "12-18" = "#7FCDBB",   # Light blue-green
      "18-24" = "#41B6C4",   # Medium blue
      ">24" = "#225EA8"      # Dark blue
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
# PANEL D: Map of Mean Recovery Time
# ============================================================================

cat("Creating Panel D: Map of mean recovery time...\n")

# Merge with shapefile
basins_shp_recovery <- merge(basins_shp,
                              events %>%
                                select(bd_ID, mean_recovery_grace),
                              by = "bd_ID", all.x = TRUE)

# Create discrete categories
basins_shp_recovery <- basins_shp_recovery %>%
  mutate(
    recovery_category = cut(mean_recovery_grace,
                           breaks = c(0, 6, 12, 18, 24, 100),
                           labels = c("<6", "6-12", "12-18", "18-24", ">24"),
                           include.lowest = TRUE)
  )

panel_d <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_recovery %>% filter(!is.na(recovery_category)),
          aes(fill = recovery_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Mean\nrecovery\n(months)",
    values = c(
      "<6" = "#FEE5D9",      # Very light orange
      "6-12" = "#FCBBA1",    # Light orange
      "12-18" = "#FC9272",   # Medium orange
      "18-24" = "#FB6A4A",   # Dark orange
      ">24" = "#CB181D"      # Very dark orange
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
  annotate("text", x = -1.4e7, y = 7.5e6, label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig20 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig20, "fig20_event_maps", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 20 EVENT MAPS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig20_event_maps.{png,pdf}\n")
cat("============================================================================\n\n")
