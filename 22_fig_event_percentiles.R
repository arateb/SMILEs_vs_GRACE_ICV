# ============================================================================
# FIGURE 22: Event Percentiles (GRACE in Both Ensembles)
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. Where does GRACE rank in CESM2 ensemble?
#   2. Where does GRACE rank in IPSL ensemble?
#   3. Do models systematically over/underestimate event intensities?
#
# Panels:
#   (a) Map: GRACE pluvial percentile in CESM2
#   (b) Map: GRACE pluvial percentile in IPSL
#   (c) Map: GRACE drought percentile in CESM2
#   (d) Map: GRACE drought percentile in IPSL
#
# Output:
#   outputs/figs/fig22_event_percentiles.png (400 dpi)
#   outputs/figs/fig22_event_percentiles.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 22: EVENT PERCENTILES (SYMMETRIC CESM2/IPSL)\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data with volumes
cat("Loading event data...\n")
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
# PANEL A: GRACE Pluvial Percentile in CESM2
# ============================================================================

cat("Creating Panel A: GRACE pluvial percentile in CESM2...\n")

basins_shp_pct_cesm <- merge(basins_shp,
                              events %>%
                                select(bd_ID, percentile_H_cesm),
                              by = "bd_ID", all.x = TRUE)

basins_shp_pct_cesm <- basins_shp_pct_cesm %>%
  mutate(
    pct_category = cut(percentile_H_cesm,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_pct_cesm %>% filter(!is.na(pct_category)),
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
  labs(title = "PLUVIAL (wet events, positive anomalies)") +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: GRACE Pluvial Percentile in IPSL
# ============================================================================

cat("Creating Panel B: GRACE pluvial percentile in IPSL...\n")

basins_shp_pct_ipsl <- merge(basins_shp,
                              events %>%
                                select(bd_ID, percentile_H_ipsl),
                              by = "bd_ID", all.x = TRUE)

basins_shp_pct_ipsl <- basins_shp_pct_ipsl %>%
  mutate(
    pct_category = cut(percentile_H_ipsl,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_pct_ipsl %>% filter(!is.na(pct_category)),
          aes(fill = pct_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "GRACE\npercentile\nin IPSL",
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
  labs(title = "PLUVIAL (wet events, positive anomalies)") +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: GRACE Drought Percentile in CESM2
# ============================================================================

cat("Creating Panel C: GRACE drought percentile in CESM2...\n")

basins_shp_drought_cesm <- merge(basins_shp,
                                  events %>%
                                    select(bd_ID, percentile_D_cesm),
                                  by = "bd_ID", all.x = TRUE)

basins_shp_drought_cesm <- basins_shp_drought_cesm %>%
  mutate(
    pct_category = cut(percentile_D_cesm,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

panel_c <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_drought_cesm %>% filter(!is.na(pct_category)),
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
  labs(title = "DROUGHT (dry events, negative anomalies)") +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "c", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: GRACE Drought Percentile in IPSL
# ============================================================================

cat("Creating Panel D: GRACE drought percentile in IPSL...\n")

basins_shp_drought_ipsl <- merge(basins_shp,
                                  events %>%
                                    select(bd_ID, percentile_D_ipsl),
                                  by = "bd_ID", all.x = TRUE)

basins_shp_drought_ipsl <- basins_shp_drought_ipsl %>%
  mutate(
    pct_category = cut(percentile_D_ipsl,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

panel_d <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_drought_ipsl %>% filter(!is.na(pct_category)),
          aes(fill = pct_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "GRACE\npercentile\nin IPSL",
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
  labs(title = "DROUGHT (dry events, negative anomalies)") +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig22 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig22, "fig22_event_percentiles", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 22 EVENT PERCENTILES COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig22_event_percentiles.{png,pdf}\n")
cat("============================================================================\n\n")
