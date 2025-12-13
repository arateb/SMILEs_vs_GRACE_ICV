# ============================================================================
# FIGURE 16: Mode Count Analysis
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. How many oscillation modes are detected per basin in model ensembles?
#   2. Does mode count vary by climate zone?
#   3. Is mode count related to ensemble size or physical characteristics?
#   4. How does mode count distribution differ across period bands?
#
# Panels:
#   (a) Map: Total number of modes detected in CESM2 ensemble (all bands combined)
#   (b) Bar chart: Mean number of modes per period band by climate class (CESM2)
#   (c) Bar chart: Mean number of modes per period band by climate class (IPSL)
#   (d) Scatter: Mode count vs ensemble size (showing saturation)
#
# Output:
#   outputs/figs/fig16_mode_counts.png (400 dpi)
#   outputs/figs/fig16_mode_counts.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 16: MODE COUNT ANALYSIS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading mode count data...\n")
mode_counts_cesm <- readRDS("outputs/phase04_cesm_mode_counts.rds")
mode_counts_ipsl <- readRDS("outputs/phase04_ipsl_mode_counts.rds")

# Load basin attributes for climate classification
attrs <- readRDS("outputs/basin_attributes.rds")

cat("  CESM mode counts:", nrow(mode_counts_cesm), "rows\n")
cat("  IPSL mode counts:", nrow(mode_counts_ipsl), "rows\n\n")

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
# COMPUTE TOTAL MODE COUNTS PER BASIN
# ============================================================================

cat("Computing total mode counts per basin...\n")

# Sum across all bands per basin
total_modes_cesm <- mode_counts_cesm %>%
  group_by(basin_id, basin_name, bd_id) %>%
  summarise(
    total_modes = sum(n_modes, na.rm = TRUE),
    .groups = "drop"
  )

total_modes_ipsl <- mode_counts_ipsl %>%
  group_by(basin_id, basin_name, bd_id) %>%
  summarise(
    total_modes = sum(n_modes, na.rm = TRUE),
    .groups = "drop"
  )

cat("  CESM total modes range:", min(total_modes_cesm$total_modes), "to",
    max(total_modes_cesm$total_modes), "\n")
cat("  IPSL total modes range:", min(total_modes_ipsl$total_modes), "to",
    max(total_modes_ipsl$total_modes), "\n\n")

# ============================================================================
# PANEL A: Map of Total Mode Counts (CESM2)
# ============================================================================

cat("Creating Panel A: Map of total mode counts (CESM2)...\n")

# Rename bd_id to bd_ID to match shapefile
total_modes_cesm <- total_modes_cesm %>%
  rename(bd_ID = bd_id)

basins_shp_modes <- merge(basins_shp,
                           total_modes_cesm[, c("bd_ID", "total_modes")],
                           by = "bd_ID", all.x = TRUE)

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_modes %>% filter(!is.na(total_modes)),
          aes(fill = total_modes),
          color = "black", linewidth = 0.1) +
  scale_fill_viridis_c(
    name = "Total\nmodes",
    option = "D",
    breaks = c(100, 200, 300, 400),
    limits = c(0, 450)
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(10, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Mean Mode Count by Climate and Band (CESM2)
# ============================================================================

cat("Creating Panel B: Mean mode count by climate and band (CESM2)...\n")

# Map climate codes to full names
climate_labels <- c(
  "A" = "Arid",
  "SA" = "Semi-Arid",
  "SH" = "Semi-Humid",
  "H" = "Humid"
)

# Create mapping from bd_ID to basin number
attrs_mapping <- attrs %>%
  mutate(basin_num = match(ID, sort(unique(ID)))) %>%
  select(basin_num, climate)

# Compute mean mode count per climate class and band
mode_climate_cesm <- mode_counts_cesm %>%
  left_join(attrs_mapping, by = c("basin_id" = "basin_num")) %>%
  filter(!is.na(climate)) %>%
  group_by(climate, band) %>%
  summarise(
    mean_modes = mean(n_modes, na.rm = TRUE),
    sd_modes = sd(n_modes, na.rm = TRUE),
    n_basins = n_distinct(basin_id),
    .groups = "drop"
  ) %>%
  mutate(
    climate = factor(climate, levels = c("A", "SA", "SH", "H"),
                     labels = climate_labels),
    band = factor(band, levels = c("ENSO_core", "Quasi-decadal", "Decadal", "Multidecadal"))
  )

panel_b <- ggplot(mode_climate_cesm, aes(x = climate, y = mean_modes, fill = band)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3) +
  scale_fill_manual(
    name = "Period band",
    values = c(
      "ENSO_core" = "#8B0000",         # Dark red
      "Quasi-decadal" = "#CD5C5C",     # Medium red
      "Decadal" = "#009E73",           # Green
      "Multidecadal" = "#CC79A7"       # Purple
    ),
    labels = c(
      "ENSO_core" = "ENSO (2-4yr)",
      "Quasi-decadal" = "Quasi-decadal (4-8yr)",
      "Decadal" = "Decadal (8-30yr)",
      "Multidecadal" = "Multidecadal (>30yr)"
    )
  ) +
  scale_x_discrete(name = "Climate class") +
  scale_y_continuous(name = "Mean number of modes", limits = c(0, 120)) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 0.6, y = 115, label = "b\nCESM2",
           size = 3, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Mean Mode Count by Climate and Band (IPSL)
# ============================================================================

cat("Creating Panel C: Mean mode count by climate and band (IPSL)...\n")

mode_climate_ipsl <- mode_counts_ipsl %>%
  left_join(attrs_mapping, by = c("basin_id" = "basin_num")) %>%
  filter(!is.na(climate)) %>%
  group_by(climate, band) %>%
  summarise(
    mean_modes = mean(n_modes, na.rm = TRUE),
    sd_modes = sd(n_modes, na.rm = TRUE),
    n_basins = n_distinct(basin_id),
    .groups = "drop"
  ) %>%
  mutate(
    climate = factor(climate, levels = c("A", "SA", "SH", "H"),
                     labels = climate_labels),
    band = factor(band, levels = c("ENSO_core", "Quasi-decadal", "Decadal", "Multidecadal"))
  )

panel_c <- ggplot(mode_climate_ipsl, aes(x = climate, y = mean_modes, fill = band)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3) +
  scale_fill_manual(
    name = "Period band",
    values = c(
      "ENSO_core" = "#8B0000",
      "Quasi-decadal" = "#CD5C5C",
      "Decadal" = "#009E73",
      "Multidecadal" = "#CC79A7"
    ),
    labels = c(
      "ENSO_core" = "ENSO (2-4yr)",
      "Quasi-decadal" = "Quasi-decadal (4-8yr)",
      "Decadal" = "Decadal (8-30yr)",
      "Multidecadal" = "Multidecadal (>30yr)"
    )
  ) +
  scale_x_discrete(name = "Climate class") +
  scale_y_continuous(name = "Mean number of modes", limits = c(0, 120)) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 0.6, y = 115, label = "c\nIPSL",
           size = 3, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Mode Count vs Ensemble Size Scatter
# ============================================================================

cat("Creating Panel D: Mode count vs ensemble size...\n")

# Compute total members per basin (should be constant but verify)
members_cesm <- mode_counts_cesm %>%
  group_by(basin_id, basin_name) %>%
  summarise(
    total_members_cesm = first(n_members),
    .groups = "drop"
  )

members_ipsl <- mode_counts_ipsl %>%
  group_by(basin_id, basin_name) %>%
  summarise(
    total_members_ipsl = first(n_members),
    .groups = "drop"
  )

# Merge with total mode counts (basin_name already in total_modes)
modes_vs_size <- total_modes_cesm %>%
  select(basin_id, basin_name, total_modes_cesm = total_modes) %>%
  left_join(members_cesm, by = c("basin_id")) %>%
  left_join(
    total_modes_ipsl %>% select(basin_id, total_modes_ipsl = total_modes),
    by = "basin_id"
  ) %>%
  left_join(members_ipsl %>% select(basin_id, total_members_ipsl), by = "basin_id") %>%
  mutate(
    modes_per_member_cesm = total_modes_cesm / total_members_cesm,
    modes_per_member_ipsl = total_modes_ipsl / total_members_ipsl
  )

# Create combined dataset for plotting
modes_combined <- bind_rows(
  modes_vs_size %>%
    select(basin_id, total_members = total_members_cesm,
           total_modes = total_modes_cesm, modes_per_member = modes_per_member_cesm) %>%
    mutate(model = "CESM2"),
  modes_vs_size %>%
    filter(!is.na(total_members_ipsl)) %>%
    select(basin_id, total_members = total_members_ipsl,
           total_modes = total_modes_ipsl, modes_per_member = modes_per_member_ipsl) %>%
    mutate(model = "IPSL")
)

panel_d <- ggplot(modes_combined, aes(x = total_members, y = total_modes, color = model)) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_manual(
    name = "Model",
    values = c("CESM2" = "#0072B2", "IPSL" = "#009E73")
  ) +
  scale_x_continuous(
    name = "Ensemble size (members)",
    breaks = c(18, 40, 60, 80),
    limits = c(10, 85)
  ) +
  scale_y_continuous(
    name = "Total modes detected",
    limits = c(0, 450)
  ) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 12, y = 430, label = "d",
           size = 5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: A on top (full width), B and C in middle row, D bottom right
fig4 <- panel_a /
  (panel_b | panel_c) /
  (plot_spacer() | panel_d) +
  plot_layout(heights = c(1.2, 1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig4, "fig16_mode_counts", width_mm = 180, height_mm = 200)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 16 MODE COUNTS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig16_mode_counts.{png,pdf}\n")
cat("============================================================================\n\n")
