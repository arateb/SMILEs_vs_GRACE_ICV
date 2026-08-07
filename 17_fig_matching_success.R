# ============================================================================
# FIGURE 17: Period and Power Matching Success Analysis
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. How often do ensemble members match GRACE oscillation periods?
#   2. When matched, how accurate are period and power estimates?
#   3. Does matching success vary by climate zone or period band?
#   4. What is the distribution of period/power biases across the ensemble?
#
# Panels:
#   (a) Map: Matching success rate (% members matching GRACE periods) - CESM2
#   (b) Histogram: Period difference distribution (model - GRACE) when matched
#   (c) Histogram: Power ratio distribution (model / GRACE) when matched
#   (d) Bar chart: Matching success rate by climate class and period band
#
# Output:
#   outputs/figs/fig17_matching_success.png (400 dpi)
#   outputs/figs/fig17_matching_success.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 17: PERIOD/POWER MATCHING SUCCESS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading member matching data...\n")
matches_cesm <- readRDS("outputs/phase04_cesm_member_matches.rds")
matches_ipsl <- readRDS("outputs/phase04_ipsl_member_matches.rds")

# Load basin attributes
attrs <- readRDS("outputs/basin_attributes.rds")

cat("  CESM matches:", nrow(matches_cesm), "rows\n")
cat("  IPSL matches:", nrow(matches_ipsl), "rows\n\n")

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

# ============================================================================
# COMPUTE MATCHING SUCCESS RATES PER BASIN
# ============================================================================

cat("Computing matching success rates...\n")

# Success rate = % of (member x GRACE_mode) combinations where matched=TRUE
success_cesm <- matches_cesm %>%
  group_by(basin_id, basin_name, bd_id) %>%
  summarise(
    total_comparisons = n(),
    total_matches = sum(matched, na.rm = TRUE),
    success_rate = 100 * total_matches / total_comparisons,
    .groups = "drop"
  ) %>%
  rename(bd_ID = bd_id)

success_ipsl <- matches_ipsl %>%
  group_by(basin_id, basin_name, bd_id) %>%
  summarise(
    total_comparisons = n(),
    total_matches = sum(matched, na.rm = TRUE),
    success_rate = 100 * total_matches / total_comparisons,
    .groups = "drop"
  ) %>%
  rename(bd_ID = bd_id)

cat("  Success rate range (CESM):", round(min(success_cesm$success_rate), 1), "to",
    round(max(success_cesm$success_rate), 1), "%\n")
cat("  Success rate range (IPSL):", round(min(success_ipsl$success_rate), 1), "to",
    round(max(success_ipsl$success_rate), 1), "%\n\n")

# ============================================================================
# PANEL A: Map of Matching Success Rate (CESM2)
# ============================================================================

cat("Creating Panel A: Map of matching success rate (CESM2)...\n")

basins_shp_success <- merge(basins_shp,
                             success_cesm[, c("bd_ID", "success_rate")],
                             by = "bd_ID", all.x = TRUE)

# Create discrete success rate categories
basins_shp_success <- basins_shp_success %>%
  mutate(
    success_category = cut(success_rate,
                          breaks = c(0, 20, 40, 60, 80, 100),
                          labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                          include.lowest = TRUE)
  )

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_success %>% filter(!is.na(success_category)),
          aes(fill = success_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Match\nsuccess",
    values = c(
      "0-20%" = "#D73027",    # Red
      "20-40%" = "#FC8D59",   # Orange
      "40-60%" = "#FEE08B",   # Yellow
      "60-80%" = "#91CF60",   # Light green
      "80-100%" = "#1A9850"   # Dark green
    ),
    drop = FALSE
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
# PANEL B: Period Difference Histogram
# ============================================================================

cat("Creating Panel B: Period difference histogram...\n")

# Only use matched cases
matched_only_cesm <- matches_cesm %>%
  filter(matched == TRUE & !is.na(period_diff))

matched_only_ipsl <- matches_ipsl %>%
  filter(matched == TRUE & !is.na(period_diff))

period_diffs <- bind_rows(
  matched_only_cesm %>% select(period_diff) %>% mutate(model = "CESM2"),
  matched_only_ipsl %>% select(period_diff) %>% mutate(model = "IPSL")
)

cat("  Period diff range:", round(min(period_diffs$period_diff, na.rm=TRUE), 2), "to",
    round(max(period_diffs$period_diff, na.rm=TRUE), 2), "yr\n")
cat("  Median (CESM):", round(median(matched_only_cesm$period_diff, na.rm=TRUE), 2), "yr\n")
cat("  Median (IPSL):", round(median(matched_only_ipsl$period_diff, na.rm=TRUE), 2), "yr\n")

panel_b <- ggplot(period_diffs, aes(x = period_diff, fill = model)) +
  geom_density(alpha = 0.6, linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.5) +
  scale_fill_manual(
    name = "Model",
    values = c("CESM2" = "#0072B2", "IPSL" = "#009E73")
  ) +
  scale_x_continuous(
    name = "Period difference (model - GRACE, years)",
    limits = c(-10, 10),
    breaks = seq(-10, 10, 5)
  ) +
  scale_y_continuous(name = "Density") +
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
  annotate("text", x = -9.5, y = Inf, label = "b",
           size = 5, fontface = "bold", hjust = 0, vjust = 1.5)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Power Ratio Histogram
# ============================================================================

cat("Creating Panel C: Power ratio histogram...\n")

power_ratios <- bind_rows(
  matched_only_cesm %>%
    filter(!is.na(power_ratio) & power_ratio > 0) %>%
    select(power_ratio) %>%
    mutate(model = "CESM2"),
  matched_only_ipsl %>%
    filter(!is.na(power_ratio) & power_ratio > 0) %>%
    select(power_ratio) %>%
    mutate(model = "IPSL")
)

cat("  Power ratio range:", round(min(power_ratios$power_ratio, na.rm=TRUE), 2), "to",
    round(max(power_ratios$power_ratio, na.rm=TRUE), 2), "\n")
cat("  Median (CESM):",
    round(median(matched_only_cesm$power_ratio[matched_only_cesm$power_ratio > 0], na.rm=TRUE), 2), "\n")
cat("  Median (IPSL):",
    round(median(matched_only_ipsl$power_ratio[matched_only_ipsl$power_ratio > 0], na.rm=TRUE), 2), "\n")

panel_c <- ggplot(power_ratios, aes(x = power_ratio, fill = model)) +
  geom_density(alpha = 0.6, linewidth = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.5) +
  scale_fill_manual(
    name = "Model",
    values = c("CESM2" = "#0072B2", "IPSL" = "#009E73")
  ) +
  scale_x_log10(
    name = "Power ratio (model / GRACE)",
    breaks = c(0.3, 0.5, 1, 2, 3),
    limits = c(0.2, 5)
  ) +
  scale_y_continuous(name = "Density") +
  annotation_logticks(sides = "b", size = 0.3, color = "grey50") +
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
  annotate("text", x = 0.22, y = Inf, label = "c",
           size = 5, fontface = "bold", hjust = 0, vjust = 1.5)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Success Rate by Climate and Band
# ============================================================================

cat("Creating Panel D: Success rate by climate and band...\n")

# Map climate codes
climate_labels <- c(
  "A" = "Arid",
  "SA" = "Semi-Arid",
  "SH" = "Semi-Humid",
  "H" = "Humid"
)

# Create basin to climate mapping
attrs_mapping <- attrs %>%
  mutate(basin_num = match(ID, sort(unique(ID)))) %>%
  select(basin_num, climate)

# Compute success rate by climate and band
success_climate_cesm <- matches_cesm %>%
  filter(!is.na(grace_band)) %>%
  left_join(attrs_mapping, by = c("basin_id" = "basin_num")) %>%
  filter(!is.na(climate)) %>%
  group_by(climate, grace_band) %>%
  summarise(
    total_comparisons = n(),
    total_matches = sum(matched, na.rm = TRUE),
    success_rate = 100 * total_matches / total_comparisons,
    .groups = "drop"
  ) %>%
  mutate(
    climate = factor(climate, levels = c("A", "SA", "SH", "H"),
                     labels = climate_labels),
    grace_band = factor(grace_band, levels = c("ENSO_core", "Quasi-decadal", "Decadal", "Multidecadal"))
  )

success_climate_ipsl <- matches_ipsl %>%
  filter(!is.na(grace_band)) %>%
  left_join(attrs_mapping, by = c("basin_id" = "basin_num")) %>%
  filter(!is.na(climate)) %>%
  group_by(climate, grace_band) %>%
  summarise(
    total_comparisons = n(),
    total_matches = sum(matched, na.rm = TRUE),
    success_rate = 100 * total_matches / total_comparisons,
    .groups = "drop"
  ) %>%
  mutate(
    climate = factor(climate, levels = c("A", "SA", "SH", "H"),
                     labels = climate_labels),
    grace_band = factor(grace_band, levels = c("ENSO_core", "Quasi-decadal", "Decadal", "Multidecadal"))
  )

# Combine models
success_combined <- bind_rows(
  success_climate_cesm %>% mutate(model = "CESM2"),
  success_climate_ipsl %>% mutate(model = "IPSL")
)

panel_d <- ggplot(success_combined, aes(x = climate, y = success_rate, fill = grace_band)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3) +
  facet_wrap(~ model, ncol = 1) +
  scale_fill_manual(
    name = "GRACE\nperiod band",
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
  scale_y_continuous(name = "Match success rate (%)", limits = c(0, 100)) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.3),
    strip.text = element_text(size = 7, face = "bold")
  ) +
  annotate("text", x = 0.6, y = 97, label = "d",
           size = 5, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: A on top (full width), B and C in middle row, D bottom
fig5 <- panel_a /
  (panel_b | panel_c) /
  panel_d +
  plot_layout(heights = c(1.2, 1, 1.2))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig5, "fig17_matching_success", width_mm = 180, height_mm = 200)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 17 MATCHING SUCCESS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig17_matching_success.{png,pdf}\n")
cat("============================================================================\n\n")
