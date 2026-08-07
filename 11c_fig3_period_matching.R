# ============================================================================
# FIGURE 3: Period Distribution and Power Matching
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. What dominant periods characterize GRACE TWS variability globally?
#   2. Do models reproduce the observed distribution of dominant periods?
#   3. When periods match, do models get the power magnitude correct?
#
# Panels:
#   (a) Map: GRACE dominant periods (continuous color scale by period in years)
#   (b) Histogram: Period distribution - GRACE vs all CESM/IPSL members
#   (c) Scatter: Power matching for same period class (CESM)
#   (d) Scatter: Power matching for same period class (IPSL)
#
# Output:
#   outputs/figs/fig3_period_matching.png (400 dpi)
#   outputs/figs/fig3_period_matching.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3: PERIOD DISTRIBUTION AND POWER MATCHING\n")
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
# PANEL A: Map of GRACE Dominant Periods
# ============================================================================

cat("Creating Panel A: Map of GRACE dominant periods...\n")

# Merge GRACE period onto shapefile
basins_shp_period <- merge(basins_shp,
                            grace_w[, c("bd_ID", "dominant_period_1")],
                            by = "bd_ID", all.x = TRUE)

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_period %>% filter(!is.na(dominant_period_1)),
          aes(fill = dominant_period_1),
          color = "black", linewidth = 0.1) +
  scale_fill_gradientn(
    name = "Period\n(years)",
    colors = c("#8B0000", "#CD5C5C", "#F08080"),  # Dark red to light red gradient
    breaks = c(2, 3, 4, 5, 6, 7),
    labels = c("2", "3", "4", "5", "6", "7"),
    limits = c(2, 7)
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
# PANEL B: Period Distribution Histogram
# ============================================================================

cat("Creating Panel B: Period distribution histogram...\n")

# Prepare period data
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
  geom_density(alpha = 0.6, linewidth = 0.5) +
  scale_fill_manual(
    name = "Dataset",
    values = c("GRACE" = "#E69F00", "CESM2" = "#0072B2", "IPSL" = "#009E73"),
    labels = c("GRACE" = "GRACE (obs)", "CESM2" = "CESM2", "IPSL" = "IPSL")
  ) +
  scale_x_continuous(
    name = "Dominant period (years)",
    breaks = 2:8,
    limits = c(1.5, 8)
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
  annotate("text", x = 1.6, y = Inf, label = "b",
           size = 5, fontface = "bold", hjust = 0, vjust = 1.5)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Power Matching Scatter (CESM2)
# ============================================================================

cat("Creating Panel C: Power matching scatter (CESM2)...\n")

# Define period classes (bins)
period_breaks <- c(0, 2.5, 3.5, 4.5, 5.5, Inf)
period_labels <- c("2-2.5yr", "2.5-3.5yr", "3.5-4.5yr", "4.5-5.5yr", "5.5+yr")

# Classify GRACE periods
grace_period_class <- grace_w %>%
  filter(!is.na(dominant_period_1) & !is.na(dominant_power_1)) %>%
  mutate(
    period_class = cut(dominant_period_1, breaks = period_breaks, labels = period_labels, include.lowest = TRUE),
    grace_power = dominant_power_1
  ) %>%
  select(basin, basin_name, period_class, grace_power)

# Classify CESM periods and compute median power per basin per period class
cesm_period_class <- cesm_w %>%
  filter(!is.na(period_1) & !is.na(power_1)) %>%
  mutate(
    period_class = cut(period_1, breaks = period_breaks, labels = period_labels, include.lowest = TRUE)
  ) %>%
  group_by(basin, basin_name, period_class) %>%
  summarise(
    power_p50 = median(power_1, na.rm = TRUE),
    power_p05 = quantile(power_1, 0.05, na.rm = TRUE),
    power_p95 = quantile(power_1, 0.95, na.rm = TRUE),
    n_members = n(),
    .groups = "drop"
  )

# Match GRACE and CESM for same period class
matching_cesm <- grace_period_class %>%
  inner_join(cesm_period_class, by = c("basin", "basin_name", "period_class")) %>%
  filter(!is.na(grace_power) & !is.na(power_p50))

cat("  Matched basins (CESM):", nrow(matching_cesm), "\n")
cat("  Period class counts:\n")
print(table(matching_cesm$period_class))

panel_c <- ggplot(matching_cesm, aes(x = power_p50, y = grace_power, color = period_class)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = power_p05, xmax = power_p95),
                 alpha = 0.3, linewidth = 0.3, height = 0) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_viridis_d(
    name = "Period\nclass",
    option = "C",
    begin = 0.2,
    end = 0.9
  ) +
  scale_x_continuous(
    name = "CESM2 power (median)",
    limits = c(0, 10)
  ) +
  scale_y_continuous(
    name = "GRACE power",
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
  annotate("text", x = 0.2, y = 9.5, label = sprintf("c\nn=%d", nrow(matching_cesm)),
           size = 3, fontface = "bold", hjust = 0, vjust = 1)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Power Matching Scatter (IPSL)
# ============================================================================

cat("Creating Panel D: Power matching scatter (IPSL)...\n")

# Classify IPSL periods and compute median power per basin per period class
ipsl_period_class <- ipsl_w %>%
  filter(!is.na(period_1) & !is.na(power_1)) %>%
  mutate(
    period_class = cut(period_1, breaks = period_breaks, labels = period_labels, include.lowest = TRUE)
  ) %>%
  group_by(basin, basin_name, period_class) %>%
  summarise(
    power_p50 = median(power_1, na.rm = TRUE),
    power_p05 = quantile(power_1, 0.05, na.rm = TRUE),
    power_p95 = quantile(power_1, 0.95, na.rm = TRUE),
    n_members = n(),
    .groups = "drop"
  )

# Match GRACE and IPSL for same period class
matching_ipsl <- grace_period_class %>%
  inner_join(ipsl_period_class, by = c("basin", "basin_name", "period_class")) %>%
  filter(!is.na(grace_power) & !is.na(power_p50))

cat("  Matched basins (IPSL):", nrow(matching_ipsl), "\n")
cat("  Period class counts:\n")
print(table(matching_ipsl$period_class))

panel_d <- ggplot(matching_ipsl, aes(x = power_p50, y = grace_power, color = period_class)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = power_p05, xmax = power_p95),
                 alpha = 0.3, linewidth = 0.3, height = 0) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_viridis_d(
    name = "Period\nclass",
    option = "C",
    begin = 0.2,
    end = 0.9
  ) +
  scale_x_continuous(
    name = "IPSL power (median)",
    limits = c(0, 10)
  ) +
  scale_y_continuous(
    name = "GRACE power",
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
  annotate("text", x = 0.2, y = 9.5, label = sprintf("d\nn=%d", nrow(matching_ipsl)),
           size = 3, fontface = "bold", hjust = 0, vjust = 1)

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

save_figure(fig3, "fig3_period_matching", width_mm = 180, height_mm = 150)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 3 PERIOD MATCHING COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig3_period_matching.{png,pdf}\n")
cat("============================================================================\n\n")
