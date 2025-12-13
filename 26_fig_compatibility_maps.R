# ============================================================================
# FIGURE 26: Multivariate Compatibility Analysis
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. Which basins are incompatible with model ensembles in multivariate space?
#   2. How does Mahalanobis distance vary spatially?
#   3. What is the global pattern of model-observation compatibility?
#
# Panels:
#   (a) Map: Compatibility class (compatible vs incompatible outliers)
#   (b) Map: Mahalanobis distance from model ensemble center
#   (c) Histogram: Distribution of Mahalanobis distances
#   (d) Scatter: Distance vs compatibility threshold (chi-square)
#
# Output:
#   outputs/figs/fig26_compatibility_maps.png (400 dpi)
#   outputs/figs/fig26_compatibility_maps.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 26: MULTIVARIATE COMPATIBILITY ANALYSIS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load Phase 8 compatibility data
cat("Loading Phase 8 compatibility data...\n")
compat <- readRDS("outputs/phase08_compatibility_basin.rds")

cat("  Compatibility data:", nrow(compat), "basins\n\n")

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

# Add bd_id from basin metadata if needed
basin_meta <- readRDS("outputs/phase07_corr_basin_summary.rds")
compat <- compat %>%
  left_join(basin_meta %>% select(basin_id, bd_id), by = "basin_id")

# ============================================================================
# PANEL A: Map of Compatibility Class
# ============================================================================

cat("Creating Panel A: Compatibility class map...\n")

# Merge with shapefile
basins_shp_compat <- merge(basins_shp,
                           compat %>%
                             select(bd_ID = bd_id, compatibility_class, C_b),
                           by = "bd_ID", all.x = TRUE)

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_compat %>% filter(!is.na(compatibility_class)),
          aes(fill = compatibility_class),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Compatibility\nclass",
    values = c(
      "compatible" = "#2171B5",           # Blue - compatible
      "incompatible_outlier" = "#CB181D"  # Red - incompatible
    ),
    labels = c(
      "compatible" = "Compatible",
      "incompatible_outlier" = "Incompatible"
    ),
    drop = FALSE
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(4, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a",
           size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Map of Mahalanobis Distance
# ============================================================================

cat("Creating Panel B: Mahalanobis distance map...\n")

# Merge with shapefile
basins_shp_mahal <- merge(basins_shp,
                          compat %>%
                            select(bd_ID = bd_id, d_mahal),
                          by = "bd_ID", all.x = TRUE)

# Create discrete categories
basins_shp_mahal <- basins_shp_mahal %>%
  mutate(
    mahal_category = cut(d_mahal,
                        breaks = c(0, 3, 5, 7, 10, 50),
                        labels = c("<3", "3-5", "5-7", "7-10", ">10"),
                        include.lowest = TRUE)
  )

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_mahal %>% filter(!is.na(mahal_category)),
          aes(fill = mahal_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Mahalanobis\ndistance",
    values = c(
      "<3" = "#F7FBFF",
      "3-5" = "#C6DBEF",
      "5-7" = "#6BAED6",
      "7-10" = "#2171B5",
      ">10" = "#08519C"
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
  annotate("text", x = -1.4e7, y = 7.5e6, label = "b",
           size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Histogram of Mahalanobis Distances
# ============================================================================

cat("Creating Panel C: Mahalanobis distance distribution...\n")

# Chi-square threshold for p=0.05, df=5 (5 metrics)
chi_threshold <- qchisq(0.95, df = 5)

panel_c <- ggplot(compat, aes(x = d_mahal)) +
  geom_histogram(bins = 30, fill = "#6BAED6", color = "black", linewidth = 0.2) +
  geom_vline(xintercept = chi_threshold, linetype = "dashed",
             color = "#CB181D", linewidth = 0.8) +
  annotate("text", x = chi_threshold + 0.5, y = Inf,
           label = paste0("χ² threshold (p=0.05): ", round(chi_threshold, 2)),
           hjust = 0, vjust = 1.5, size = 3, color = "#CB181D") +
  scale_x_continuous(
    name = "Mahalanobis distance",
    limits = c(0, max(compat$d_mahal, na.rm = TRUE) * 1.05)
  ) +
  scale_y_continuous(
    name = "Number of basins",
    expand = c(0, 0)
  ) +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 0.5, y = Inf, label = "c",
           size = 5, fontface = "bold", hjust = 0, vjust = 1.5)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Compatibility Summary Statistics
# ============================================================================

cat("Creating Panel D: Compatibility summary...\n")

# Calculate summary statistics
n_total <- nrow(compat)
n_compatible <- sum(compat$compatibility_class == "compatible", na.rm = TRUE)
n_incompatible <- sum(compat$compatibility_class == "incompatible_outlier", na.rm = TRUE)
pct_incompatible <- (n_incompatible / n_total) * 100

# Create summary data frame
summary_df <- data.frame(
  class = c("Compatible", "Incompatible"),
  count = c(n_compatible, n_incompatible),
  percentage = c((n_compatible/n_total)*100, (n_incompatible/n_total)*100)
)

panel_d <- ggplot(summary_df, aes(x = class, y = count, fill = class)) +
  geom_col(color = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c(
      "Compatible" = "#2171B5",
      "Incompatible" = "#CB181D"
    )
  ) +
  scale_y_continuous(
    name = "Number of basins",
    limits = c(0, max(summary_df$count) * 1.2),
    expand = c(0, 0)
  ) +
  scale_x_discrete(name = "") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  annotate("text", x = 0.6, y = max(summary_df$count) * 1.15,
           label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig26 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig26, "fig26_compatibility_maps", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 26 COMPATIBILITY MAPS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig26_compatibility_maps.{png,pdf}\n")
cat("============================================================================\n\n")

# ============================================================================
# Print Summary Statistics
# ============================================================================

cat("Compatibility summary:\n")
cat("  Total basins:       ", n_total, "\n")
cat("  Compatible:         ", n_compatible, " (", round((n_compatible/n_total)*100, 1), "%)\n", sep = "")
cat("  Incompatible:       ", n_incompatible, " (", round((n_incompatible/n_total)*100, 1), "%)\n", sep = "")
cat("\n")
cat("Mahalanobis distance statistics:\n")
cat("  Mean:   ", round(mean(compat$d_mahal, na.rm = TRUE), 2), "\n")
cat("  Median: ", round(median(compat$d_mahal, na.rm = TRUE), 2), "\n")
cat("  SD:     ", round(sd(compat$d_mahal, na.rm = TRUE), 2), "\n")
cat("  Range:  [", round(min(compat$d_mahal, na.rm = TRUE), 2), ", ",
    round(max(compat$d_mahal, na.rm = TRUE), 2), "]\n", sep = "")
cat("  χ² threshold (p=0.05, df=5): ", round(chi_threshold, 2), "\n\n")
