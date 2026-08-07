# ============================================================================
# FIGURE 1: Overview of Study Design
# ============================================================================
#
# Panels:
#   (a) Global basin map showing 184 river basins
#   (b) Schematic of windowing analysis (GRACE vs SMILE timeframes)
#   (c) Total analysis windows bar chart (CESM2 vs IPSL)
#
# Output:
#   outputs/figs/fig1_overview.png (500 dpi)
#   outputs/figs/fig1_overview.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 1: OVERVIEW\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load basin attributes
cat("Loading data...\n")
attrs <- readRDS("outputs/basin_attributes.rds")

# Load dispersion summary for window counts
disp <- readRDS("outputs/dispersion_summary.rds")

# Load basin shapefile
cat("Loading basin shapefile...\n")
basins_shp <- st_read("/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)

# Simplify geometries to reduce PDF file size (tolerance = 0.1 degrees ~ 10km)
cat("  Simplifying geometries for smaller file size...\n")
basins_shp <- st_simplify(basins_shp, dTolerance = 0.1, preserveTopology = TRUE)

cat("  Basins:", nrow(attrs), "\n")
cat("  Dispersion summary basins:", nrow(disp), "\n")
cat("  Shapefile basins:", nrow(basins_shp), "\n\n")

# ============================================================================
# PANEL A: Global Basin Map
# ============================================================================

cat("Creating Panel A: Global basin map...\n")

# Load country borders (simplified, turn off s2)
sf::sf_use_s2(FALSE)
world_borders <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
sf::sf_use_s2(TRUE)

# Create map with country outlines and basin polygons
panel_a <- ggplot() +
  # Country borders only (no fill)
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  # Basin polygons
  geom_sf(data = basins_shp, color = "#d95f02", fill = "#d95f02",
          alpha = 0.7, linewidth = 0.3) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  # Add panel label
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Windowing Schematic
# ============================================================================

cat("Creating Panel B: Windowing schematic...\n")

# Define time spans
grace_start <- 2002
grace_end <- 2024
grace_length <- 23  # years
grace_months <- grace_length * 12  # 276 months

cesm_start <- 1900
cesm_end <- 2100
cesm_months <- 201 * 12  # 2412 months

ipsl_start <- 1900
ipsl_end <- 2020
ipsl_months <- 121 * 12  # 1452 months

# Calculate windows per member
cesm_windows_per_member <- cesm_months - grace_months + 1  # 2137
ipsl_windows_per_member <- ipsl_months - grace_months + 1  # 1177

# Create data frame for timeline bars
timeline_data <- data.frame(
  model = c("CESM2 (80 members)", "IPSL (18 members)", "GRACE"),
  start = c(cesm_start, ipsl_start, grace_start),
  end = c(cesm_end, ipsl_end, grace_end),
  y = c(3, 2, 1),
  color = c("#0072B2", "#009E73", "black")
)

# Create example windows on CESM2 bar
window_starts <- seq(cesm_start, cesm_end - grace_length, by = 30)
window_data <- data.frame(
  start = window_starts,
  end = window_starts + grace_length,
  y = 3
) %>%
  slice_head(n = 6)  # Show first 6 windows as examples

panel_b <- ggplot() +
  # Model timeline bars
  geom_rect(data = timeline_data,
            aes(xmin = start, xmax = end, ymin = y - 0.3, ymax = y + 0.3, fill = model),
            alpha = 0.8) +
  scale_fill_manual(
    name = NULL,
    values = c("CESM2 (80 members)" = "#0072B2",
               "IPSL (18 members)" = "#009E73",
               "GRACE" = "black")
  ) +
  # Example windows (on CESM2)
  geom_rect(data = window_data,
            aes(xmin = start, xmax = end, ymin = y - 0.25, ymax = y + 0.25),
            fill = NA, color = "black", linewidth = 0.4, linetype = "dashed") +
  # Labels
  geom_text(data = timeline_data,
            aes(x = (start + end) / 2, y = y, label = model),
            size = 2.5, fontface = "bold", color = "white") +
  # Annotation for windowing concept
  annotate("text", x = 1950, y = 3.8, label = "Sliding windows (L = 23 yr)",
           size = 2.2, hjust = 0, fontface = "italic") +
  annotate("segment", x = 1920, xend = 1920 + grace_length, y = 3.6, yend = 3.6,
           linewidth = 0.6, color = "black") +
  # Add arrow heads manually
  annotate("segment", x = 1920, xend = 1922, y = 3.6, yend = 3.55,
           linewidth = 0.4, color = "black") +
  annotate("segment", x = 1920, xend = 1922, y = 3.6, yend = 3.65,
           linewidth = 0.4, color = "black") +
  annotate("segment", x = 1920 + grace_length, xend = 1920 + grace_length - 2,
           y = 3.6, yend = 3.55, linewidth = 0.4, color = "black") +
  annotate("segment", x = 1920 + grace_length, xend = 1920 + grace_length - 2,
           y = 3.6, yend = 3.65, linewidth = 0.4, color = "black") +
  # Styling
  scale_x_continuous(
    name = "Year",
    breaks = seq(1900, 2100, by = 50),
    limits = c(1880, 2120)
  ) +
  scale_y_continuous(limits = c(0.5, 4.2), expand = c(0, 0)) +
  theme_nature(base_size = 8) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3)
  ) +
  # Add panel label
  annotate("text", x = 1885, y = 4.1, label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Analysis Scale Visualization
# ============================================================================

cat("Creating Panel C: Analysis scale visualization...\n")

# Compute exact window counts
n_basins <- nrow(disp)
cesm_members <- 80
ipsl_members <- 18

# Windows per basin
cesm_windows_per_basin <- cesm_members * cesm_windows_per_member  # 80 × 2137 = 170,960
ipsl_windows_per_basin <- ipsl_members * ipsl_windows_per_member  # 18 × 1177 = 21,186
total_windows_per_basin <- cesm_windows_per_basin + ipsl_windows_per_basin  # 192,146

# Global totals
cesm_total_windows <- n_basins * cesm_windows_per_basin  # 184 × 170,960
ipsl_total_windows <- n_basins * ipsl_windows_per_basin  # 184 × 21,186
global_total_windows <- n_basins * total_windows_per_basin  # 35.4 million

# Create stacked bar chart showing windows per basin
windows_data <- data.frame(
  category = c("Per Basin", "Global Total"),
  cesm = c(cesm_windows_per_basin / 1000, cesm_total_windows / 1e6),
  ipsl = c(ipsl_windows_per_basin / 1000, ipsl_total_windows / 1e6),
  total = c(total_windows_per_basin / 1000, global_total_windows / 1e6)
)

windows_long <- data.frame(
  category = rep(c("Per Basin", "Global Total"), each = 2),
  model = rep(c("CESM2", "IPSL"), 2),
  value = c(cesm_windows_per_basin / 1000, ipsl_windows_per_basin / 1000,
            cesm_total_windows / 1e6, ipsl_total_windows / 1e6),
  unit = rep(c("thousands", "millions"), each = 2)
)

windows_long$category <- factor(windows_long$category, levels = c("Per Basin", "Global Total"))

panel_c <- ggplot(windows_long, aes(x = model, y = value, fill = model)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.4, width = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c("CESM2" = "#0072B2", "IPSL" = "#009E73"),
    labels = c(
      sprintf("CESM2: 80 × %s", format(cesm_windows_per_member, big.mark=",")),
      sprintf("IPSL: 18 × %s", format(ipsl_windows_per_member, big.mark=","))
    )
  ) +
  scale_y_log10(
    name = "Analysis windows",
    breaks = c(10, 20, 50, 100, 200, 1, 3, 10, 30),
    labels = function(x) {
      ifelse(x < 1000,
             paste0(x, "k"),
             sprintf("%.1fM", x/1000))
    },
    expand = expansion(mult = c(0.02, 0.15))
  ) +
  scale_x_discrete(name = "") +
  # Add value labels on bars
  geom_text(aes(label = ifelse(category == "Per Basin",
                               format(round(value, 1), big.mark=","),
                               sprintf("%.1fM", value))),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.5, fontface = "bold") +
  facet_wrap(~category, ncol = 2) +
  annotation_logticks(sides = "l", size = 0.25, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.5, 0.95),
    legend.justification = c(0.5, 0.5),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.text = element_text(size = 6.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.3),
    strip.text = element_text(size = 7.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  annotate("text", x = 0.55, y = Inf, label = "c", size = 5, fontface = "bold", hjust = 0, vjust = 1.2)

cat("  ✓ Panel C complete\n\n")

# ============================================================================
# PANEL D: Analysis Workflow Diagram
# ============================================================================

cat("Creating Panel D: Analysis workflow diagram...\n")

# Create workflow boxes data
workflow_boxes <- data.frame(
  x = c(0.5, 0.5, 0.5, 0.15, 0.5, 0.85, 0.15, 0.5, 0.85),
  y = c(0.92, 0.75, 0.58, 0.38, 0.38, 0.38, 0.18, 0.18, 0.18),
  width = c(0.85, 0.85, 0.85, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25),
  height = c(0.10, 0.10, 0.10, 0.12, 0.12, 0.12, 0.12, 0.12, 0.12),
  label = c(
    "1. Preprocessing\nDetrend, deseasonalize,\nlow-pass filter",
    "2. Windowing\nExtract 192k windows\nper basin",
    "3. Dispersion\nAmplitude & variance\nenvelopes",
    "4. Wavelet\nSpectral\npower",
    "5. Persistence\nAR(1)\ntimescales",
    "6. Events\nMorphology\nanalysis",
    "7. Coherence\nCross-basin\ncorrelation",
    "8. Multivariate\nMD test",
    "Results\nIntegration"
  ),
  fill_color = c(
    "#E8F4F8", "#E8F4F8", "#E8F4F8",
    "#CCE5FF", "#CCE5FF", "#CCE5FF",
    "#B3D9FF", "#B3D9FF", "#B3D9FF"
  )
)

panel_d <- ggplot(workflow_boxes) +
  # Draw boxes
  geom_rect(aes(xmin = x - width/2, xmax = x + width/2,
                ymin = y - height/2, ymax = y + height/2,
                fill = I(fill_color)),
            color = "black", linewidth = 0.5) +
  # Add text labels
  geom_text(aes(x = x, y = y, label = label),
            size = 2.2, fontface = "bold", lineheight = 0.85) +
  # Add arrows
  annotate("segment", x = 0.5, xend = 0.5, y = 0.87, yend = 0.80,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           linewidth = 0.6, color = "grey30") +
  annotate("segment", x = 0.5, xend = 0.5, y = 0.70, yend = 0.63,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           linewidth = 0.6, color = "grey30") +
  annotate("segment", x = 0.5, xend = 0.15, y = 0.53, yend = 0.44,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           linewidth = 0.5, color = "grey30") +
  annotate("segment", x = 0.5, xend = 0.5, y = 0.53, yend = 0.44,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           linewidth = 0.5, color = "grey30") +
  annotate("segment", x = 0.5, xend = 0.85, y = 0.53, yend = 0.44,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           linewidth = 0.5, color = "grey30") +
  annotate("segment", x = 0.15, xend = 0.15, y = 0.32, yend = 0.24,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           linewidth = 0.5, color = "grey30") +
  annotate("segment", x = 0.5, xend = 0.5, y = 0.32, yend = 0.24,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           linewidth = 0.5, color = "grey30") +
  annotate("segment", x = 0.85, xend = 0.85, y = 0.32, yend = 0.24,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           linewidth = 0.5, color = "grey30") +
  # Panel title
  annotate("text", x = 0.5, y = 0.98,
           label = "Analysis Workflow (8 phases)",
           size = 3.2, fontface = "bold", hjust = 0.5) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void(base_size = 8) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  # Add panel label
  annotate("text", x = 0.02, y = 0.98, label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: A on top (full width), B and C in middle (side by side), D at bottom (full width)
fig1 <- panel_a /
  (panel_b | panel_c) /
  panel_d +
  plot_layout(heights = c(1.2, 1, 0.9))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig1, "fig1_overview", width_mm = 180, height_mm = 200)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 1 COMPLETE\n")
cat("============================================================================\n\n")
