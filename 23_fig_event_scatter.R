# ============================================================================
# FIGURE 23: Event Height/Depth Scatter Plots (GRACE vs Model Envelopes)
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. How do GRACE pluvial heights compare to model ensemble distributions?
#   2. How do GRACE drought depths compare to model ensemble distributions?
#   3. Are GRACE events systematically above or below model envelopes?
#   4. Which basins show the largest deviations from model predictions?
#
# Panels:
#   (a) Scatter: GRACE pluvial height vs CESM2 ensemble (p05, p50, p95)
#   (b) Scatter: GRACE pluvial height vs IPSL ensemble (p05, p50, p95)
#   (c) Scatter: GRACE drought depth vs CESM2 ensemble (p05, p50, p95)
#   (d) Scatter: GRACE drought depth vs IPSL ensemble (p05, p50, p95)
#
# Output:
#   outputs/figs/fig23_event_scatter.png (400 dpi)
#   outputs/figs/fig23_event_scatter.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 23: EVENT HEIGHT/DEPTH SCATTER PLOTS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data with volumes
cat("Loading event data with height/depth volumes...\n")
events <- readRDS("outputs/phase06_event_summary_corrected.rds")

cat("  Event data:", nrow(events), "basins\n\n")

# ============================================================================
# PANEL A: GRACE Pluvial Height vs CESM2 Ensemble
# ============================================================================

cat("Creating Panel A: GRACE pluvial height vs CESM2...\n")

# Prepare data
scatter_pluvial_cesm <- events %>%
  select(basin_id, basin_name,
         V_H_max_grace,
         V_H_p05_cesm, V_H_p50_cesm, V_H_p95_cesm,
         percentile_H_cesm) %>%
  filter(!is.na(V_H_max_grace)) %>%
  mutate(
    pct_category = cut(percentile_H_cesm,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

# Calculate 1:1 line range
max_val_pluvial <- max(c(scatter_pluvial_cesm$V_H_max_grace,
                         scatter_pluvial_cesm$V_H_p95_cesm), na.rm = TRUE)

panel_a <- ggplot(scatter_pluvial_cesm, aes(x = V_H_p50_cesm, y = V_H_max_grace)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  # Points colored by percentile
  geom_point(aes(fill = pct_category), size = 2.5, alpha = 0.8, shape = 21, color = "black", stroke = 0.2) +
  scale_fill_manual(
    name = "GRACE\npercentile\nin CESM2",
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
    name = "CESM2 median pluvial height (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  scale_y_log10(
    name = "GRACE pluvial height (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  annotation_logticks(sides = "bl", size = 0.25, color = "grey50") +
  labs(title = "PLUVIAL (wet events, positive anomalies)") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    legend.position = "right"
  ) +
  annotate("text", x = 10^(log10(10) + 0.1), y = 10^(log10(max_val_pluvial) - 0.1),
           label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: GRACE Pluvial Height vs IPSL Ensemble
# ============================================================================

cat("Creating Panel B: GRACE pluvial height vs IPSL...\n")

# Prepare data
scatter_pluvial_ipsl <- events %>%
  select(basin_id, basin_name,
         V_H_max_grace,
         V_H_p05_ipsl, V_H_p50_ipsl, V_H_p95_ipsl,
         percentile_H_ipsl) %>%
  filter(!is.na(V_H_max_grace)) %>%
  mutate(
    pct_category = cut(percentile_H_ipsl,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

panel_b <- ggplot(scatter_pluvial_ipsl, aes(x = V_H_p50_ipsl, y = V_H_max_grace)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  # Points colored by percentile
  geom_point(aes(fill = pct_category), size = 2.5, alpha = 0.8, shape = 21, color = "black", stroke = 0.2) +
  scale_fill_manual(
    name = "GRACE\npercentile\nin IPSL",
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
    name = "IPSL median pluvial height (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  scale_y_log10(
    name = "GRACE pluvial height (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  annotation_logticks(sides = "bl", size = 0.25, color = "grey50") +
  labs(title = "PLUVIAL (wet events, positive anomalies)") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    legend.position = "right"
  ) +
  annotate("text", x = 10^(log10(10) + 0.1), y = 10^(log10(max_val_pluvial) - 0.1),
           label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: GRACE Drought Depth vs CESM2 Ensemble
# ============================================================================

cat("Creating Panel C: GRACE drought depth vs CESM2...\n")

# Prepare data (use absolute values for drought)
scatter_drought_cesm <- events %>%
  select(basin_id, basin_name,
         V_D_max_grace,
         V_D_p05_cesm, V_D_p50_cesm, V_D_p95_cesm,
         percentile_D_cesm) %>%
  filter(!is.na(V_D_max_grace)) %>%
  mutate(
    V_D_max_grace_abs = abs(V_D_max_grace),
    V_D_p05_cesm_abs = abs(V_D_p05_cesm),
    V_D_p50_cesm_abs = abs(V_D_p50_cesm),
    V_D_p95_cesm_abs = abs(V_D_p95_cesm),
    pct_category = cut(percentile_D_cesm,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

# Calculate 1:1 line range
max_val_drought <- max(c(scatter_drought_cesm$V_D_max_grace_abs,
                         scatter_drought_cesm$V_D_p95_cesm_abs), na.rm = TRUE)

panel_c <- ggplot(scatter_drought_cesm, aes(x = V_D_p50_cesm_abs, y = V_D_max_grace_abs)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  # Points colored by percentile
  geom_point(aes(fill = pct_category), size = 2.5, alpha = 0.8, shape = 21, color = "black", stroke = 0.2) +
  scale_fill_manual(
    name = "GRACE\npercentile\nin CESM2",
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
    name = "CESM2 median drought depth (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  scale_y_log10(
    name = "GRACE drought depth (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  annotation_logticks(sides = "bl", size = 0.25, color = "grey50") +
  labs(title = "DROUGHT (dry events, negative anomalies)") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    legend.position = "right"
  ) +
  annotate("text", x = 10^(log10(10) + 0.1), y = 10^(log10(max_val_drought) - 0.1),
           label = "c", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: GRACE Drought Depth vs IPSL Ensemble
# ============================================================================

cat("Creating Panel D: GRACE drought depth vs IPSL...\n")

# Prepare data (use absolute values for drought)
scatter_drought_ipsl <- events %>%
  select(basin_id, basin_name,
         V_D_max_grace,
         V_D_p05_ipsl, V_D_p50_ipsl, V_D_p95_ipsl,
         percentile_D_ipsl) %>%
  filter(!is.na(V_D_max_grace)) %>%
  mutate(
    V_D_max_grace_abs = abs(V_D_max_grace),
    V_D_p05_ipsl_abs = abs(V_D_p05_ipsl),
    V_D_p50_ipsl_abs = abs(V_D_p50_ipsl),
    V_D_p95_ipsl_abs = abs(V_D_p95_ipsl),
    pct_category = cut(percentile_D_ipsl,
                      breaks = c(0, 5, 25, 75, 95, 100),
                      labels = c("<5%", "5-25%", "25-75%", "75-95%", ">95%"),
                      include.lowest = TRUE)
  )

panel_d <- ggplot(scatter_drought_ipsl, aes(x = V_D_p50_ipsl_abs, y = V_D_max_grace_abs)) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  # Points colored by percentile
  geom_point(aes(fill = pct_category), size = 2.5, alpha = 0.8, shape = 21, color = "black", stroke = 0.2) +
  scale_fill_manual(
    name = "GRACE\npercentile\nin IPSL",
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
    name = "IPSL median drought depth (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  scale_y_log10(
    name = "GRACE drought depth (km³)",
    breaks = c(10, 50, 100, 500, 1000),
    labels = c("10", "50", "100", "500", "1000")
  ) +
  annotation_logticks(sides = "bl", size = 0.25, color = "grey50") +
  labs(title = "DROUGHT (dry events, negative anomalies)") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    legend.position = "right"
  ) +
  annotate("text", x = 10^(log10(10) + 0.1), y = 10^(log10(max_val_drought) - 0.1),
           label = "d", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig23 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig23, "fig23_event_scatter", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 23 EVENT SCATTER PLOTS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig23_event_scatter.{png,pdf}\n")
cat("============================================================================\n\n")
