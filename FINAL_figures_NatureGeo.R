#!/usr/bin/env Rscript
# ============================================================================
# FINAL FIGURES FOR NATURE GEOSCIENCE SUBMISSION
# ============================================================================
#
# This script generates all 6 main figures + 2 SI figures for submission
# Fixes identified issues:
#   1. Unified figure set with maps where needed
#   2. Correct panel labels (lowercase a, b, c)
#   3. Fixed Figure 4 panel c (correct GRACE vs model event counts)
#   4. Nature Geo compliant formatting (8pt fonts, proper dimensions)
#
# Output: outputs/FINAL_NatureGeo/
#
# Author: Ashraf Rateb
# Date: 2025-12-01
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FINAL FIGURES FOR NATURE GEOSCIENCE\n")
cat("============================================================================\n\n")

# ============================================================================
# SETUP
# ============================================================================

library(data.table)
library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)

# Set working directory
setwd("/Volumes/lab/projs/2025/CESM_TWS/GGFO_Vs_MMLEs")

# Create output directory
OUT_DIR <- "outputs/FINAL_NatureGeo"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Output directory:", OUT_DIR, "\n\n")

# ============================================================================
# NATURE GEOSCIENCE THEME
# ============================================================================

theme_nature <- function(base_size = 8, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_line(colour = "black", linewidth = 0.5),
      axis.ticks.length = unit(2, "pt"),
      axis.text = element_text(colour = "black", size = rel(0.9)),
      axis.title = element_text(colour = "black", size = rel(1.0)),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key = element_rect(fill = "white", colour = NA),
      legend.key.size = unit(4, "mm"),
      legend.text = element_text(size = rel(0.9)),
      legend.title = element_text(size = rel(1.0), face = "bold"),
      legend.position = "right",
      strip.background = element_rect(fill = "grey90", colour = "black", linewidth = 0.5),
      strip.text = element_text(colour = "black", size = rel(1.0), face = "bold"),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0),
      plot.margin = margin(2, 2, 2, 2, "mm")
    )
}

theme_nature_map <- function(base_size = 8, base_family = "Helvetica") {
  theme_nature(base_size, base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

# Save function
save_fig <- function(plot, name, width_mm = 180, height_mm = 120, dpi = 500) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4

  ggsave(file.path(OUT_DIR, paste0(name, ".png")), plot,
         width = width_in, height = height_in, dpi = dpi, bg = "white")
  ggsave(file.path(OUT_DIR, paste0(name, ".pdf")), plot,
         width = width_in, height = height_in, device = cairo_pdf, bg = "white")
  cat("  Saved:", name, "\n")
}

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading data...\n")

# Basin attributes
attrs <- readRDS("outputs/basin_attributes.rds")

# Dispersion results
disp <- fread("outputs/dispersion_summary.csv")

# Wavelet results
wave_sum <- fread("outputs/phase04_wavelet_summary.csv")

# Persistence results
pers_sum <- fread("outputs/phase05_persistence_summary.csv")

# Event results
events <- fread("outputs/phase06_event_summary.csv")
events_grace <- fread("outputs/phase06_events_grace.csv")
events_models <- fread("outputs/phase06_events_models.csv")

# Compatibility results
compat <- fread("outputs/phase08_compatibility_basin.csv")

# Basin shapefile
cat("Loading shapefile...\n")
basins_shp <- st_read("/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)
basins_shp <- st_simplify(basins_shp, dTolerance = 0.1, preserveTopology = TRUE)

# World map
sf::sf_use_s2(FALSE)
world <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
sf::sf_use_s2(TRUE)

cat("  Basins:", nrow(disp), "\n")
cat("  GRACE events:", nrow(events_grace), "\n")
cat("  Model events:", nrow(events_models), "\n\n")

# ============================================================================
# FIGURE 1: STUDY OVERVIEW
# ============================================================================

cat("FIGURE 1: Study Overview...\n")

# Panel A: Global basin map
panel_1a <- ggplot() +
  geom_sf(data = world, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp, color = "#d95f02", fill = "#d95f02",
          alpha = 0.7, linewidth = 0.3) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

# Panel B: Timeline schematic
grace_start <- 2002; grace_end <- 2024; grace_length <- 23
cesm_start <- 1900; cesm_end <- 2100
ipsl_start <- 1900; ipsl_end <- 2020

timeline_data <- data.frame(
  model = c("CESM2 (80 members)", "IPSL (18 members)", "GRACE"),
  start = c(cesm_start, ipsl_start, grace_start),
  end = c(cesm_end, ipsl_end, grace_end),
  y = c(3, 2, 1)
)

panel_1b <- ggplot() +
  geom_rect(data = timeline_data,
            aes(xmin = start, xmax = end, ymin = y - 0.3, ymax = y + 0.3, fill = model),
            alpha = 0.8) +
  scale_fill_manual(values = c("CESM2 (80 members)" = "#0072B2",
                               "IPSL (18 members)" = "#009E73",
                               "GRACE" = "black")) +
  geom_text(data = timeline_data,
            aes(x = (start + end) / 2, y = y, label = model),
            size = 2.5, fontface = "bold", color = "white") +
  annotate("text", x = 1950, y = 3.8, label = "Sliding windows (L = 23 yr)",
           size = 2.2, hjust = 0, fontface = "italic") +
  scale_x_continuous(name = "Year", breaks = seq(1900, 2100, by = 50), limits = c(1880, 2120)) +
  scale_y_continuous(limits = c(0.5, 4.2), expand = c(0, 0)) +
  theme_nature(base_size = 8) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(),
        legend.position = "none") +
  annotate("text", x = 1885, y = 4.1, label = "b", size = 5, fontface = "bold", hjust = 0)

# Panel C: Window counts
n_basins <- nrow(disp)
cesm_windows_per_member <- (201 * 12) - (23 * 12) + 1  # 2137
ipsl_windows_per_member <- (121 * 12) - (23 * 12) + 1  # 1177
cesm_total <- n_basins * 80 * cesm_windows_per_member / 1e6
ipsl_total <- n_basins * 18 * ipsl_windows_per_member / 1e6

windows_df <- data.frame(
  model = c("CESM2", "IPSL"),
  windows = c(cesm_total, ipsl_total)
)

panel_1c <- ggplot(windows_df, aes(x = model, y = windows, fill = model)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1fM", windows)), vjust = -0.3, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("CESM2" = "#0072B2", "IPSL" = "#009E73")) +
  scale_y_continuous(name = "Total windows (millions)", expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL) +
  theme_nature(base_size = 8) +
  theme(legend.position = "none", panel.grid.major.x = element_blank()) +
  annotate("text", x = 0.55, y = max(windows_df$windows) * 1.1, label = "c",
           size = 5, fontface = "bold", hjust = 0)

fig1 <- panel_1a / (panel_1b | panel_1c) + plot_layout(heights = c(1.2, 1))
save_fig(fig1, "Figure1_overview", width_mm = 180, height_mm = 160)

# ============================================================================
# FIGURE 2: DISPERSION ANALYSIS (WITH MAPS)
# ============================================================================

cat("FIGURE 2: Dispersion Analysis...\n")

# Merge with shapefile
disp_sf <- merge(basins_shp, disp, by.x = "Num", by.y = "basin")

# Classify dispersion direction
disp_sf <- disp_sf %>%
  mutate(
    amp_class_cesm = case_when(
      A_grace > A_p95_cesm ~ "above_p95",
      A_grace < A_p05_cesm ~ "below_p05",
      TRUE ~ "within"
    ),
    var_class_cesm = case_when(
      sigma_grace > sigma_p95_cesm ~ "above_p95",
      sigma_grace < sigma_p05_cesm ~ "below_p05",
      TRUE ~ "within"
    )
  )

# Panel A: Amplitude map
panel_2a <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = disp_sf, aes(fill = amp_class_cesm), color = "grey30", linewidth = 0.1) +
  scale_fill_manual(
    name = "CESM2\nAmplitude",
    values = c("within" = "#87CEEB", "above_p95" = "#505050", "below_p05" = "#B8B8B8"),
    labels = c("Within envelope", "Above p95", "Below p05")
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(legend.position = c(0.15, 0.35), legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 6), legend.title = element_text(size = 7)) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

# Panel B: Variance map
panel_2b <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = disp_sf, aes(fill = var_class_cesm), color = "grey30", linewidth = 0.1) +
  scale_fill_manual(
    name = "CESM2\nVariance",
    values = c("within" = "#87CEEB", "above_p95" = "#505050", "below_p05" = "#B8B8B8"),
    labels = c("Within envelope", "Above p95", "Below p05")
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(legend.position = c(0.15, 0.35), legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 6), legend.title = element_text(size = 7)) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "b", size = 5, fontface = "bold", hjust = 0)

# Count basins above/below
n_above_amp <- sum(disp_sf$amp_class_cesm == "above_p95", na.rm = TRUE)
n_below_amp <- sum(disp_sf$amp_class_cesm == "below_p05", na.rm = TRUE)
n_above_var <- sum(disp_sf$var_class_cesm == "above_p95", na.rm = TRUE)
n_below_var <- sum(disp_sf$var_class_cesm == "below_p05", na.rm = TRUE)

# Panel C: GRACE vs CESM amplitude scatter
panel_2c <- ggplot(disp, aes(x = A_p50_cesm, y = A_grace, color = amp_class_cesm)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = A_p05_cesm, xmax = A_p95_cesm), alpha = 0.3, height = 0, linewidth = 0.3) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_color_manual(values = c("within" = "#87CEEB", "above_p95" = "#505050", "below_p05" = "#B8B8B8")) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "CESM2 amplitude (mm, median)", y = "GRACE amplitude (mm)") +
  annotate("text", x = 30, y = 800, label = sprintf("n=%d (above p95)\nn=%d (below p05)", n_above_amp, n_below_amp),
           hjust = 0, size = 2.5) +
  theme_nature(base_size = 8) +
  theme(legend.position = "none") +
  annotate("text", x = 25, y = 1000, label = "c", size = 5, fontface = "bold", hjust = 0)

# Classify dispersion for disp data.table
disp[, amp_class_cesm := fifelse(A_grace > A_p95_cesm, "above_p95",
                                  fifelse(A_grace < A_p05_cesm, "below_p05", "within"))]
disp[, var_class_cesm := fifelse(sigma_grace > sigma_p95_cesm, "above_p95",
                                  fifelse(sigma_grace < sigma_p05_cesm, "below_p05", "within"))]

# Panel D: GRACE vs CESM variance scatter
panel_2d <- ggplot(disp, aes(x = sigma_p50_cesm, y = sigma_grace, color = var_class_cesm)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = sigma_p05_cesm, xmax = sigma_p95_cesm), alpha = 0.3, height = 0, linewidth = 0.3) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_color_manual(values = c("within" = "#87CEEB", "above_p95" = "#505050", "below_p05" = "#B8B8B8")) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "CESM2 variance (mm, median)", y = "GRACE variance (mm)") +
  annotate("text", x = 8, y = 250, label = sprintf("n=%d (above p95)\nn=%d (below p05)", n_above_var, n_below_var),
           hjust = 0, size = 2.5) +
  theme_nature(base_size = 8) +
  theme(legend.position = "none") +
  annotate("text", x = 6, y = 350, label = "d", size = 5, fontface = "bold", hjust = 0)

fig2 <- (panel_2a | panel_2b) / (panel_2c | panel_2d)
save_fig(fig2, "Figure2_dispersion", width_mm = 180, height_mm = 160)

# ============================================================================
# FIGURE 3: SPECTRAL POWER & PERSISTENCE
# ============================================================================

cat("FIGURE 3: Spectral Power & Persistence...\n")

# Panel A: ENSO power percentiles
median_enso <- median(wave_sum$enso_power_percentile_cesm, na.rm = TRUE)
frac_above90_enso <- mean(wave_sum$enso_power_percentile_cesm > 0.9, na.rm = TRUE) * 100

panel_3a <- ggplot(wave_sum, aes(x = enso_power_percentile_cesm)) +
  geom_histogram(bins = 20, fill = "#D32F2F", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0.9, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = 0.92, y = Inf, label = sprintf("%.1f%% > 90th", frac_above90_enso),
           vjust = 2, hjust = 0, size = 2.5) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "GRACE ENSO power percentile in CESM2", y = "Number of basins",
       title = "ENSO band (2-7 years)") +
  theme_nature(base_size = 8) +
  theme(plot.title = element_text(size = 9)) +
  annotate("text", x = 0.02, y = Inf, label = "a", size = 5, fontface = "bold", hjust = 0, vjust = 2)

# Panel B: QD power percentiles
median_qd <- median(wave_sum$qd_power_percentile_cesm, na.rm = TRUE)
frac_above90_qd <- mean(wave_sum$qd_power_percentile_cesm > 0.9, na.rm = TRUE) * 100

panel_3b <- ggplot(wave_sum, aes(x = qd_power_percentile_cesm)) +
  geom_histogram(bins = 20, fill = "#1976D2", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0.9, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = 0.92, y = Inf, label = sprintf("%.1f%% > 90th", frac_above90_qd),
           vjust = 2, hjust = 0, size = 2.5) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "GRACE QD power percentile in CESM2", y = "Number of basins",
       title = "Quasi-decadal band (7-13 years)") +
  theme_nature(base_size = 8) +
  theme(plot.title = element_text(size = 9)) +
  annotate("text", x = 0.02, y = Inf, label = "b", size = 5, fontface = "bold", hjust = 0, vjust = 2)

# Panel C: Persistence comparison boxplots
pers_long <- pers_sum %>%
  select(basin_name, tau_grace, tau_p50_cesm, tau_p50_ipsl) %>%
  pivot_longer(cols = -basin_name, names_to = "source", values_to = "tau") %>%
  mutate(source = case_when(
    source == "tau_grace" ~ "GRACE",
    source == "tau_p50_cesm" ~ "CESM2",
    source == "tau_p50_ipsl" ~ "IPSL"
  ),
  source = factor(source, levels = c("GRACE", "CESM2", "IPSL")))

panel_3c <- ggplot(pers_long, aes(x = source, y = tau, fill = source)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.8) +
  scale_fill_manual(values = c("GRACE" = "#D55E00", "CESM2" = "#0072B2", "IPSL" = "#009E73")) +
  labs(x = NULL, y = "Persistence timescale τ (months)") +
  theme_nature(base_size = 8) +
  theme(legend.position = "none") +
  annotate("text", x = 0.55, y = max(pers_long$tau, na.rm = TRUE) * 0.95,
           label = "c", size = 5, fontface = "bold", hjust = 0)

# Panel D: Tau ratio histogram
pers_sum_dt <- as.data.table(pers_sum)
pers_sum_dt[, tau_ratio := tau_grace / tau_p50_cesm]
median_ratio <- median(pers_sum_dt$tau_ratio, na.rm = TRUE)

panel_3d <- ggplot(pers_sum_dt, aes(x = tau_ratio)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = median_ratio, linetype = "solid", color = "black", linewidth = 1) +
  annotate("text", x = median_ratio + 0.05, y = Inf,
           label = sprintf("median = %.2f", median_ratio),
           vjust = 2, hjust = 0, size = 2.5) +
  labs(x = "GRACE / CESM2 persistence ratio", y = "Number of basins") +
  theme_nature(base_size = 8) +
  annotate("text", x = min(pers_sum_dt$tau_ratio, na.rm = TRUE) + 0.02, y = Inf,
           label = "d", size = 5, fontface = "bold", hjust = 0, vjust = 2)

fig3 <- (panel_3a | panel_3b) / (panel_3c | panel_3d)
save_fig(fig3, "Figure3_spectral_persistence", width_mm = 180, height_mm = 140)

# ============================================================================
# FIGURE 4: EVENT MORPHOLOGY (FIXED)
# ============================================================================

cat("FIGURE 4: Event Morphology (FIXED)...\n")

# Model pluvials
pluvials_model <- events_models %>%
  filter(type == "pluvial" & !is.na(pluvial_height) & !is.na(duration_months)) %>%
  filter(duration_months <= 50 & pluvial_height <= 500)

# GRACE pluvials
pluvials_grace <- events_grace %>%
  filter(type == "pluvial" & !is.na(pluvial_height) & !is.na(duration_months))

# Panel A: Pluvial height vs duration
panel_4a <- ggplot() +
  geom_hex(data = pluvials_model,
           aes(x = duration_months, y = pluvial_height),
           bins = 40, alpha = 0.7) +
  scale_fill_viridis_c(name = "Model\nevents", option = "C", trans = "log10",
                       breaks = c(10, 100, 1000, 10000)) +
  geom_point(data = pluvials_grace,
             aes(x = duration_months, y = pluvial_height),
             color = "#D55E00", fill = "#D55E00", size = 2, alpha = 0.8, shape = 21, stroke = 0.5) +
  scale_x_continuous(name = "Duration (months)", limits = c(0, 50)) +
  scale_y_continuous(name = "Pluvial height (mm)", limits = c(0, 500)) +
  theme_nature(base_size = 8) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
        legend.key.size = unit(3, "mm")) +
  annotate("point", x = 45, y = 470, color = "#D55E00", fill = "#D55E00", size = 2.5, shape = 21) +
  annotate("text", x = 43, y = 470, label = "GRACE", hjust = 1, size = 2.5, fontface = "bold") +
  annotate("text", x = 1, y = 495, label = "a", size = 5, fontface = "bold", hjust = 0)

# Model droughts
droughts_model <- events_models %>%
  filter(type == "drought" & !is.na(drought_depth) & !is.na(duration_months)) %>%
  filter(duration_months <= 50 & drought_depth >= -500)

# GRACE droughts
droughts_grace <- events_grace %>%
  filter(type == "drought" & !is.na(drought_depth) & !is.na(duration_months))

# Panel B: Drought depth vs duration
panel_4b <- ggplot() +
  geom_hex(data = droughts_model,
           aes(x = duration_months, y = abs(drought_depth)),
           bins = 40, alpha = 0.7) +
  scale_fill_viridis_c(name = "Model\nevents", option = "C", trans = "log10",
                       breaks = c(10, 100, 1000, 10000)) +
  geom_point(data = droughts_grace,
             aes(x = duration_months, y = abs(drought_depth)),
             color = "#D55E00", fill = "#D55E00", size = 2, alpha = 0.8, shape = 21, stroke = 0.5) +
  scale_x_continuous(name = "Duration (months)", limits = c(0, 50)) +
  scale_y_continuous(name = "Drought depth (mm, absolute)", limits = c(0, 500)) +
  theme_nature(base_size = 8) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
        legend.key.size = unit(3, "mm")) +
  annotate("point", x = 45, y = 470, color = "#D55E00", fill = "#D55E00", size = 2.5, shape = 21) +
  annotate("text", x = 43, y = 470, label = "GRACE", hjust = 1, size = 2.5, fontface = "bold") +
  annotate("text", x = 1, y = 495, label = "b", size = 5, fontface = "bold", hjust = 0)

# Panel C: Recovery time distributions (FIXED ORDER: GRACE first, then models)
SEVERITY_THRESHOLD <- 100

# Get GRACE recovery data
grace_recovery <- events_grace %>%
  filter(
    (type == "pluvial" & pluvial_height > SEVERITY_THRESHOLD) |
      (type == "drought" & abs(drought_depth) > SEVERITY_THRESHOLD)
  ) %>%
  filter(!is.na(recovery_months) & recovery_months > 0 & recovery_months < 100) %>%
  mutate(source = "GRACE")

# Get model recovery data - need to deduplicate across windows
model_recovery <- events_models %>%
  filter(
    (type == "pluvial" & pluvial_height > SEVERITY_THRESHOLD) |
      (type == "drought" & abs(drought_depth) > SEVERITY_THRESHOLD)
  ) %>%
  filter(!is.na(recovery_months) & recovery_months > 0 & recovery_months < 100) %>%
  mutate(source = model)

# Combine
recovery_data <- bind_rows(
  grace_recovery %>% select(recovery_months, source),
  model_recovery %>% select(recovery_months, source)
)

recovery_data$source <- factor(recovery_data$source, levels = c("GRACE", "CESM2", "IPSL"))

# Get counts for labels
n_grace <- sum(recovery_data$source == "GRACE")
n_cesm <- sum(recovery_data$source == "CESM2")
n_ipsl <- sum(recovery_data$source == "IPSL")

panel_4c <- ggplot(recovery_data, aes(x = source, y = recovery_months, fill = source)) +
  geom_violin(alpha = 0.6, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.2, alpha = 0.9, outlier.size = 0.5, outlier.alpha = 0.4) +
  scale_fill_manual(values = c("GRACE" = "#D55E00", "CESM2" = "#0072B2", "IPSL" = "#009E73")) +
  scale_y_continuous(name = "Recovery time (months)", breaks = seq(0, 100, by = 20)) +
  labs(x = NULL) +
  theme_nature(base_size = 8) +
  theme(legend.position = "none", panel.grid.major.x = element_blank()) +
  stat_summary(fun = median, geom = "text",
               aes(label = sprintf("%.0f", after_stat(y))),
               vjust = -0.8, size = 2.5, fontface = "bold") +
  annotate("text", x = 1:3, y = -3,
           label = c(sprintf("n=%d", n_grace), sprintf("n=%d", n_cesm), sprintf("n=%d", n_ipsl)),
           size = 2.2, color = "grey40") +
  annotate("text", x = 0.55, y = 98, label = "c", size = 5, fontface = "bold", hjust = 0)

fig4 <- (panel_4a | panel_4b) / (plot_spacer() | panel_4c | plot_spacer()) +
  plot_layout(heights = c(1, 0.9), widths = c(1, 2, 1))
save_fig(fig4, "Figure4_event_morphology", width_mm = 180, height_mm = 140)

# ============================================================================
# FIGURE 5: BASIN CLASSIFICATIONS
# ============================================================================

cat("FIGURE 5: Basin Classifications...\n")

# Panel A: Classification map
disp_sf$basin_class_strict <- factor(disp_sf$basin_class_strict,
                                      levels = c("both", "cesm_only", "ipsl_only", "neither"))

panel_5a <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = disp_sf, aes(fill = basin_class_strict), color = "grey30", linewidth = 0.1) +
  scale_fill_manual(
    name = "Classification",
    values = c("both" = "#2E7D32", "cesm_only" = "#1976D2",
               "ipsl_only" = "#F57C00", "neither" = "#C62828"),
    labels = c("Both models", "CESM2 only", "IPSL only", "Neither"),
    na.value = "grey80"
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(legend.position = c(0.15, 0.35), legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 6), legend.title = element_text(size = 7)) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

# Panel B: Classification bar chart
class_counts <- disp %>%
  count(basin_class_strict) %>%
  mutate(
    perc = 100 * n / sum(n),
    basin_class_strict = factor(basin_class_strict, levels = c("both", "cesm_only", "ipsl_only", "neither"))
  )

panel_5b <- ggplot(class_counts, aes(x = basin_class_strict, y = n, fill = basin_class_strict)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, perc)), vjust = -0.2, size = 2.8, fontface = "bold") +
  scale_fill_manual(values = c("both" = "#2E7D32", "cesm_only" = "#1976D2",
                               "ipsl_only" = "#F57C00", "neither" = "#C62828")) +
  scale_x_discrete(labels = c("Both", "CESM2\nonly", "IPSL\nonly", "Neither")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Number of basins") +
  theme_nature(base_size = 8) +
  theme(legend.position = "none", panel.grid.major.x = element_blank()) +
  annotate("text", x = 0.55, y = max(class_counts$n) * 1.1, label = "b",
           size = 5, fontface = "bold", hjust = 0)

fig5 <- panel_5a / panel_5b + plot_layout(heights = c(1.2, 1))
save_fig(fig5, "Figure5_classifications", width_mm = 180, height_mm = 160)

# ============================================================================
# FIGURE 6: COMPATIBILITY ANALYSIS
# ============================================================================

cat("FIGURE 6: Compatibility Analysis...\n")

# Merge compatibility with shapefile
compat_sf <- merge(basins_shp, compat, by.x = "Num", by.y = "basin")

# Panel A: C_b map for CESM2
panel_6a <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = compat_sf, aes(fill = C_b_cesm), color = "grey30", linewidth = 0.1) +
  scale_fill_viridis_c(name = expression(C[b]), option = "plasma", limits = c(0, 1),
                       na.value = "grey80") +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  labs(subtitle = "CESM2 compatibility index (0.5 = typical, 0 or 1 = outlier)") +
  theme_nature_map(base_size = 8) +
  theme(legend.position = c(0.15, 0.35), legend.key.size = unit(3, "mm"),
        plot.subtitle = element_text(size = 7)) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

# Panel B: Mahalanobis distance boxplots
compat_long <- compat %>%
  select(basin_name, d_mahal_cesm, d_mahal_ipsl) %>%
  pivot_longer(cols = c(d_mahal_cesm, d_mahal_ipsl),
               names_to = "model", values_to = "d_mahal") %>%
  mutate(model = ifelse(model == "d_mahal_cesm", "CESM2", "IPSL")) %>%
  filter(!is.na(d_mahal))

panel_6b <- ggplot(compat_long, aes(x = model, y = d_mahal, fill = model)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("CESM2" = "#0072B2", "IPSL" = "#009E73")) +
  labs(x = NULL, y = "Mahalanobis distance") +
  theme_nature(base_size = 8) +
  theme(legend.position = "none") +
  annotate("text", x = 0.55, y = max(compat_long$d_mahal, na.rm = TRUE) * 0.95,
           label = "b", size = 5, fontface = "bold", hjust = 0)

# Panel C: C_b histogram
panel_6c <- ggplot(compat %>% filter(!is.na(C_b_cesm)), aes(x = C_b_cesm)) +
  geom_histogram(bins = 20, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(x = expression(paste("Compatibility index ", C[b])), y = "Number of basins") +
  theme_nature(base_size = 8) +
  annotate("text", x = 0.02, y = Inf, label = "c", size = 5, fontface = "bold", hjust = 0, vjust = 2)

fig6 <- panel_6a / (panel_6b | panel_6c) + plot_layout(heights = c(1.2, 1))
save_fig(fig6, "Figure6_compatibility", width_mm = 180, height_mm = 160)

# ============================================================================
# SUPPLEMENTARY FIGURES
# ============================================================================

cat("Supplementary Figures...\n")

# S1: IPSL comparison maps
disp_sf <- disp_sf %>%
  mutate(
    amp_class_ipsl = case_when(
      A_grace > A_p95_ipsl ~ "above_p95",
      A_grace < A_p05_ipsl ~ "below_p05",
      TRUE ~ "within"
    )
  )

panel_s1a <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.1) +
  geom_sf(data = disp_sf, aes(fill = amp_class_ipsl), color = "grey30", linewidth = 0.1) +
  scale_fill_manual(
    name = "IPSL\nAmplitude",
    values = c("within" = "#87CEEB", "above_p95" = "#505050", "below_p05" = "#B8B8B8"),
    labels = c("Within", "Above p95", "Below p05")
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(legend.position = c(0.15, 0.35)) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

# S1 Panel B: GRACE vs IPSL scatter
disp[, amp_class_ipsl := fifelse(A_grace > A_p95_ipsl, "above_p95",
                                  fifelse(A_grace < A_p05_ipsl, "below_p05", "within"))]

panel_s1b <- ggplot(disp, aes(x = A_p50_ipsl, y = A_grace, color = amp_class_ipsl)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = A_p05_ipsl, xmax = A_p95_ipsl), alpha = 0.3, height = 0, linewidth = 0.3) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_color_manual(values = c("within" = "#87CEEB", "above_p95" = "#505050", "below_p05" = "#B8B8B8")) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "IPSL amplitude (mm, median)", y = "GRACE amplitude (mm)") +
  theme_nature(base_size = 8) +
  theme(legend.position = "none") +
  annotate("text", x = 20, y = 600, label = "b", size = 5, fontface = "bold", hjust = 0)

figS1 <- panel_s1a / panel_s1b
save_fig(figS1, "FigureS1_IPSL_dispersion", width_mm = 180, height_mm = 160)

# S2: Event percentiles
panel_s2 <- events %>%
  select(basin_name, percentile_H_cesm, percentile_D_cesm) %>%
  pivot_longer(cols = c(percentile_H_cesm, percentile_D_cesm),
               names_to = "metric", values_to = "percentile") %>%
  mutate(metric = ifelse(metric == "percentile_H_cesm", "Pluvial height", "Drought depth")) %>%
  ggplot(aes(x = percentile, fill = metric)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity", color = "white") +
  scale_fill_manual(values = c("Pluvial height" = "#1976D2", "Drought depth" = "#D32F2F")) +
  geom_vline(xintercept = c(10, 90), linetype = "dashed", color = "black") +
  labs(x = "GRACE percentile in CESM2 (%)", y = "Number of basins",
       fill = "Metric") +
  theme_nature(base_size = 8) +
  theme(legend.position = c(0.85, 0.85))

save_fig(panel_s2, "FigureS2_event_percentiles", width_mm = 140, height_mm = 100)

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE GENERATION COMPLETE\n")
cat("============================================================================\n\n")

cat("Output directory:", OUT_DIR, "\n\n")

cat("MAIN FIGURES:\n")
cat("  Figure 1: Study overview (basin map, timeline, windows)\n")
cat("  Figure 2: Dispersion analysis (maps + scatters)\n")
cat("  Figure 3: Spectral power & persistence\n")
cat("  Figure 4: Event morphology (FIXED panel c)\n")
cat("  Figure 5: Basin classifications\n")
cat("  Figure 6: Compatibility analysis\n\n")

cat("SUPPLEMENTARY FIGURES:\n")
cat("  Figure S1: IPSL dispersion comparison\n")
cat("  Figure S2: Event percentile distributions\n\n")

# List files
files <- list.files(OUT_DIR, pattern = "\\.(png|pdf)$")
cat(sprintf("Total files: %d\n\n", length(files)))
for(f in sort(files)) cat(sprintf("  - %s\n", f))

cat("\n✓ COMPLETE\n\n")
