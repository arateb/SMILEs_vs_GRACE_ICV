# ============================================================================
# GRACE vs CESM2 Ensemble: Statistical Comparison Plots
# ============================================================================
# Author:  Ashraf Rateb
# ============================================================================

# Load required libraries
library(tidyverse)
library(ggthemes)
library(scales)
library(viridis)

# Load data
G <- readRDS('analysis/GGFo_vs_MMILEs_Comparison_UpdatedAug25.rds')
amplsd_aug <- G$updated_AMp_SD_Min_MAC

# Filter for CESM2 only
cesm_data <- amplsd_aug %>%
  filter(model_name == "CESM2") %>%
  mutate(
    # Create performance indicator: is GRACE inside 5-95% envelope?
    amp_inside = (amp_grace >= q5_amp) & (amp_grace <= q95_amp),
    sd_inside = (sd_grace >= q5_sd) & (sd_grace <= q95_sd),
    min_inside = (min_grace >= q5_min) & (min_grace <= q95_min),
    max_inside = (max_grace >= q5_max) & (max_grace <= q95_max)
  )

# ============================================================================
# DEFINE BASIN CATEGORIES
# ============================================================================

# Category 1: Top 50 irrigated basins
top50_irrigated <- cesm_data %>%
  arrange(desc(Irrig_pct)) %>%
  slice_head(n = 50) %>%
  mutate(category = "Top 50 Irrigated")

# Category 2: Humid only (Arid == "H")
humid_basins <- cesm_data %>%
  filter(Arid == "H") %>%
  mutate(category = "Humid (H)")

# Category 3: Arid only (Arid == "A")
arid_basins <- cesm_data %>%
  filter(Arid == "A") %>%
  mutate(category = "Arid (A)")

# Category 4: Semi-Arid only (Arid == "SA")
semiarid_basins <- cesm_data %>%
  filter(Arid == "SA") %>%
  mutate(category = "Semi-Arid (SA)")

# ============================================================================
# PLOTTING FUNCTION
# ============================================================================

plot_ensemble_vs_grace <- function(data,
                                   metric = "amp",
                                   category_name = "All Basins",
                                   metric_label = "Amplitude",
                                   output_prefix = "amplitude_all",
                                   top_n = 50,
                                   order_by = "Irrig_pct") {

  # Define column names based on metric
  grace_col <- paste0(metric, "_grace")
  q5_col <- paste0("q5_", metric)
  q50_col <- paste0("q50_", metric)
  q95_col <- paste0("q95_", metric)
  inside_col <- paste0(metric, "_inside")

  # Prepare data: select top N basins based on order_by
  if (order_by == "performance") {
    # Order by how far outside the envelope (absolute distance to median)
    plot_data <- data %>%
      mutate(
        distance = abs(.data[[grace_col]] - .data[[q50_col]]) / .data[[q50_col]]
      ) %>%
      arrange(desc(distance)) %>%
      slice_head(n = top_n)
  } else if (order_by == "Irrig_pct") {
    # Already ordered by irrigation
    plot_data <- data %>%
      slice_head(n = top_n)
  } else {
    # Default: order by the metric value
    plot_data <- plot_data %>%
      arrange(desc(.data[[grace_col]])) %>%
      slice_head(n = top_n)
  }

  # Create basin labels with irrigation percentage
  plot_data <- plot_data %>%
    mutate(
      basin_label = paste0(River, " (", round(Irrig_pct, 1), "%)"),
      basin_label = factor(basin_label, levels = rev(basin_label))
    )

  # Create plot
  p <- ggplot(plot_data, aes(y = basin_label)) +
    # Ensemble range (q5 to q95)
    geom_linerange(
      aes(xmin = .data[[q5_col]],
          xmax = .data[[q95_col]],
          color = .data[[inside_col]]),
      size = 1.5,
      alpha = 0.6
    ) +
    # Ensemble median (q50)
    geom_point(
      aes(x = .data[[q50_col]],
          color = .data[[inside_col]]),
      size = 3,
      shape = 21,
      fill = "white",
      stroke = 1.5
    ) +
    # GRACE observation
    geom_point(
      aes(x = .data[[grace_col]],
          fill = .data[[inside_col]]),
      size = 4,
      shape = 23,
      color = "black",
      stroke = 0.8
    ) +
    # Color scale
    scale_color_manual(
      values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
      labels = c("TRUE" = "GRACE inside 5-95%", "FALSE" = "GRACE outside 5-95%"),
      name = "Coverage"
    ) +
    scale_fill_manual(
      values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
      labels = c("TRUE" = "GRACE inside 5-95%", "FALSE" = "GRACE outside 5-95%"),
      name = "Coverage"
    ) +
    # Labels
    labs(
      title = paste0(category_name, ": ", metric_label),
      subtitle = paste0("GRACE observations vs CESM2 ensemble (n=", nrow(plot_data), " basins)"),
      x = paste0(metric_label, " (mm)"),
      y = "Basin (Irrigation %)",
      caption = "Line: CESM2 5-95% envelope | Circle: CESM2 median | Diamond: GRACE observation"
    ) +
    # Theme
    theme_clean(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      axis.title = element_text(face = "bold", size = 13),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(size = 12, hjust = 0, color = "gray30"),
      plot.caption = element_text(size = 9, hjust = 0, color = "gray50"),
      panel.grid.major.x = element_line(color = "gray90", size = 0.3),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    )

  # Save as PDF
  ggsave(
    filename = paste0("finalFigures/", output_prefix, ".pdf"),
    plot = p,
    width = 12,
    height = 10,
    dpi = 500,
    device = "pdf"
  )

  # Save as PNG
  ggsave(
    filename = paste0("finalFigures/", output_prefix, ".png"),
    plot = p,
    width = 12,
    height = 10,
    dpi = 500,
    device = "png"
  )

  cat("✓ Saved:", output_prefix, "\n")

  return(p)
}

# ============================================================================
# GENERATE ALL 16 PLOTS
# ============================================================================

cat("\n=================================================================\n")
cat("Generating 16 Statistical Plots: GRACE vs CESM2\n")
cat("=================================================================\n\n")

# Category 1: Top 50 Irrigated Basins (4 plots)
cat("Category 1: Top 50 Irrigated Basins\n")
cat("-----------------------------------------------------------------\n")

plot_ensemble_vs_grace(
  data = top50_irrigated,
  metric = "amp",
  category_name = "Top 50 Irrigated Basins",
  metric_label = "Amplitude",
  output_prefix = "amplitude_top50irrigated",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = top50_irrigated,
  metric = "sd",
  category_name = "Top 50 Irrigated Basins",
  metric_label = "Standard Deviation",
  output_prefix = "sd_top50irrigated",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = top50_irrigated,
  metric = "min",
  category_name = "Top 50 Irrigated Basins",
  metric_label = "Minimum (Drought Depth)",
  output_prefix = "min_top50irrigated",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = top50_irrigated,
  metric = "max",
  category_name = "Top 50 Irrigated Basins",
  metric_label = "Maximum (Pluvial Height)",
  output_prefix = "max_top50irrigated",
  top_n = 50,
  order_by = "Irrig_pct"
)

# Category 2: Humid Basins (4 plots)
cat("\nCategory 2: Humid Basins (Arid == H)\n")
cat("-----------------------------------------------------------------\n")

plot_ensemble_vs_grace(
  data = humid_basins,
  metric = "amp",
  category_name = "Humid Basins",
  metric_label = "Amplitude",
  output_prefix = "amplitude_humid",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = humid_basins,
  metric = "sd",
  category_name = "Humid Basins",
  metric_label = "Standard Deviation",
  output_prefix = "sd_humid",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = humid_basins,
  metric = "min",
  category_name = "Humid Basins",
  metric_label = "Minimum (Drought Depth)",
  output_prefix = "min_humid",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = humid_basins,
  metric = "max",
  category_name = "Humid Basins",
  metric_label = "Maximum (Pluvial Height)",
  output_prefix = "max_humid",
  top_n = 50,
  order_by = "Irrig_pct"
)

# Category 3: Arid Basins (4 plots)
cat("\nCategory 3: Arid Basins (Arid == A)\n")
cat("-----------------------------------------------------------------\n")

# Note: Only 28 arid basins, so we'll use all of them
plot_ensemble_vs_grace(
  data = arid_basins,
  metric = "amp",
  category_name = "Arid Basins",
  metric_label = "Amplitude",
  output_prefix = "amplitude_arid",
  top_n = nrow(arid_basins),
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = arid_basins,
  metric = "sd",
  category_name = "Arid Basins",
  metric_label = "Standard Deviation",
  output_prefix = "sd_arid",
  top_n = nrow(arid_basins),
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = arid_basins,
  metric = "min",
  category_name = "Arid Basins",
  metric_label = "Minimum (Drought Depth)",
  output_prefix = "min_arid",
  top_n = nrow(arid_basins),
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = arid_basins,
  metric = "max",
  category_name = "Arid Basins",
  metric_label = "Maximum (Pluvial Height)",
  output_prefix = "max_arid",
  top_n = nrow(arid_basins),
  order_by = "Irrig_pct"
)

# Category 4: Semi-Arid Basins (4 plots)
cat("\nCategory 4: Semi-Arid Basins (Arid == SA)\n")
cat("-----------------------------------------------------------------\n")

plot_ensemble_vs_grace(
  data = semiarid_basins,
  metric = "amp",
  category_name = "Semi-Arid Basins",
  metric_label = "Amplitude",
  output_prefix = "amplitude_semiarid",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = semiarid_basins,
  metric = "sd",
  category_name = "Semi-Arid Basins",
  metric_label = "Standard Deviation",
  output_prefix = "sd_semiarid",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = semiarid_basins,
  metric = "min",
  category_name = "Semi-Arid Basins",
  metric_label = "Minimum (Drought Depth)",
  output_prefix = "min_semiarid",
  top_n = 50,
  order_by = "Irrig_pct"
)

plot_ensemble_vs_grace(
  data = semiarid_basins,
  metric = "max",
  category_name = "Semi-Arid Basins",
  metric_label = "Maximum (Pluvial Height)",
  output_prefix = "max_semiarid",
  top_n = 50,
  order_by = "Irrig_pct"
)

cat("\n=================================================================\n")
cat("✓ ALL 16 PLOTS COMPLETED SUCCESSFULLY!\n")
cat("=================================================================\n")
cat("Output location: finalFigures/\n")
cat("Format: Both PDF and PNG at 500 DPI\n")
cat("Dimensions: 12 x 10 inches\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("Summary Statistics:\n")
cat("-----------------------------------------------------------------\n")

categories <- list(
  "Top 50 Irrigated" = top50_irrigated,
  "Humid (H)" = humid_basins,
  "Arid (A)" = arid_basins,
  "Semi-Arid (SA)" = semiarid_basins
)

for (cat_name in names(categories)) {
  cat_data <- categories[[cat_name]]

  cat("\n", cat_name, " (n=", nrow(cat_data), "):\n", sep = "")

  # Coverage statistics
  amp_coverage <- mean(cat_data$amp_inside, na.rm = TRUE) * 100
  sd_coverage <- mean(cat_data$sd_inside, na.rm = TRUE) * 100
  min_coverage <- mean(cat_data$min_inside, na.rm = TRUE) * 100
  max_coverage <- mean(cat_data$max_inside, na.rm = TRUE) * 100

  cat("  Amplitude coverage:  ", round(amp_coverage, 1), "%\n", sep = "")
  cat("  SD coverage:         ", round(sd_coverage, 1), "%\n", sep = "")
  cat("  Min coverage:        ", round(min_coverage, 1), "%\n", sep = "")
  cat("  Max coverage:        ", round(max_coverage, 1), "%\n", sep = "")
}

cat("\n=================================================================\n")
