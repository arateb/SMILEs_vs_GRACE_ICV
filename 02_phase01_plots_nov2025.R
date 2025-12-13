# ============================================================================
# PHASE 01 - TIME SERIES PLOTS - NOVEMBER 2025
# ============================================================================
# Basin-by-basin comparison: CESM2 vs IPSL vs GRACE-FO
# Author: Ashraf Rateb
# Date: 2025-11-18
# ============================================================================

library(tidyverse)
library(stlplus)
library(ggthemes)

# Create output directories
dir.create("final_TSPlots", showWarnings = FALSE, recursive = TRUE)
dir.create("final_TSPlots_GRACEera", showWarnings = FALSE, recursive = TRUE)

# Load enhanced clean dataset
cat("Loading Enhanced_GGFO_MMLEs_Nov2025.rds...\n")
data <- readRDS("data/Enhanced_GGFO_MMLEs_Nov2025.rds")

attrs <- data$attrs
G_CESM <- data$G_CESM
G_IPSL_GH <- data$G_IPSL_GH
G_IPSL_NAT <- data$G_IPSL_NAT
gfo_dtrend <- data$gfo_dtrend
date_cesm <- data$date_cesm
date_ipsl <- data$date_ipsl
dates_grace <- data$dates_grace

cat("✓ Data loaded\n\n")

# ============================================================================
# PREPROCESSING
# ============================================================================

cat("Preprocessing data...\n")

# Subset to 1900-2100 (CESM2) and 1900-2020 (IPSL)
idx_cesm <- which(format(date_cesm, "%Y") >= "1900")
G_CESM_subset <- G_CESM[, , idx_cesm]
date_cesm_subset <- date_cesm[idx_cesm]

idx_ipsl <- which(format(date_ipsl, "%Y") >= "1900")
G_IPSL_GH_subset <- G_IPSL_GH[, , idx_ipsl]
G_IPSL_NAT_subset <- G_IPSL_NAT[, , idx_ipsl]
date_ipsl_subset <- date_ipsl[idx_ipsl]

# Remove long-term mean and forced signal, then STL filter
cat("  Deforcing and filtering models...\n")

# CESM2
G_CESM_anom <- array(NA, dim = dim(G_CESM_subset))
for (b in 1:dim(G_CESM_subset)[1]) {
  for (m in 1:dim(G_CESM_subset)[2]) {
    ts <- G_CESM_subset[b, m, ]
    G_CESM_anom[b, m, ] <- ts - mean(ts, na.rm = TRUE)
  }
}

G_CESM_deforced <- array(NA, dim = dim(G_CESM_anom))
for (b in 1:dim(G_CESM_anom)[1]) {
  for (t in 1:dim(G_CESM_anom)[3]) {
    forced_t <- mean(G_CESM_anom[b, , t], na.rm = TRUE)
    G_CESM_deforced[b, , t] <- G_CESM_anom[b, , t] - forced_t
  }
}

# STL filter CESM2
G_CESM_filtered <- array(NA, dim = dim(G_CESM_deforced))
for (b in 1:dim(G_CESM_deforced)[1]) {
  if (b %% 50 == 0) cat("    CESM2 basin", b, "/", dim(G_CESM_deforced)[1], "\n")
  for (m in 1:dim(G_CESM_deforced)[2]) {
    ts_data <- G_CESM_deforced[b, m, ]
    if (all(is.na(ts_data))) next
    ts_obj <- ts(ts_data, frequency = 12, start = c(1900, 1))
    stl_fit <- stlplus(ts_obj, s.window = "periodic", t.window = 25, robust = TRUE)
    G_CESM_filtered[b, m, ] <- stl_fit$data$trend
  }
}

# IPSL
G_IPSL_GH_anom <- array(NA, dim = dim(G_IPSL_GH_subset))
for (b in 1:dim(G_IPSL_GH_subset)[1]) {
  for (m in 1:dim(G_IPSL_GH_subset)[2]) {
    ts <- G_IPSL_GH_subset[b, m, ]
    G_IPSL_GH_anom[b, m, ] <- ts - mean(ts, na.rm = TRUE)
  }
}

G_IPSL_NAT_anom <- array(NA, dim = dim(G_IPSL_NAT_subset))
for (b in 1:dim(G_IPSL_NAT_subset)[1]) {
  for (m in 1:dim(G_IPSL_NAT_subset)[2]) {
    ts <- G_IPSL_NAT_subset[b, m, ]
    G_IPSL_NAT_anom[b, m, ] <- ts - mean(ts, na.rm = TRUE)
  }
}

G_IPSL_GH_deforced <- array(NA, dim = dim(G_IPSL_GH_anom))
for (b in 1:dim(G_IPSL_GH_anom)[1]) {
  for (t in 1:dim(G_IPSL_GH_anom)[3]) {
    forced_t <- mean(G_IPSL_GH_anom[b, , t], na.rm = TRUE)
    G_IPSL_GH_deforced[b, , t] <- G_IPSL_GH_anom[b, , t] - forced_t
  }
}

G_IPSL_NAT_deforced <- array(NA, dim = dim(G_IPSL_NAT_anom))
for (b in 1:dim(G_IPSL_NAT_anom)[1]) {
  for (t in 1:dim(G_IPSL_NAT_anom)[3]) {
    forced_t <- mean(G_IPSL_NAT_anom[b, , t], na.rm = TRUE)
    G_IPSL_NAT_deforced[b, , t] <- G_IPSL_NAT_anom[b, , t] - forced_t
  }
}

# STL filter IPSL
G_IPSL_GH_filtered <- array(NA, dim = dim(G_IPSL_GH_deforced))
G_IPSL_NAT_filtered <- array(NA, dim = dim(G_IPSL_NAT_deforced))

for (b in 1:dim(G_IPSL_GH_deforced)[1]) {
  for (m in 1:dim(G_IPSL_GH_deforced)[2]) {
    ts_data <- G_IPSL_GH_deforced[b, m, ]
    if (all(is.na(ts_data))) next
    ts_obj <- ts(ts_data, frequency = 12, start = c(1900, 1))
    stl_fit <- stlplus(ts_obj, s.window = "periodic", t.window = 25, robust = TRUE)
    G_IPSL_GH_filtered[b, m, ] <- stl_fit$data$trend
  }
  
  for (m in 1:dim(G_IPSL_NAT_deforced)[2]) {
    ts_data <- G_IPSL_NAT_deforced[b, m, ]
    if (all(is.na(ts_data))) next
    ts_obj <- ts(ts_data, frequency = 12, start = c(1900, 1))
    stl_fit <- stlplus(ts_obj, s.window = "periodic", t.window = 25, robust = TRUE)
    G_IPSL_NAT_filtered[b, m, ] <- stl_fit$data$trend
  }
}

# Merge IPSL
G_IPSL_combined <- abind::abind(G_IPSL_GH_filtered, G_IPSL_NAT_filtered, along = 2)

# GRACE: Already filtered, use median and MAD
GRACE_median <- gfo_dtrend$median  # [time × basins]
GRACE_mad <- gfo_dtrend$mad

cat("✓ Preprocessing complete\n\n")

# ============================================================================
# GENERATE PLOTS FOR EACH BASIN
# ============================================================================

cat("Generating time series plots...\n")

for (basin_idx in 1:nrow(attrs)) {
  # Get basin info from attrs
  basin_name <- attrs$name[basin_idx]
  basin_id <- attrs$ID[basin_idx]
  basin_bd_id <- attrs$bd_id[basin_idx]
  basin_area <- attrs$area[basin_idx]
  
  cat(sprintf("  Basin %d/%d: %s\n", basin_idx, nrow(attrs), basin_name))
  
  # Extract data for this basin
  cesm_data <- G_CESM_filtered[basin_idx, , ]  # [80 members × time]
  ipsl_data <- G_IPSL_combined[basin_idx, , ]  # [18 members × time]
  grace_obs <- GRACE_median[, basin_idx]
  grace_unc <- GRACE_mad[, basin_idx]
  
  # Create data frames
  cesm_df <- expand.grid(
    date = date_cesm_subset,
    member = 1:dim(cesm_data)[1]
  ) %>%
    mutate(value = as.vector(t(cesm_data)))
  
  ipsl_df <- expand.grid(
    date = date_ipsl_subset,
    member = 1:dim(ipsl_data)[1]
  ) %>%
    mutate(value = as.vector(t(ipsl_data)))
  
  grace_df <- data.frame(
    date = dates_grace,
    median = grace_obs,
    lower = grace_obs - grace_unc,
    upper = grace_obs + grace_unc
  )
  
  # Compute envelopes
  cesm_env <- cesm_df %>%
    group_by(date) %>%
    summarise(
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  ipsl_env <- ipsl_df %>%
    group_by(date) %>%
    summarise(
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create plot
  p <- ggplot() +
    # GRACE era background - more prominent
    annotate("rect", xmin = as.Date("2002-04-01"), xmax = as.Date("2024-12-31"),
             ymin = -Inf, ymax = Inf, fill = "#FFF4E6", alpha = 0.8) +

    # CESM2 envelope (brighter blue)
    geom_ribbon(data = cesm_env, aes(x = date, ymin = min, ymax = max, fill = "CESM2"),
                alpha = 0.4) +
    geom_line(data = cesm_env, aes(x = date, y = median, color = "CESM2"),
              linewidth = 1) +

    # IPSL envelope (brighter red/orange)
    geom_ribbon(data = ipsl_env, aes(x = date, ymin = min, ymax = max, fill = "IPSL"),
                alpha = 0.4) +
    geom_line(data = ipsl_env, aes(x = date, y = median, color = "IPSL"),
              linewidth = 1) +

    # GRACE observations (dark with uncertainty)
    geom_ribbon(data = grace_df, aes(x = date, ymin = lower, ymax = upper, fill = "GRACE-FO"),
                alpha = 0.5) +
    geom_line(data = grace_df, aes(x = date, y = median, color = "GRACE-FO"),
              linewidth = 1.2) +

    # Manual color scales
    scale_fill_manual(
      name = NULL,
      values = c("CESM2" = "#4292C6", "IPSL" = "#FC8D59", "GRACE-FO" = "#636363"),
      breaks = c("CESM2", "IPSL", "GRACE-FO")
    ) +
    scale_color_manual(
      name = NULL,
      values = c("CESM2" = "#08519C", "IPSL" = "#D7301F", "GRACE-FO" = "#000000"),
      breaks = c("CESM2", "IPSL", "GRACE-FO")
    ) +

    # Styling
    scale_x_date(date_breaks = "20 years", date_labels = "%Y",
                 limits = c(as.Date("1900-01-01"), as.Date("2100-12-31"))) +
    labs(
      title = basin_name,
      x = "Years",
      y = "TWS Anomalies [mm]"
    ) +
    theme_clean(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    ) +
    guides(
      fill = guide_legend(override.aes = list(alpha = 0.5)),
      color = guide_legend(override.aes = list(linewidth = 1.5))
    )
  
  # Save plot with basin ID and name (high resolution PNG + PDF)
  basename <- sprintf("%03d_%s", basin_id, gsub(" ", "_", basin_name))

  # PNG version - high resolution
  ggsave(
    filename = sprintf("final_TSPlots/%s.png", basename),
    plot = p,
    width = 12,
    height = 6,
    dpi = 600,
    bg = "white"
  )

  # PDF version - vector graphics
  ggsave(
    filename = sprintf("final_TSPlots/%s.pdf", basename),
    plot = p,
    width = 12,
    height = 6,
    device = cairo_pdf
  )

  # =========================================================================
  # GRACE ERA PLOT (2002-2024 only)
  # =========================================================================

  # Filter data to GRACE era
  grace_start <- as.Date("2002-04-01")
  grace_end <- as.Date("2024-12-31")

  cesm_env_grace <- cesm_env %>% filter(date >= grace_start & date <= grace_end)
  ipsl_env_grace <- ipsl_env %>% filter(date >= grace_start & date <= grace_end)

  # Create GRACE-era focused plot
  p_grace <- ggplot() +
    # CESM2 envelope (brighter blue)
    geom_ribbon(data = cesm_env_grace, aes(x = date, ymin = min, ymax = max, fill = "CESM2"),
                alpha = 0.4) +
    geom_line(data = cesm_env_grace, aes(x = date, y = median, color = "CESM2"),
              linewidth = 1) +

    # IPSL envelope (brighter red/orange)
    geom_ribbon(data = ipsl_env_grace, aes(x = date, ymin = min, ymax = max, fill = "IPSL"),
                alpha = 0.4) +
    geom_line(data = ipsl_env_grace, aes(x = date, y = median, color = "IPSL"),
              linewidth = 1) +

    # GRACE observations (dark with uncertainty)
    geom_ribbon(data = grace_df, aes(x = date, ymin = lower, ymax = upper, fill = "GRACE-FO"),
                alpha = 0.5) +
    geom_line(data = grace_df, aes(x = date, y = median, color = "GRACE-FO"),
              linewidth = 1.2) +

    # Manual color scales
    scale_fill_manual(
      name = NULL,
      values = c("CESM2" = "#4292C6", "IPSL" = "#FC8D59", "GRACE-FO" = "#636363"),
      breaks = c("CESM2", "IPSL", "GRACE-FO")
    ) +
    scale_color_manual(
      name = NULL,
      values = c("CESM2" = "#08519C", "IPSL" = "#D7301F", "GRACE-FO" = "#000000"),
      breaks = c("CESM2", "IPSL", "GRACE-FO")
    ) +

    # Styling
    scale_x_date(date_breaks = "5 years", date_labels = "%Y",
                 limits = c(grace_start, grace_end)) +
    labs(
      title = sprintf("%s (GRACE Era: 2002-2024)", basin_name),
      x = "Years",
      y = "TWS Anomalies [mm]"
    ) +
    theme_clean(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    ) +
    guides(
      fill = guide_legend(override.aes = list(alpha = 0.5)),
      color = guide_legend(override.aes = list(linewidth = 1.5))
    )

  # Save GRACE-era plot
  ggsave(
    filename = sprintf("final_TSPlots_GRACEera/%s.png", basename),
    plot = p_grace,
    width = 12,
    height = 6,
    dpi = 600,
    bg = "white"
  )

  ggsave(
    filename = sprintf("final_TSPlots_GRACEera/%s.pdf", basename),
    plot = p_grace,
    width = 12,
    height = 6,
    device = cairo_pdf
  )
}

cat("\n✓ All", nrow(attrs), "plots saved to final_TSPlots/\n")
cat("✓ All", nrow(attrs), "GRACE-era plots saved to final_TSPlots_GRACEera/\n")
