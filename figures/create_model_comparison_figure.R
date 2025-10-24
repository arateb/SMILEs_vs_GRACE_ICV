# ==============================================================================
# Function: create_model_comparison_figure
# ==============================================================================
# Original name: make_figure2# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 2: MODEL PERFORMANCE
# =============================================================================
#
# ==============================================================================

create_model_comparison_figure <- function(amplsd_aug, basins_sf) {
  cat("Creating Figure 2: Model Performance...\n")
  
  # Prepare data
  fig2_data <- amplsd_aug %>%
    mutate(
      amp_ratio = amp_grace / median_amp,
      sd_ratio = sd_grace / median_sd,
      amp_adequate = amp_ratio >= 0.8 & amp_ratio <= 1.2,
      sd_adequate = sd_ratio >= 0.8 & sd_ratio <= 1.2,
      performance = case_when(
        amp_adequate & sd_adequate ~ "Both adequate",
        amp_adequate ~ "Amplitude only",
        sd_adequate ~ "Variance only",
        TRUE ~ "Both inadequate"
      ),
      performance = factor(performance, 
                          levels = c("Both adequate", "Amplitude only", 
                                   "Variance only", "Both inadequate"))
    )
  
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = "+proj=robin")
  basins_robin <- basins_sf %>% st_transform(crs = "+proj=robin")
  
  spatial_cesm <- basins_robin %>%
    left_join(filter(fig2_data, model_name == "CESM2"), by = "bd_ID")
  
  spatial_ipsl <- basins_robin %>%
    left_join(filter(fig2_data, model_name == "IPSL"), by = "bd_ID")
  
  # UNIFIED COLOR SCHEME
  performance_colors <- c(
    "Both adequate" = "#4575b4",      # blue
    "Amplitude only" = "#abd9e9",     # light blue
    "Variance only" = "#fdae61",      # light orange
    "Both inadequate" = "#d73027"     # red
  )
  
  # Panel A: CESM2 Performance
  p2a <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_cesm, aes(fill = performance), color = "grey40", size = 0.1) +
    scale_fill_manual(values = performance_colors, name = "Performance") +
    coord_sf(datum = NA) +
    labs(title = "a. CESM2 performance") +
    theme_nature_map()
  
  # Panel B: IPSL Performance
  p2b <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_ipsl, aes(fill = performance), color = "grey40", size = 0.1) +
    scale_fill_manual(values = performance_colors, name = "Performance") +
    coord_sf(datum = NA) +
    labs(title = "b. IPSL performance") +
    theme_nature_map()
  
  # Panel C: CESM2 Scatter (IMPROVED)
  scatter_cesm <- fig2_data %>% filter(model_name == "CESM2")
  p2c <- ggplot(scatter_cesm, aes(x = amp_ratio, y = sd_ratio, color = performance)) +
    # Reference lines at 1.0
    geom_hline(yintercept = 1.0, color = "grey60", linetype = "solid", size = 0.3) +
    geom_vline(xintercept = 1.0, color = "grey60", linetype = "solid", size = 0.3) +
    # Adequacy box
    geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0.8, ymax = 1.2),
              fill = "grey90", color = "grey40", linetype = "dashed", 
              size = 0.4, inherit.aes = FALSE, alpha = 0.3) +
    # Points with improved styling
    geom_point(size = 2.5, alpha = 0.75, stroke = 0.3, shape = 21, 
               aes(fill = performance), color = "grey30") +
    scale_color_manual(values = performance_colors, guide = "none") +
    scale_fill_manual(values = performance_colors, name = "Performance") +
    scale_x_continuous(
      limits = c(0.3, 2.0),
      breaks = seq(0.4, 2.0, 0.4),
      expand = c(0.02, 0.02)
    ) +
    scale_y_continuous(
      limits = c(0.3, 2.5),
      breaks = seq(0.4, 2.4, 0.4),
      expand = c(0.02, 0.02)
    ) +
    labs(
      title = "c. CESM2 joint space",
      x = "Amplitude ratio",
      y = "Variance ratio"
    ) +
    theme_nature() +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(color = "grey92", size = 0.3),
      panel.grid.minor = element_blank()
    )
  
  # Panel D: IPSL Scatter (IMPROVED)
  scatter_ipsl <- fig2_data %>% filter(model_name == "IPSL")
  p2d <- ggplot(scatter_ipsl, aes(x = amp_ratio, y = sd_ratio, color = performance)) +
    # Reference lines at 1.0
    geom_hline(yintercept = 1.0, color = "grey60", linetype = "solid", size = 0.3) +
    geom_vline(xintercept = 1.0, color = "grey60", linetype = "solid", size = 0.3) +
    # Adequacy box
    geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0.8, ymax = 1.2),
              fill = "grey90", color = "grey40", linetype = "dashed", 
              size = 0.4, inherit.aes = FALSE, alpha = 0.3) +
    # Points with improved styling
    geom_point(size = 2.5, alpha = 0.75, stroke = 0.3, shape = 21, 
               aes(fill = performance), color = "grey30") +
    scale_color_manual(values = performance_colors, guide = "none") +
    scale_fill_manual(values = performance_colors, name = "Performance") +
    scale_x_continuous(
      limits = c(0.3, 2.0),
      breaks = seq(0.4, 2.0, 0.4),
      expand = c(0.02, 0.02)
    ) +
    scale_y_continuous(
      limits = c(0.3, 2.5),
      breaks = seq(0.4, 2.4, 0.4),
      expand = c(0.02, 0.02)
    ) +
    labs(
      title = "d. IPSL joint space",
      x = "Amplitude ratio",
      y = "Variance ratio"
    ) +
    theme_nature() +
    theme(
      panel.grid.major = element_line(color = "grey92", size = 0.3),
      panel.grid.minor = element_blank()
    )
  
  fig2 <- (p2a + p2b) / (p2c + p2d)
  
  cat("  ✓ Figure 2 complete\n")
  return(fig2)
}
