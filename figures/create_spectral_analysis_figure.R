# ==============================================================================
# Function: create_spectral_analysis_figure
# ==============================================================================
# Original name: make_figure4# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 4: ENSEMBLE SPREAD
# =============================================================================
#
# ==============================================================================

create_spectral_analysis_figure <- function(amplsd_aug, basins_sf) {
  
  cat("Creating Figure 4: Ensemble Spread...\n")
  
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = "+proj=robin")
  basins_robin <- basins_sf %>% st_transform(crs = "+proj=robin")
  
  spatial_cesm <- basins_robin %>%
    left_join(filter(amplsd_aug, 
      model_name == "CESM2"), by = "bd_ID")
  
  spatial_ipsl <- basins_robin %>%
    left_join(filter(amplsd_aug, model_name == "IPSL"), by = "bd_ID")
  
  # Consistent color schemes
  model_colors <- c("CESM2" = "#6baed6", "IPSL" = "#fc8d59")
  
  # Spread colors (soft sequential)
  spread_colors <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", 
                    "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b")
  
  # CRPS colors (green=good, red=bad)
  crps_colors <- c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", 
                   "#fdae61", "#f46d43", "#d73027", "#a50026")
  
  # Panel A: CESM2 Spread
  p4a <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_cesm, aes(fill = IQR_amp), color = "grey50", size = 0.1) +
    scale_fill_gradientn(
      colors = spread_colors,
      name = "Spread\n(mm)",
      limits = c(0, 200),
      breaks = seq(0, 200, 50),
      oob = squish
    ) +
    coord_sf(datum = NA) +
    labs(title = "a. CESM2 ensemble spread") +
    theme_nature_map()
  
  # Panel B: IPSL Spread
  p4b <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_ipsl, aes(fill = IQR_amp), color = "grey50", size = 0.1) +
    scale_fill_gradientn(
      colors = spread_colors,
      name = "Spread\n(mm)",
      limits = c(0, 200),
      breaks = seq(0, 200, 50),
      oob = squish
    ) +
    coord_sf(datum = NA) +
    labs(title = "b. IPSL ensemble spread") +
    theme_nature_map()
  
  # Panel C: CESM2 CRPS
  p4c <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_cesm, aes(fill = nCRPS_amp), color = "grey50", size = 0.1) +
    scale_fill_gradientn(
      colors = rev(crps_colors),
      name = "nCRPS",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      oob = squish
    ) +
    coord_sf(datum = NA) +
    labs(title = "c. CESM2 skill (lower is better)") +
    theme_nature_map()
  
  # Panel D: IPSL CRPS
  p4d <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_ipsl, aes(fill = nCRPS_amp), color = "grey50", size = 0.1) +
    scale_fill_gradientn(
      colors = rev(crps_colors),
      name = "nCRPS",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      oob = squish
    ) +
    coord_sf(datum = NA) +
    labs(title = "d. IPSL skill (lower is better)") +
    theme_nature_map()
  
  # Panel E: Spread-skill relationship (IMPROVED)
  # Calculate correlations
  cor_stats <- amplsd_aug %>%
    group_by(model_name) %>%
    summarise(
      r = cor(IQR_amp, amp_grace, use = "complete.obs"),
      label = sprintf("r = %.2f", r),
      .groups = "drop"
    )
  
  p4e <- ggplot(amplsd_aug, aes(x = IQR_amp, y = amp_grace, color = model_name, fill = model_name)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", size = 0.4) +
    geom_point(alpha = 0.4, size = 1.8, shape = 21, stroke = 0.3, color = "grey30") +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.15, size = 0.6) +
    geom_text(data = cor_stats, 
              aes(label = label, color = model_name),
              x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
              size = 3, fontface = "italic", inherit.aes = FALSE, show.legend = FALSE) +
    scale_color_manual(values = model_colors, name = "Model") +
    scale_fill_manual(values = model_colors, name = "Model") +
    scale_x_continuous(
      limits = c(0, max(amplsd_aug$IQR_amp, na.rm = TRUE) * 1.05),
      expand = c(0.02, 0)
    ) +
    scale_y_continuous(
      limits = c(0, max(amplsd_aug$amp_grace, na.rm = TRUE) * 1.05),
      expand = c(0.02, 0)
    ) +
    labs(
      title = "e. Spread-skill relationship",
      x = "Ensemble spread (IQR, mm)",
      y = "Observed amplitude (mm)"
    ) +
    theme_nature() +
    theme(
      panel.grid.major = element_line(color = "grey90", size = 0.25),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "grey30", size = 0.3),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "white", color = "grey70", size = 0.3)
    )
  
  # Panel F: CRPS distribution (IMPROVED with density)
  crps_summary <- amplsd_aug %>%
    group_by(model_name) %>%
    summarise(
      median = median(nCRPS_amp, na.rm = TRUE),
      mean = mean(nCRPS_amp, na.rm = TRUE),
      .groups = "drop"
    )
  
  p4f <- ggplot(amplsd_aug, aes(x = nCRPS_amp, fill = model_name, color = model_name)) +
    geom_density(alpha = 0.5, size = 0.6, adjust = 1.2) +
    geom_vline(data = crps_summary, aes(xintercept = median, color = model_name),
               linetype = "dashed", size = 0.5, show.legend = FALSE) +
    scale_fill_manual(values = model_colors, name = "Model") +
    scale_color_manual(values = model_colors, name = "Model") +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "f. Skill score distribution",
      x = "Normalized CRPS (lower is better)",
      y = "Density"
    ) +
    theme_nature() +
    theme(
      panel.grid.major = element_line(color = "grey90", size = 0.25),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "grey30", size = 0.3),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5)
    )
  
  fig4 <- (p4a + p4b) / (p4c + p4d) / (p4e + p4f)
  
  cat("  ✓ Figure 4 complete\n")
  return(fig4)
}
