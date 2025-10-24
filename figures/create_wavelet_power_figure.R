# ==============================================================================
# Function: create_wavelet_power_figure
# ==============================================================================
# Original name: make_figure8# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 8: ENSEMBLE POSITION ANALYSIS (Cross-model comparison)
# =============================================================================

# =============================================================================
# FIGURE 8: ENSEMBLE POSITION ANALYSIS - BEAUTIFUL VERSION
# =============================================================================
#
# ==============================================================================

create_wavelet_power_figure <- function(q10_cross, basins_sf) {
  
  cat("Creating Figure 8: Ensemble Position Analysis...\n")
  
  if (is.null(q10_cross)) {
    cat("  ⚠ Skipping Figure 8: No q10_cross data provided\n")
    return(NULL)
  }
  
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(patchwork)
  
  # Gorgeous color schemes
  position_colors <- c(
    "Below ensemble" = "#d7191c",      # Red - obs too weak
    "Within ensemble" = "#1a9850",     # Green - captured!
    "Above ensemble" = "#2c7bb6"       # Blue - obs too strong
  )
  
  agreement_colors <- c(
    "Both capture" = "#1a9850",        # Dark green - both good
    "CESM2 only" = "#91bfdb",          # Light blue
    "IPSL only" = "#fc8d59",           # Light orange
    "Both miss" = "#762a83"            # Purple - both fail
  )
  
  # Prepare combined agreement data
  fig8_data <- q10_cross %>%
    mutate(
      # Amplitude agreement
      amp_agreement = case_when(
        amp_position_CESM2 == "Within (q5-q95)" & amp_position_IPSL == "Within (q5-q95)" ~ "Both capture",
        amp_position_CESM2 == "Within (q5-q95)" ~ "CESM2 only",
        amp_position_IPSL == "Within (q5-q95)" ~ "IPSL only",
        TRUE ~ "Both miss"
      ),
      # Variance agreement
      sd_agreement = case_when(
        sd_position_CESM2 == "Within (q5-q95)" & sd_position_IPSL == "Within (q5-q95)" ~ "Both capture",
        sd_position_CESM2 == "Within (q5-q95)" ~ "CESM2 only",
        sd_position_IPSL == "Within (q5-q95)" ~ "IPSL only",
        TRUE ~ "Both miss"
      ),
      # Simplified positions for mapping
      amp_cesm_simple = case_when(
        grepl("Below", amp_position_CESM2) ~ "Below ensemble",
        grepl("Within", amp_position_CESM2) ~ "Within ensemble",
        grepl("Above", amp_position_CESM2) ~ "Above ensemble"
      ),
      amp_ipsl_simple = case_when(
        grepl("Below", amp_position_IPSL) ~ "Below ensemble",
        grepl("Within", amp_position_IPSL) ~ "Within ensemble",
        grepl("Above", amp_position_IPSL) ~ "Above ensemble"
      ),
      sd_cesm_simple = case_when(
        grepl("Below", sd_position_CESM2) ~ "Below ensemble",
        grepl("Within", sd_position_CESM2) ~ "Within ensemble",
        grepl("Above", sd_position_CESM2) ~ "Above ensemble"
      ),
      sd_ipsl_simple = case_when(
        grepl("Below", sd_position_IPSL) ~ "Below ensemble",
        grepl("Within", sd_position_IPSL) ~ "Within ensemble",
        grepl("Above", sd_position_IPSL) ~ "Above ensemble"
      )
    )
  
  # Calculate comprehensive statistics
  amp_stats <- fig8_data %>%
    count(amp_agreement) %>%
    mutate(pct = n / sum(n) * 100)
  
  sd_stats <- fig8_data %>%
    count(sd_agreement) %>%
    mutate(pct = n / sum(n) * 100)
  
  cesm_amp_stats <- fig8_data %>%
    count(amp_cesm_simple) %>%
    mutate(pct = n / sum(n) * 100)
  
  ipsl_amp_stats <- fig8_data %>%
    count(amp_ipsl_simple) %>%
    mutate(pct = n / sum(n) * 100)
  
  cesm_sd_stats <- fig8_data %>%
    count(sd_cesm_simple) %>%
    mutate(pct = n / sum(n) * 100)
  
  ipsl_sd_stats <- fig8_data %>%
    count(sd_ipsl_simple) %>%
    mutate(pct = n / sum(n) * 100)
  
  # Prepare spatial data
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = "+proj=robin")
  basins_robin <- basins_sf %>% st_transform(crs = "+proj=robin")
  
  spatial_data <- basins_robin %>%
    left_join(fig8_data, by = "bd_ID")
  
  # Enhanced map-making function
  make_position_map <- function(spatial_df, fill_var, title_text, stats_df, 
                                stat_var, color_scheme, legend_title) {
    
    # Create elegant statistics box
    within_pct <- stats_df$pct[stats_df[[stat_var]] == "Within ensemble"]
    below_pct <- stats_df$pct[stats_df[[stat_var]] == "Below ensemble"]
    above_pct <- stats_df$pct[stats_df[[stat_var]] == "Above ensemble"]
    
    # Handle missing values
    if(length(within_pct) == 0) within_pct <- 0
    if(length(below_pct) == 0) below_pct <- 0
    if(length(above_pct) == 0) above_pct <- 0
    
    stats_text <- sprintf("✓ Captured: %.1f%%\n↓ Below: %.1f%%\n↑ Above: %.1f%%", 
                         within_pct, below_pct, above_pct)
    
    ggplot() +
      geom_sf(data = world, fill = "grey97", color = "grey80", size = 0.15) +
      geom_sf(data = spatial_df, aes(fill = .data[[fill_var]]), 
              color = "grey50", size = 0.15, alpha = 0.9) +
      scale_fill_manual(
        values = color_scheme,
        name = legend_title,
        na.value = "grey85",
        drop = FALSE
      ) +
      coord_sf(datum = NA) +
      # Stats annotation box
      annotate("label",
               x = -Inf, y = Inf,
               label = stats_text,
               hjust = -0.05, vjust = 1.1,
               size = 3,
               color = "grey15",
               fill = alpha("white", 0.92),
               label.size = 0.3,
               lineheight = 0.95,
               fontface = "plain") +
      labs(title = title_text) +
      theme_nature_map() +
      theme(
        plot.title = element_text(size = 11, face = "bold", color = "grey10"),
        legend.position = "right",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, "lines"),
        legend.background = element_rect(fill = "white", color = "grey70", size = 0.3),
        panel.background = element_rect(fill = "#f0f8ff", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  }
  
  # Enhanced agreement map function
  make_agreement_map <- function(spatial_df, fill_var, title_text, 
                                 stats_df, stat_var, color_scheme) {
    
    # Extract percentages
    both_pct <- stats_df$pct[stats_df[[stat_var]] == "Both capture"]
    miss_pct <- stats_df$pct[stats_df[[stat_var]] == "Both miss"]
    cesm_pct <- stats_df$pct[stats_df[[stat_var]] == "CESM2 only"]
    ipsl_pct <- stats_df$pct[stats_df[[stat_var]] == "IPSL only"]
    
    # Handle missing
    if(length(both_pct) == 0) both_pct <- 0
    if(length(miss_pct) == 0) miss_pct <- 0
    if(length(cesm_pct) == 0) cesm_pct <- 0
    if(length(ipsl_pct) == 0) ipsl_pct <- 0
    
    stats_text <- sprintf("✓✓ Both: %.1f%%\n✗✗ Neither: %.1f%%\nC only: %.1f%%\nI only: %.1f%%", 
                         both_pct, miss_pct, cesm_pct, ipsl_pct)
    
    ggplot() +
      geom_sf(data = world, fill = "grey97", color = "grey80", size = 0.15) +
      geom_sf(data = spatial_df, aes(fill = .data[[fill_var]]), 
              color = "grey50", size = 0.15, alpha = 0.9) +
      scale_fill_manual(
        values = color_scheme,
        name = "Agreement",
        na.value = "grey85",
        drop = FALSE
      ) +
      coord_sf(datum = NA) +
      # Stats box
      annotate("label",
               x = -Inf, y = Inf,
               label = stats_text,
               hjust = -0.05, vjust = 1.1,
               size = 3,
               color = "grey15",
               fill = alpha("white", 0.92),
               label.size = 0.3,
               lineheight = 0.95,
               fontface = "plain") +
      labs(title = title_text) +
      theme_nature_map() +
      theme(
        plot.title = element_text(size = 11, face = "bold", color = "grey10"),
        legend.position = "right",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.9, "lines"),
        legend.background = element_rect(fill = "white", color = "grey70", size = 0.3),
        panel.background = element_rect(fill = "#f0f8ff", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  }
  
  # Create all panels
  p8a <- make_position_map(
    spatial_data, "amp_cesm_simple", 
    "a. CESM2 amplitude position",
    cesm_amp_stats, "amp_cesm_simple",
    position_colors, "GRACE vs Ensemble"
  )
  
  p8b <- make_position_map(
    spatial_data, "amp_ipsl_simple",
    "b. IPSL amplitude position",
    ipsl_amp_stats, "amp_ipsl_simple",
    position_colors, "GRACE vs Ensemble"
  )
  
  p8c <- make_agreement_map(
    spatial_data, "amp_agreement",
    "c. Amplitude: Cross-model agreement",
    amp_stats, "amp_agreement",
    agreement_colors
  )
  
  p8d <- make_position_map(
    spatial_data, "sd_cesm_simple",
    "d. CESM2 variance position",
    cesm_sd_stats, "sd_cesm_simple",
    position_colors, "GRACE vs Ensemble"
  )
  
  p8e <- make_position_map(
    spatial_data, "sd_ipsl_simple",
    "e. IPSL variance position",
    ipsl_sd_stats, "sd_ipsl_simple",
    position_colors, "GRACE vs Ensemble"
  )
  
  p8f <- make_agreement_map(
    spatial_data, "sd_agreement",
    "f. Variance: Cross-model agreement",
    sd_stats, "sd_agreement",
    agreement_colors
  )
  
  # Add summary bar charts at bottom
  # Panel G: Amplitude summary
  amp_summary <- bind_rows(
    cesm_amp_stats %>% mutate(model = "CESM2", metric = "Position"),
    ipsl_amp_stats %>% mutate(model = "IPSL", metric = "Position")
  ) %>%
    rename(category = amp_cesm_simple) %>%
    bind_rows(
      amp_stats %>% mutate(model = "Agreement", metric = "Cross-model") %>%
        rename(category = amp_agreement)
    )
  
  p8g <- ggplot(amp_summary, aes(x = model, y = pct, fill = category)) +
    geom_col(position = "stack", width = 0.7, color = "white", size = 0.4) +
    geom_text(aes(label = sprintf("%.0f%%", pct)),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white", fontface = "bold") +
    scale_fill_manual(
      values = c(position_colors, agreement_colors),
      name = NULL
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      expand = c(0, 0)
    ) +
    labs(
      title = "g. Amplitude summary",
      x = NULL,
      y = "Percentage (%)"
    ) +
    theme_nature() +
    theme(
      panel.grid.major.y = element_line(color = "grey90", size = 0.25),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_line(color = "grey50", size = 0.3),
      axis.ticks.x = element_blank(),
      panel.border = element_rect(color = "grey65", fill = NA, size = 0.4),
      plot.title = element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.7, "lines")
    )
  
  # Panel H: Variance summary
  sd_summary <- bind_rows(
    cesm_sd_stats %>% mutate(model = "CESM2", metric = "Position"),
    ipsl_sd_stats %>% mutate(model = "IPSL", metric = "Position")
  ) %>%
    rename(category = sd_cesm_simple) %>%
    bind_rows(
      sd_stats %>% mutate(model = "Agreement", metric = "Cross-model") %>%
        rename(category = sd_agreement)
    )
  
  p8h <- ggplot(sd_summary, aes(x = model, y = pct, fill = category)) +
    geom_col(position = "stack", width = 0.7, color = "white", size = 0.4) +
    geom_text(aes(label = sprintf("%.0f%%", pct)),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white", fontface = "bold") +
    scale_fill_manual(
      values = c(position_colors, agreement_colors),
      name = NULL
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      expand = c(0, 0)
    ) +
    labs(
      title = "h. Variance summary",
      x = NULL,
      y = "Percentage (%)"
    ) +
    theme_nature() +
    theme(
      panel.grid.major.y = element_line(color = "grey90", size = 0.25),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_line(color = "grey50", size = 0.3),
      axis.ticks.x = element_blank(),
      panel.border = element_rect(color = "grey65", fill = NA, size = 0.4),
      plot.title = element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.7, "lines")
    )
  
  fig8 <- (p8a + p8b + p8c) / (p8d + p8e + p8f) / (p8g + p8h)
  
  cat("  ✓ Figure 8 complete\n")
  return(fig8)
}
