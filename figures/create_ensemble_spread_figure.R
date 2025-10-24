# ==============================================================================
# Function: create_ensemble_spread_figure
# ==============================================================================
# Original name: make_figure7# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 7: WAVELET DOMINANT MODES (if wave_results provided)
# =============================================================================

#
# ==============================================================================

create_ensemble_spread_figure <- function(wave_results, basins_sf) {
  
  cat("Creating Figure 7: Wavelet Modes...\n")
  
  if (is.null(wave_results) || is.null(wave_results$wave_q1_summary)) {
    cat("  ⚠ Skipping Figure 7: No wavelet data provided\n")
    return(NULL)
  }
  
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = "+proj=robin")
  basins_robin <- basins_sf %>% st_transform(crs = "+proj=robin")
  
  spatial_grace <- basins_robin %>%
    left_join(wave_results$wave_q1_summary, by = c("bd_ID" = "ID"))
  
  # Panel A: Primary Period
  p7a <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_grace, aes(fill = primary_period), color = "grey40", size = 0.1) +
    scale_fill_gradientn(colors = earth_colors$amplitude, name = "Period\n(years)",
                        limits = c(0, 10), oob = squish) +
    coord_sf(datum = NA) +
    labs(title = "a. GRACE dominant period") +
    theme_nature_map()
  
  # Panel B: Primary Power
  p7b <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_grace, aes(fill = primary_power), color = "grey40", size = 0.1) +
    scale_fill_gradientn(colors = earth_colors$water, name = "Power",
                        trans = "log10") +
    coord_sf(datum = NA) +
    labs(title = "b. GRACE dominant power") +
    theme_nature_map()
  
  # Panel C: Period classification
  p7c <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_grace, aes(fill = primary_class), color = "grey40", size = 0.1) +
    scale_fill_manual(values = c(annual = "#E41A1C", biennial = "#377EB8",
                                 triennial = "#4DAF4A", multiyear = "#984EA3"),
                     name = "Class", na.value = "grey80") +
    coord_sf(datum = NA) +
    labs(title = "c. Period classification") +
    theme_nature_map()
  
  # Panel D: Period histogram
  p7d <- ggplot(wave_results$wave_q1_summary, aes(x = primary_period)) +
    geom_histogram(binwidth = 0.5, fill = "#377EB8", color = "grey40") +
    geom_vline(xintercept = c(1, 2, 3), linetype = "dashed", color = "red") +
    labs(title = "d. Period distribution", x = "Period (years)", y = "Count") +
    theme_nature()
  
  fig7 <- (p7a + p7b) / (p7c + p7d)
  
  cat("  ✓ Figure 7 complete\n")
  return(fig7)
}
