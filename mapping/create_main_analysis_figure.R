# ==============================================================================
# Function: create_main_analysis_figure
# ==============================================================================
# Original name: create_figure1_clean
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# PART 4: FIGURE CREATION FUNCTIONS FOR YOUR RESULTS
# =============================================================================

#' FIGURE 1: Global Variability Maps
#
# ==============================================================================

create_main_analysis_figure <- function(all_results, shp) {
  
  cat("\nFIGURE 1: Global Variability Assessment\n")
  
  # Extract amplitude data - removes NAs automatically
  amp_result <- extract_and_merge_clean(all_results, shp, "amplitude_summary")
  amp_data <- amp_result$merged_shp
  
  if (nrow(amp_data) == 0) {
    warning("No amplitude data available")
    return(NULL)
  }
  
  # Separate by model
  cesm_data <- amp_data %>% filter(source == "CESM2")
  ipsl_data <- amp_data %>% filter(source == "IPSL")
  
  # Panel A: Observed variability
  p1a <- ggplot(cesm_data) +
    geom_sf(data = shp, fill = "grey90", color = "grey70", size = 0.1) +
    geom_sf(aes(fill = obs_sd), color = "grey50", size = 0.1) +
    scale_fill_viridis_c(
      name = expression(sigma[obs]~"(mm)"),
      option = "viridis",
      na.value = "transparent"
    ) +
    theme_nature_map() +
    add_panel_label("a")
  
  # Panel B: CESM2 model variability
  p1b <- ggplot(cesm_data) +
    geom_sf(data = shp, fill = "grey90", color = "grey70", size = 0.1) +
    geom_sf(aes(fill = model_sd), color = "grey50", size = 0.1) +
    scale_fill_viridis_c(
      name = expression(sigma[CESM2]~"(mm)"),
      option = "plasma",
      na.value = "transparent"
    ) +
    theme_nature_map() +
    add_panel_label("b")
  
  # Panel C: IPSL model variability
  p1c <- ggplot(ipsl_data) +
    geom_sf(data = shp, fill = "grey90", color = "grey70", size = 0.1) +
    geom_sf(aes(fill = model_sd), color = "grey50", size = 0.1) +
    scale_fill_viridis_c(
      name = expression(sigma[IPSL]~"(mm)"),
      option = "plasma",
      na.value = "transparent"
    ) +
    theme_nature_map() +
    add_panel_label("c")
  
  # Panel D: Ratio (Obs/Model)
  p1d <- ggplot(cesm_data) +
    geom_sf(data = shp, fill = "grey90", color = "grey70", size = 0.1) +
    geom_sf(aes(fill = ratio), color = "grey50", size = 0.1) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "#F7F7F7",
      high = "#B2182B",
      midpoint = 1,
      limits = c(0.5, 2),
      oob = scales::squish,
      name = "Obs/Model"
    ) +
    theme_nature_map() +
    add_panel_label("d")
  
  # Combine all panels
  combined <- grid.arrange(p1a, p1b, p1c, p1d, ncol = 2)
  
  return(combined)
}
