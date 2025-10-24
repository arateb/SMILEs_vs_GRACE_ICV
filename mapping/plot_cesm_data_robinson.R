# ==============================================================================
# Function: plot_cesm_data_robinson
# ==============================================================================
# Original name: plot_cesm_robin
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#' Create CESM2 Robinson map with specific styling
#
# ==============================================================================

plot_cesm_data_robinson <- function(cesm_data,
                           ratio_limits = c(0.5, 2),
                           ratio_midpoint = 1,
                           title = "CESM2: SD ratio",
                           panel_label = NULL) {
  
  p <- plot_robin_map(
    data = cesm_data,
    fill_var = "ratio",
    scale_limits = ratio_limits,
    scale_midpoint = ratio_midpoint,
    scale_colors = nature_color_schemes$RdBu,
    scale_name = "Obs/Model",
    scale_breaks = c(0.5, 0.75, 1, 1.5, 2),
    scale_labels = c("0.5", "0.75", "1", "1.5", "2"),
    world_fill = "white",
    world_colour = "grey40"
  ) +
    labs(title = title) +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
  
  if (!is.null(panel_label)) {
    p <- p + add_panel_label(panel_label)
  }
  
  return(p)
}
