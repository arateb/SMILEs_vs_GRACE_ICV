# ==============================================================================
# Function: apply_nature_map_theme
# ==============================================================================
# Original name: theme_nature_map
# Source file: Claude_18_Qs_results_amplitude_Extremes.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# THEMES AND COLORS
# =============================================================================

#
# ==============================================================================

apply_nature_map_theme <- function() {
  theme_minimal(base_size = 9) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold"),
      legend.position = "right",
      legend.key.size = unit(4, "mm"),
      plot.margin = margin(2, 2, 2, 2, "mm")
    )
}
