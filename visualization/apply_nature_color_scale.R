# ==============================================================================
# Function: apply_nature_color_scale
# ==============================================================================
# Original name: scale_color_nature
# Source file: core_tws_functions.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#' Scale functions for Nature colors
#
# ==============================================================================

apply_nature_color_scale <- function(...) {
  discrete_scale("colour", "nature", palette = function(n) nature_colors[1:n], ...)
}
