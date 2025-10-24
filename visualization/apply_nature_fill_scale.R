# ==============================================================================
# Function: apply_nature_fill_scale
# ==============================================================================
# Original name: scale_fill_nature
# Source file: core_tws_functions.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#
# ==============================================================================

apply_nature_fill_scale <- function(...) {
  discrete_scale("fill", "nature", palette = function(n) nature_colors[1:n], ...)
}
