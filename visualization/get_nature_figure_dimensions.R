# ==============================================================================
# Function: get_nature_figure_dimensions
# ==============================================================================
# Original name: nature_figure_size
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#' Get Nature figure dimensions
#
# ==============================================================================

get_nature_figure_dimensions <- function(width_type = "single") {
  mm_to_in <- 1/25.4
  
  sizes <- list(
    single = list(
      width = NATURE_SPECS$single_width * mm_to_in, 
      height = NATURE_SPECS$single_width * mm_to_in
    ),
    double = list(
      width = NATURE_SPECS$double_width * mm_to_in, 
      height = NATURE_SPECS$max_height * mm_to_in
    ),
    full_width = list(
      width = NATURE_SPECS$double_width * mm_to_in, 
      height = 170 * mm_to_in
    )
  )
  
  return(sizes[[width_type]])
}
