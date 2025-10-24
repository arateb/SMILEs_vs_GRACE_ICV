# ==============================================================================
# Function: get_robinson_projection_crs
# ==============================================================================
# Original name: robin_crs
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# PART 3: MAP PROJECTION FUNCTIONS
# =============================================================================

#' Robinson projection CRS
#
# ==============================================================================

get_robinson_projection_crs <- function() {
  st_crs("+proj=robin")
}
