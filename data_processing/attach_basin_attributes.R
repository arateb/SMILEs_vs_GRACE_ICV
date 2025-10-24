# ==============================================================================
# Function: attach_basin_attributes
# ==============================================================================
# Original name: attach_attrs
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Helper function
#
# ==============================================================================

attach_basin_attributes <- function(Wave, Gattrs) {
  Wave_full <- Wave
  n_rows <- nrow(Wave)
  
  for (col in names(Gattrs)) {
    Wave_full[[col]] <- rep(Gattrs[[col]][1], n_rows)
  }
  
  return(Wave_full)
}
