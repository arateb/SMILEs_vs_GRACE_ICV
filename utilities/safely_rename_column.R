# ==============================================================================
# Function: safely_rename_column
# ==============================================================================
# Original name: safe_rename
# Source file: Claude_18_Qs_results_amplitude_Extremes.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# ====== helpers ======
#
# ==============================================================================

safely_rename_column <- function(d, old, new){
  if (old %in% names(d)) d <- d %>% dplyr::rename(!!new := all_of(old))
  d
}
