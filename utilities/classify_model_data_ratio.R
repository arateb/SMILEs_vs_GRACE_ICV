# ==============================================================================
# Function: classify_model_data_ratio
# ==============================================================================
# Original name: classify_ratio
# Source file: Claude_18_Qs_results_amplitude_Extremes.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#
# ==============================================================================

classify_model_data_ratio <- function(x){
  case_when(
    is.na(x)            ~ NA_character_,
    x < 0.8             ~ "over-dispersive",
    x <= 1.2            ~ "adequate",
    x >  1.2            ~ "under-dispersive"
  )
}
