# ==============================================================================
# Function: classify_temporal_period
# ==============================================================================
# Original name: classify_period
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#######  Wavelete analaysis the clock of the  global TWS  ###########
# =============================================================================
# WAVELET ANALYSIS FOR STL-FILTERED CLIMATE DATA
# Climate period classification
#
# ==============================================================================

classify_temporal_period <- function(period_years) {
  case_when(
    period_years >= 2 & period_years < 4   ~ "ENSO-core (2-4y)",
    period_years >= 4 & period_years < 8   ~ "ENSO-extended (4-8y)", 
    period_years >= 8 & period_years < 30  ~ "Decadal (8-30y)",
    period_years >= 30 & period_years < 100 ~ "Multi-decadal (30-100y)",
    period_years >= 100                    ~ "Centennial (100y+)",
    TRUE                                   ~ "Residual (<2y)"
  )
}
