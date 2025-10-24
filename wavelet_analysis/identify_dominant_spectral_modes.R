# ==============================================================================
# Function: identify_dominant_spectral_modes
# ==============================================================================
# Original name: analyze_dominant_modes
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Q1: Dominant modes
#
# ==============================================================================

identify_dominant_spectral_modes <- function(Wave_full) {
  obs_modes <- Wave_full %>% 
    filter(data == "GRACE(-FO)") %>%
    arrange(rank) %>%
    dplyr::select(rank, period, power, period_class)
  
  dominant_summary <- obs_modes %>%
    summarise(
      primary_period = period[1],
      primary_class = period_class[1],
      primary_power = power[1],
      secondary_period = if(n() > 1) period[2] else NA,
      secondary_class = if(n() > 1) period_class[2] else NA,
      total_modes = n()
    )
  interpretation <- case_when(
    dominant_summary$primary_class == "Centennial (100y+)" ~ 
      "Centennial-scale variability dominates - climate regime shifts",
    dominant_summary$primary_class == "Multi-decadal (30-100y)" ~ 
      "Multi-decadal mode dominates - Atlantic/Pacific basin oscillations (AMO/PDO)",
    dominant_summary$primary_class == "Decadal (8-30y)" ~ 
      "Decadal mode dominates - PDO/AMO cycles, possible ENSO modulation",
    dominant_summary$primary_class == "ENSO-extended (4-8y)" ~ 
      "Extended-ENSO mode dominates - La Niña persistence, ENSO asymmetry",
    dominant_summary$primary_class == "ENSO-core (2-4y)" ~ 
      "Core-ENSO mode dominates - classic El Niño/La Niña cycles",
    TRUE ~ "High-frequency residual variability"
  )
  
  list(
    summary = dominant_summary,
    observed_modes = obs_modes,
    interpretation = interpretation
  )
}
