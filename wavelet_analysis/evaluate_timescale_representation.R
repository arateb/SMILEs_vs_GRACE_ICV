# ==============================================================================
# Function: evaluate_timescale_representation
# ==============================================================================
# Original name: analyze_timescale_representation
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Q2: Timescale representation
#
# ==============================================================================

evaluate_timescale_representation <- function(Wave_full) {
  obs_periods <- Wave_full %>% 
    filter(data == "GRACE(-FO)") %>% 
    pull(period)
  representation <- Wave_full %>%
    filter(data != "GRACE(-FO)") %>%
    group_by(data) %>%
    summarise(
      min_period = min(period),
      max_period = max(period),
      period_range = max_period - min_period,
      
      obs_periods_covered = sum(sapply(obs_periods, function(p) {
        p >= min_period & p <= max_period
      })),
      obs_coverage_fraction = obs_periods_covered / length(obs_periods),
      
      dominant_obs_period = obs_periods[1],
      dominant_period_percentile = mean(period <= dominant_obs_period) * 100,
      
      .groups = 'drop'
    )
  
  representation$coverage_quality <- case_when(
    representation$obs_coverage_fraction == 1 ~ "Excellent - all periods covered",
    representation$obs_coverage_fraction >= 0.67 ~ "Good - most periods covered", 
    representation$obs_coverage_fraction >= 0.33 ~ "Partial - some periods covered",
    TRUE ~ "Poor - few periods covered"
  )
  
  representation$dominant_period_assessment <- case_when(
    representation$dominant_period_percentile >= 10 & 
    representation$dominant_period_percentile <= 90 ~ "Well-represented",
    representation$dominant_period_percentile < 10 ~ "Underdispersive",
    TRUE ~ "Overdispersive"
  )
  
  return(representation)
}
