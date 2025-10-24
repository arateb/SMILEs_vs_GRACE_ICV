# ==============================================================================
# Function: assess_ensemble_spectral_consistency
# ==============================================================================
# Original name: analyze_ensemble_consistency
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Q3: Ensemble consistency
#
# ==============================================================================

assess_ensemble_spectral_consistency <- function(Wave_full) {
  consistency <- Wave_full %>%
    filter(data != "GRACE(-FO)") %>%
    group_by(data, rank) %>%
    summarise(
      n_members = n(),
      mean_period = mean(period),
      min_period = min(period),
      max_period = max(period),
      sd_period = sd(period),
      cv_period = sd_period / mean_period,
      mean_power = mean(power),
      sd_power = sd(power),
      cv_power = sd_power / mean_power,
      mode_class = names(sort(table(period_class), decreasing = TRUE))[1],
      class_agreement = max(table(period_class)) / n(),
      .groups = 'drop'
    )
  
  consistency$period_consistency_rating <- case_when(
    consistency$cv_period < 0.2 ~ "High consistency",
    consistency$cv_period < 0.4 ~ "Moderate consistency",
    TRUE ~ "Low consistency"
  )
  
  consistency$class_consistency_rating <- case_when(
    consistency$class_agreement >= 0.8 ~ "High agreement",
    consistency$class_agreement >= 0.6 ~ "Moderate agreement", 
    TRUE ~ "Low agreement"
  )
  
  return(consistency)
}
