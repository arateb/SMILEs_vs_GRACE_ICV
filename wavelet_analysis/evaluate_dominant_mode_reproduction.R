# ==============================================================================
# Function: evaluate_dominant_mode_reproduction
# ==============================================================================
# Original name: analyze_dominant_mode_reproduction
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Q4: How well do model ensembles reproduce GRACE's dominant mode?
#
# ==============================================================================

evaluate_dominant_mode_reproduction <- function(Wave_full) {
  obs_data <- Wave_full %>% 
    filter(data == "GRACE(-FO)") %>%
    arrange(desc(power))
  
  if (nrow(obs_data) == 0) {
    return(list(grace_dominant = NULL, model_results = NULL))
  }
  
  # Get GRACE's dominant mode (highest power)
  grace_dominant <- obs_data[1, ]
  
  results <- list()
  
  for (model_name in c("CESM2", "IPSL")) {
    model_data <- Wave_full %>% filter(data == model_name)
    
    # Find all model modes in same period class as GRACE's dominant
    matching_modes <- model_data %>%
      filter(period_class == grace_dominant$period_class)
    
    if (nrow(matching_modes) > 0) {
      # CRITICAL: For each member, take only their STRONGEST mode in this period class
      # This ensures we compare one mode per member
      member_best_modes <- matching_modes %>%
        group_by(member) %>%
        slice_max(power, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(power_ratio = power / grace_dominant$power)
      
      # Now calculate statistics on the per-member best modes
      model_summary <- data.frame(
        model = model_name,
        n_members_total = n_distinct(model_data$member),
        n_members_with_class = nrow(member_best_modes),  # One row per member now
        
        # Distribution of power ratios across members
        min_ratio = min(member_best_modes$power_ratio),
        q10_ratio = quantile(member_best_modes$power_ratio, 0.10),
        q25_ratio = quantile(member_best_modes$power_ratio, 0.25),
        median_ratio = median(member_best_modes$power_ratio),
        q75_ratio = quantile(member_best_modes$power_ratio, 0.75),
        q90_ratio = quantile(member_best_modes$power_ratio, 0.90),
        max_ratio = max(member_best_modes$power_ratio),
        
        # What fraction of MEMBERS come close?
        frac_within_10pct = mean(abs(member_best_modes$power_ratio - 1) <= 0.1),
        frac_within_25pct = mean(abs(member_best_modes$power_ratio - 1) <= 0.25),
        frac_within_50pct = mean(abs(member_best_modes$power_ratio - 1) <= 0.5),
        
        # Does ensemble encompass GRACE?
        encompasses_grace = (min(member_best_modes$power_ratio) <= 1) & 
                           (max(member_best_modes$power_ratio) >= 1),
        
        # Which side of GRACE is the ensemble median?
        median_bias = ifelse(
          median(member_best_modes$power_ratio) < 1,
          "weaker than GRACE",
          ifelse(median(member_best_modes$power_ratio) > 1,
                 "stronger than GRACE",
                 "matches GRACE")
        )
      )
      
      results[[model_name]] <- model_summary
      
    } else {
      # No members have this period class
      results[[model_name]] <- data.frame(
        model = model_name,
        n_members_total = n_distinct(model_data$member),
        n_members_with_class = 0,
        min_ratio = NA, q10_ratio = NA, q25_ratio = NA,
        median_ratio = NA, q75_ratio = NA, q90_ratio = NA, max_ratio = NA,
        frac_within_10pct = 0, frac_within_25pct = 0, frac_within_50pct = 0,
        encompasses_grace = FALSE,
        median_bias = "no matching periods"
      )
    }
  }
  
  return(list(
    grace_dominant = grace_dominant,
    model_results = do.call(rbind, results)
  ))
}
