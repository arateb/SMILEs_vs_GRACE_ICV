# ==============================================================================
# Function: interpret_extreme_analysis
# ==============================================================================
# Original name: interpret_extreme_results
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Interpretation function
#
# ==============================================================================

interpret_extreme_analysis <- function(analysis_results) {
  model_sum <- analysis_results$model_summary
  comp_sum <- analysis_results$comparison_summary
  
  cat("\n=== EXTREME REPRESENTATION ANALYSIS ===\n")
  
  # 1. Overall performance
  cat("\n1. OVERALL PERFORMANCE:\n")
  for(i in 1:nrow(model_sum)) {
    cat(sprintf("   %s (%d members):\n", model_sum$source[i], model_sum$n_members[i]))
    cat(sprintf("     - Minimum reached: %.1f%% (median: %.1f%%)\n", 
                model_sum$mean_pct_reach_min[i],
                model_sum$median_pct_reach_min[i]))
    cat(sprintf("     - Maximum reached: %.1f%% (median: %.1f%%)\n",
                model_sum$mean_pct_reach_max[i],
                model_sum$median_pct_reach_max[i]))
    cat(sprintf("     - Both well covered: %.1f%% of basins\n",
                model_sum$pct_both_well[i]))
  }
  
  # 2. Problem identification
  cat("\n2. PROBLEM BASINS:\n")
  for(i in 1:nrow(model_sum)) {
    cat(sprintf("   %s:\n", model_sum$source[i]))
    cat(sprintf("     - Minimum never reached: %d basins (%.1f%%)\n",
                model_sum$n_min_never_reached[i],
                model_sum$pct_min_never[i]))
    cat(sprintf("     - Maximum never reached: %d basins (%.1f%%)\n",
                model_sum$n_max_never_reached[i],
                model_sum$pct_max_never[i]))
  }
  
  # 3. Multi-model comparison (if available)
  if (!is.null(comp_sum) && nrow(comp_sum) > 0) {
    cat("\n3. MULTI-MODEL ENSEMBLE:\n")
    cat(sprintf("   Combined ensemble reach - Minimum: %.1f%%\n", comp_sum$mean_combined_reach_min))
    cat(sprintf("   Combined ensemble reach - Maximum: %.1f%%\n", comp_sum$mean_combined_reach_max))
    cat(sprintf("   Basins with good combined coverage: %d (%.1f%%)\n",
                comp_sum$n_good_combined,
                comp_sum$n_good_combined / comp_sum$n_basins * 100))
  }
  
  # 4. Physical insights
  cat("\n4. IMPLICATIONS:\n")
  mean_min_never <- mean(model_sum$pct_min_never)
  mean_max_never <- mean(model_sum$pct_max_never)
  
  if (mean_min_never > mean_max_never) {
    cat("   - Models struggle more with drought extremes (low minima)\n")
  } else if (mean_max_never > mean_min_never) {
    cat("   - Models struggle more with pluvial extremes (high maxima)\n")
  } else {
    cat("   - Models show similar performance for both extremes\n")
  }
  
  cat("   - Low reachability indicates insufficient ensemble spread or missing processes\n")
  cat("   - Combined ensembles improve coverage but may still miss critical extremes\n")
}
