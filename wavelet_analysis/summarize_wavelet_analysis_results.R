# ==============================================================================
# Function: summarize_wavelet_analysis_results
# ==============================================================================
# Original name: summarize_wavelet_answers
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Summary function
#
# ==============================================================================

summarize_wavelet_analysis_results <- function(results) {
  cat("=== WAVELET ANALYSIS SUMMARY ===\n")
  cat(sprintf("Basin: %s (%s) - %.0f km²\n\n", 
              results$data$ID[1], results$data$name[1], results$data$area[1]))
  # Q1
  cat("Q1: What are the dominant long-term modes?\n")
  q1 <- results$Q1_dominant_modes
  cat(sprintf("   Primary: %.1f years (%s, power=%.3f)\n", 
              q1$summary$primary_period, q1$summary$primary_class, 
              q1$summary$primary_power))
  if (!is.na(q1$summary$secondary_period)) {
    cat(sprintf("   Secondary: %.1f years (%s)\n", 
                q1$summary$secondary_period, q1$summary$secondary_class))
  }
  cat(sprintf("   Interpretation: %s\n\n", q1$interpretation))
  
  # Q2
  cat("Q2: How well do ensembles represent observed timescales?\n")
  q2 <- results$Q2_timescale_representation
  for (i in 1:nrow(q2)) {
    cat(sprintf("   %s: %s (%.0f%% coverage, dominant period %s)\n",
                q2$data[i], q2$coverage_quality[i], 
                q2$obs_coverage_fraction[i]*100, 
                q2$dominant_period_assessment[i]))
  }
  cat("\n")
  
  # Q3
  cat("Q3: Are ensemble members consistent in spectral characteristics?\n")
  q3 <- results$Q3_ensemble_consistency
  for (i in 1:nrow(q3)) {
    if (q3$rank[i] == 1) {
      cat(sprintf("   %s: %s periods (CV=%.2f), %s in period class\n",
                  q3$data[i], q3$period_consistency_rating[i], 
                  q3$cv_period[i], q3$class_consistency_rating[i]))
    }
  }
  cat("\n")
  
  # Q4
  cat("Q4: Do models show systematic spectral biases?\n")
  q4 <- results$Q4_spectral_biases
  for (i in 1:nrow(q4)) {
    cat(sprintf("   %s: %s (ratio=%.2f), %s (ratio=%.2f)\n",
                q4$data[i], q4$period_bias_type[i], q4$period_bias_ratio[i],
                q4$power_bias_type[i], q4$power_bias_ratio[i]))
  }
  cat("\n")
  
  # Q5
  cat("Q5: Which model better represents long-term variability?\n")
  q5 <- results$Q5_model_comparison
  cat(sprintf("   Winner: %s (margin: %.1f points)\n", 
              q5$interpretation, q5$margin))
  cat("   Detailed scores:\n")
  for (i in 1:nrow(q5$scores)) {
    scores <- q5$scores[i, ]
    cat(sprintf("     %s: %.1f/100 (Coverage:%.0f + Dominant:%.0f + Consistency:%.0f + Bias:%.0f)\n", 
                scores$model, scores$total_score, scores$coverage_score,
                scores$dominant_score, scores$consistency_score, scores$bias_score))
  }
  cat("\n")
  
  return(invisible(results))
}
