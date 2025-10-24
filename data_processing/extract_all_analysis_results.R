# ==============================================================================
# Function: extract_all_analysis_results
# ==============================================================================
# Original name: extract_all_results
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#' Extract all result types at once
#
# ==============================================================================

extract_all_analysis_results <- function(all_results, shp) {
  
  result_types <- c(
    "amplitude_metrics", "amplitude_summary", "amplitude_interpretation",
    "extremes_model", "extremes_basin", "extremes_comparison", "extremes_summary",
    "wave_full", "wave_q1_summary", "wave_q1_modes", "wave_q1_interpretation",
    "wave_q2_timescale", "wave_q3_consistency", "wave_q4_grace_dominant", 
    "wave_q4_model_results", "metadata"
  )
  
  all_extractions <- list()
  
  for (type in result_types) {
    cat(sprintf("\n--- Processing %s ---\n", type))
    
    tryCatch({
      result <- extract_and_merge_clean(all_results, shp, type)
      all_extractions[[type]] <- result
    }, error = function(e) {
      cat(sprintf("Error with %s: %s\n", type, e$message))
      all_extractions[[type]] <- NULL
    })
  }
  
  return(all_extractions)
}
