# ==============================================================================
# Function: extract_result_by_type
# ==============================================================================
# Original name: extract_analysis_type
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# -----------------------------------------------------------------------------
# HELPER: Extract specific analysis results across all basins
# -----------------------------------------------------------------------------
#
# ==============================================================================

extract_result_by_type <- function(all_results, analysis_type) {
  # analysis_type can be: "amplitude", "extremes", "wave_q1", "wave_q4", etc.
  
  extracted <- lapply(all_results$results, function(basin) {
    # Find all result names that contain the analysis_type
    matching_names <- grep(analysis_type, names(basin), value = TRUE)
    
    if (length(matching_names) > 0) {
      # Return all matching results for this basin
      basin[matching_names]
    } else {
      NULL
    }
  })
  
  # Remove NULL entries
  extracted <- extracted[!sapply(extracted, is.null)]
  
  return(extracted)
}
