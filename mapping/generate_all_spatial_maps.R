# ==============================================================================
# Function: generate_all_spatial_maps
# ==============================================================================
# Original name: generate_all_maps_clean
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#' Generate all maps with clean data (no NAs)
#
# ==============================================================================

generate_all_spatial_maps <- function(all_results, shp) {
  
  cat("\n========================================\n")
  cat("GENERATING CLEAN MAPS (NO MISSING DATA)\n")
  cat("========================================\n")
  
  # Test data extraction
  test_result <- extract_and_merge_clean(all_results, shp, "amplitude_summary")
  cat(sprintf("\nBasins with complete data: %d/%d\n", 
              test_result$n_basins, nrow(shp)))
  
  # Generate Figure 1
  fig1 <- create_figure1_clean(all_results, shp)
  
  # Save if successful
  if (!is.null(fig1)) {
    save_nature_figure(fig1, "figure1_variability.pdf", width_type = "double")
  }
  
  # Return test data for verification
  return(test_result$merged_shp)
}
