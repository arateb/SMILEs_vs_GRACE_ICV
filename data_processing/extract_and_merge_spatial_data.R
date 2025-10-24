# ==============================================================================
# Function: extract_and_merge_spatial_data
# ==============================================================================
# Original name: extract_and_merge_clean
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# PART 1: DATA EXTRACTION AND MERGING FUNCTIONS
# =============================================================================

#' Extract and merge clean data - MAIN FUNCTION FOR PROCESSING RESULTS
#' Removes basins with missing data to prevent NA issues in plots
#
# ==============================================================================

extract_and_merge_spatial_data <- function(all_results, shp, result_type) {
  
  cat(sprintf("\nExtracting %s...\n", result_type))
  
  # Extract specified result type from all basins
  extracted_data <- lapply(seq_along(all_results), function(i) {
    result <- all_results[[i]]
    
    # Skip NULL or error results
    if (is.null(result) || !is.null(result$error)) {
      return(NULL)
    }
    
    # Extract based on result type
    data <- switch(result_type,
      # Amplitude analysis
      "amplitude_metrics" = result$amplitude_metrics,
      "amplitude_summary" = result$amplitude_summary,
      "amplitude_interpretation" = result$amplitude_interpretation,
      
      # Extremes analysis
      "extremes_model" = result$extremes_model,
      "extremes_basin" = result$extremes_basin,
      "extremes_comparison" = result$extremes_comparison,
      "extremes_summary" = result$extremes_summary,
      
      # Wavelet analysis
      "wave_full" = result$wave_full,
      "wave_q1_summary" = result$wave_q1_summary,
      "wave_q1_modes" = result$wave_q1_modes,
      "wave_q1_interpretation" = result$wave_q1_interpretation,
      "wave_q2_timescale" = result$wave_q2_timescale,
      "wave_q3_consistency" = result$wave_q3_consistency,
      "wave_q4_grace_dominant" = result$wave_q4_grace_dominant,
      "wave_q4_model_results" = result$wave_q4_model_results,
      
      # Metadata
      "metadata" = result$metadata,
      
      # Default - try to access by name
      result[[result_type]]
    )
    
    # Return only valid data frames with rows
    if (!is.null(data) && is.data.frame(data) && nrow(data) > 0) {
      return(data)
    }
    return(NULL)
  })
  
  # Remove NULL results
  extracted_data <- extracted_data[!sapply(extracted_data, is.null)]
  
  # Handle case with no valid data
  if (length(extracted_data) == 0) {
    warning(paste("No valid", result_type, "data found"))
    return(list(
      merged_shp = shp[0,],  # Empty sf object
      combined_data = data.frame(),
      n_basins = 0,
      merge_key = NA
    ))
  }
  
  # Combine all valid data frames
  combined_data <- bind_rows(extracted_data)
  
  cat(sprintf("Combined data: %d rows\n", nrow(combined_data)))
  if ("bd_id" %in% names(combined_data)) {
    cat(sprintf("Unique basins in combined data: %d\n", 
                length(unique(combined_data$bd_id))))
  }
  
  # Merge with shapefile - INNER JOIN to remove NAs
  merge_key <- NULL
  merged_shp <- NULL
  
  if ("bd_id" %in% names(combined_data)) {
    merge_key <- "bd_id"
    merged_shp <- shp %>%
      inner_join(combined_data, by = c("bd_ID" = "bd_id"))
  } else if ("ID" %in% names(combined_data)) {
    merge_key <- "ID"
    merged_shp <- shp %>%
      inner_join(combined_data, by = c("OBJECTID" = "ID"))
  } else {
    stop("No valid merge key found in data!")
  }
  
  # Report results
  n_unique_basins <- length(unique(merged_shp$bd_ID))
  cat(sprintf("Merged shapefile: %d rows, %d unique basins\n", 
              nrow(merged_shp), n_unique_basins))
  cat(sprintf("Excluded %d basins with missing data\n", 
              nrow(shp) - n_unique_basins))
  
  # Return both merged shapefile and combined data
  return(list(
    merged_shp = merged_shp,
    combined_data = combined_data,
    n_basins = n_unique_basins,
    merge_key = merge_key
  ))
}
