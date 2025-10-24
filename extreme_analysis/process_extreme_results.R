# ==============================================================================
# Function: process_extreme_results
# ==============================================================================
# Original name: process_extreme_analysis
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#
# ==============================================================================

process_extreme_results <- function(results) {
  library(dplyr)
  library(tidyr)
  
  # Check if results is empty
  if (nrow(results) == 0) {
    stop("No results to process")
  }
  
  # 1. Model-level summary ---------------------------------------------------
  model_summary <- results %>%
    group_by(source) %>%
    summarise(
      n_basins                     = n(),
      n_members                    = first(n_members),
      
      # Reachability statistics
      mean_pct_reach_min           = mean(frac_reach_min) * 100,
      mean_pct_reach_max           = mean(frac_reach_max) * 100,
      median_pct_reach_min         = median(frac_reach_min) * 100,
      median_pct_reach_max         = median(frac_reach_max) * 100,
      
      # Problem basins
      n_min_never_reached          = sum(n_reach_min == 0),
      n_max_never_reached          = sum(n_reach_max == 0),
      n_both_never_reached         = sum(n_reach_min == 0 & n_reach_max == 0),
      n_min_rarely_reached         = sum(frac_reach_min < 0.05),
      n_max_rarely_reached         = sum(frac_reach_max < 0.05),
      n_both_well_represented      = sum(frac_reach_min >= 0.50 & frac_reach_max >= 0.50),
      
      # Percentage of basins in each category
      pct_min_never                = mean(n_reach_min == 0) * 100,
      pct_max_never                = mean(n_reach_max == 0) * 100,
      pct_min_rarely               = mean(frac_reach_min < 0.05) * 100,
      pct_max_rarely               = mean(frac_reach_max < 0.05) * 100,
      pct_both_well                = mean(frac_reach_min >= 0.50 & frac_reach_max >= 0.50) * 100,
      
      .groups = "drop"
    )
  
  # 2. Basin-specific classifications ----------------------------------------
  basin_results <- results %>%
    mutate(
      min_classification = case_when(
        frac_reach_min == 0    ~ "Never reached",
        frac_reach_min < 0.05  ~ "Rarely reached",
        frac_reach_min < 0.50  ~ "Moderately reached",
        TRUE                   ~ "Well represented"
      ),
      max_classification = case_when(
        frac_reach_max == 0    ~ "Never reached",
        frac_reach_max < 0.05  ~ "Rarely reached",
        frac_reach_max < 0.50  ~ "Moderately reached",
        TRUE                   ~ "Well represented"
      ),
      overall_assessment = case_when(
        frac_reach_min >= 0.50 & frac_reach_max >= 0.50 ~ "Good - both well represented",
        frac_reach_min >= 0.05 & frac_reach_max >= 0.05 ~ "Moderate - both sometimes reached",
        frac_reach_min > 0 & frac_reach_max > 0         ~ "Poor - rarely reached",
        TRUE                                             ~ "Inadequate - at least one never reached"
      )
    )
  
  # 3. Check if we have multiple models for comparison ----------------------
  models <- unique(results$source)
  n_models <- length(models)
  
  if (n_models < 2) {
    return(list(
      model_summary      = model_summary,
      basin_results      = basin_results,
      basin_comparison   = NULL,
      comparison_summary = NULL
    ))
  }
  
  # 4. Basin comparison between models ---------------------------------------
  basin_comparison <- results %>%
    dplyr::select(ID, name, bd_id, area, C_lon, C_lat, IrrigatPct,
           source, n_members,
           frac_reach_min, frac_reach_max,
           extreme_status) %>%
    pivot_wider(
      names_from   = source,
      values_from  = c(n_members, frac_reach_min, frac_reach_max, extreme_status),
      names_sep    = "_"
    )
  
  # 5. Enhanced comparison with combined metrics -----------------------------
  # Get column names dynamically
  member_cols <- paste0("n_members_", models)
  reach_min_cols <- paste0("frac_reach_min_", models)
  reach_max_cols <- paste0("frac_reach_max_", models)
  
  # Check if all expected columns exist
  if (all(member_cols %in% names(basin_comparison))) {
    
    # Calculate total members
    basin_comparison$total_members <- rowSums(
      basin_comparison[member_cols], 
      na.rm = TRUE
    )
    
    # Calculate weighted average reach
    weighted_min <- 0
    weighted_max <- 0
    
    for (model in models) {
      weight_col <- paste0("n_members_", model)
      min_col <- paste0("frac_reach_min_", model)
      max_col <- paste0("frac_reach_max_", model)
      
      weighted_min <- weighted_min + 
        basin_comparison[[weight_col]] * basin_comparison[[min_col]]
      weighted_max <- weighted_max + 
        basin_comparison[[weight_col]] * basin_comparison[[max_col]]
    }
    
    basin_comparison$combined_reach_min <- weighted_min / basin_comparison$total_members
    basin_comparison$combined_reach_max <- weighted_max / basin_comparison$total_members
    
    # Find best model for each basin
    basin_comparison$best_model_min <- apply(
      basin_comparison[reach_min_cols], 1,
      function(x) {
        if (all(is.na(x))) return(NA)
        models[which.max(x)]
      }
    )
    
    basin_comparison$best_model_max <- apply(
      basin_comparison[reach_max_cols], 1,
      function(x) {
        if (all(is.na(x))) return(NA)
        models[which.max(x)]
      }
    )
    
    # Overall representation
    basin_comparison$combined_assessment <- case_when(
      basin_comparison$combined_reach_min >= 0.50 & 
        basin_comparison$combined_reach_max >= 0.50 ~ "Good - combined ensemble",
      basin_comparison$combined_reach_min >= 0.05 & 
        basin_comparison$combined_reach_max >= 0.05 ~ "Moderate - combined ensemble",
      basin_comparison$combined_reach_min > 0 & 
        basin_comparison$combined_reach_max > 0     ~ "Poor - combined ensemble",
      TRUE                                           ~ "Inadequate - even combined"
    )
  }
  
  # 6. Summary comparison ----------------------------------------------------
  comparison_summary <- data.frame(
    n_basins                 = nrow(basin_comparison),
    n_models                 = n_models,
    mean_combined_reach_min  = mean(basin_comparison$combined_reach_min, na.rm = TRUE) * 100,
    mean_combined_reach_max  = mean(basin_comparison$combined_reach_max, na.rm = TRUE) * 100,
    n_good_combined          = sum(basin_comparison$combined_assessment == "Good - combined ensemble", na.rm = TRUE),
    n_moderate_combined      = sum(basin_comparison$combined_assessment == "Moderate - combined ensemble", na.rm = TRUE),
    n_poor_combined          = sum(basin_comparison$combined_assessment == "Poor - combined ensemble", na.rm = TRUE),
    n_inadequate_combined    = sum(basin_comparison$combined_assessment == "Inadequate - even combined", na.rm = TRUE)
  )
  
  # Add model-specific comparisons
  for (model in models) {
    comparison_summary[[paste0(model, "_best_min")]] <- sum(basin_comparison$best_model_min == model, na.rm = TRUE)
    comparison_summary[[paste0(model, "_best_max")]] <- sum(basin_comparison$best_model_max == model, na.rm = TRUE)
  }
  
  # Return all tables
  list(
    model_summary      = model_summary,
    basin_results      = basin_results,
    basin_comparison   = basin_comparison,
    comparison_summary = comparison_summary
  )
}
