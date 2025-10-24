# ==============================================================================
# Function: analyze_basin_tws
# ==============================================================================
# Original name: analyze_one
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# Purpose: Comprehensive TWS analysis comparing GRACE observations with 
#          CESM2 and IPSL climate model ensembles
# Analyses performed:
# 1. Amplitude Variance: How well do models capture TWS variability spread?
# 2. Extreme Coverage: Do model ensembles encompass GRACE extremes?
# 3. Wavelet Analysis: Spectral characteristics across timescales
#    - Q1: What are the dominant modes in GRACE?
#    - Q2: Do models represent GRACE timescales?
#    - Q3: Are ensemble members consistent?
#    - Q4: Can any member reproduce GRACE's dominant power?
#    - Q5: Which model performs better overall?
# =============================================================================
# =============================================================================
# COMPLETE BASIN ANALYSIS FUNCTION WITH DOCUMENTATION
# =============================================================================
# Purpose: Comprehensive TWS analysis comparing GRACE observations with 
#          CESM2 and IPSL climate model ensembles
# 
# Analyses performed:
# 1. Amplitude Variance: How well do models capture TWS variability spread?
# 2. Extreme Coverage: Do model ensembles encompass GRACE extremes?
# 3. Wavelet Analysis: Spectral characteristics across timescales
#    - Q1: What are the dominant modes in GRACE?
#    - Q2: Do models represent GRACE timescales?
#    - Q3: Are ensemble members consistent?
#    - Q4: Can any member reproduce GRACE's dominant power?
#    - Q5: Which model performs better overall?
# =============================================================================

#
# ==============================================================================

analyze_basin_tws <- function(Gattrs, obs_b, ensP, ensC, verbose = FALSE) {
    
  # Start timing
  start_time <- Sys.time()
  basin_name <- paste0(Gattrs$ID[1], "_", Gattrs$name[1])
  
  if (verbose) {
    cat("\n", rep("=", 60), "\n", sep = "")
    cat(sprintf("ANALYZING BASIN: %s (%.0f km²)\n", basin_name, Gattrs$area[1]))
    cat(rep("=", 60), "\n", sep = "")
    cat(sprintf("Data: %d months, %d CESM2 members, %d IPSL members\n",
                length(obs_b), ncol(ensC), ncol(ensP)))
    cat(sprintf("IPSL = %d Natural + %d GHG members\n\n",
                ncol(ensP)/2, ncol(ensP)/2))  # Since ensP = cbind(NatIPSL, GHIPSL)
  }
  
  # Initialize results list
  results <- list()
  
  # -----------------------------------------------------------------------------
  # 1. AMPLITUDE VARIANCE ANALYSIS
  # -----------------------------------------------------------------------------
  # Question: Do models capture the magnitude and spread of TWS variability?
  # -----------------------------------------------------------------------------
  
  if (verbose) cat("1. AMPLITUDE VARIANCE ANALYSIS\n")
  if (verbose) cat("   Question: Do models capture TWS variability spread?\n")
  
  tryCatch({
    # Basic spread metrics for both models
    tbl_cesm <- basic_spread_metrics(
      obs_b  = obs_b,
      ensC   = ensC,
      Gattrs = Gattrs,
      src    = "CESM2"
    )
    
    tbl_ipsl <- basic_spread_metrics(
      obs_b  = obs_b,
      ensC   = ensP,  # Fixed: ensP not ensC
      Gattrs = Gattrs,
      src    = "IPSL"
    )
    
    # Combine results
    M_basics_metrics <- rbind(tbl_cesm$metrics, tbl_ipsl$metrics)
    M_basics_summary <- rbind(tbl_cesm$summary, tbl_ipsl$summary)
    InterPret <- combine_model_interpretations(tbl_cesm, tbl_ipsl, Gattrs)
    
    # Store amplitude results
    results$amplitude_metrics <- as.data.frame(M_basics_metrics)
    results$amplitude_summary <- as.data.frame(M_basics_summary)
    results$amplitude_interpretation <- as.data.frame(InterPret)
    
    if (verbose) cat("   ✓ Complete\n\n")
    
  }, error = function(e) {
    warning(sprintf("Amplitude analysis failed for %s: %s", basin_name, e$message))
    results$amplitude_metrics <- data.frame()
    results$amplitude_summary <- data.frame()
    results$amplitude_interpretation <- data.frame()
    if (verbose) cat("   ✗ Failed\n\n")
  })
  
  # -----------------------------------------------------------------------------
  # 2. EXTREME COVERAGE ANALYSIS
  # -----------------------------------------------------------------------------
  # Question: Do model ensembles encompass GRACE-observed extremes?
  # -----------------------------------------------------------------------------
  
  if (verbose) cat("2. EXTREME COVERAGE ANALYSIS\n")
  if (verbose) cat("   Question: Do ensembles encompass GRACE extremes?\n")
  
  tryCatch({
    # Process both models
    cesm_ext <- extreme_coverage_analysis(obs_b, ensC, Gattrs, "CESM2")
    ipsl_ext <- extreme_coverage_analysis(obs_b, ensP, Gattrs, "IPSL")
    
    # Combine and process
    Xresults <- rbind(cesm_ext, ipsl_ext)
    postProcssX <- process_extreme_analysis(Xresults)
    InterpreResults <- interpret_extreme_results(postProcssX)
    
    # Store extreme results with attributes
    results$extremes_model <- as.data.frame(attach_attrs(postProcssX$model_summary, Gattrs))
    results$extremes_basin <- as.data.frame(postProcssX$basin_results)
    results$extremes_comparison <- as.data.frame(postProcssX$basin_comparison)
    results$extremes_summary <- as.data.frame(attach_attrs(postProcssX$comparison_summary, Gattrs))
    results$extremes_interpretation <- InterpreResults
    
    if (verbose) cat("   ✓ Complete\n\n")
    
  }, error = function(e) {
    warning(sprintf("Extreme analysis failed for %s: %s", basin_name, e$message))
    results$extremes_model <- data.frame()
    results$extremes_basin <- data.frame()
    results$extremes_comparison <- data.frame()
    results$extremes_summary <- data.frame()
    results$extremes_interpretation <- data.frame()
    if (verbose) cat("   ✗ Failed\n\n")
  })
  
  # -----------------------------------------------------------------------------
  # 3. WAVELET ANALYSIS - 5 QUESTIONS
  # -----------------------------------------------------------------------------
  # Q1: What are the dominant long-term modes in GRACE?
  # Q2: How well do ensembles represent observed timescales?
  # Q3: Are ensemble members consistent in spectral characteristics?
  # Q4: Can any model member reproduce GRACE's dominant mode power?
  # Q5: Which model better represents long-term variability?
  # -----------------------------------------------------------------------------
  
  if (verbose) {
    cat("3. WAVELET SPECTRAL ANALYSIS\n")
    cat("   Analyzing long-term variability modes (STL-filtered data)\n")
  }
  
  tryCatch({
    # Run main wavelet analysis
    WaveBasins <- ensemble_wavelet_analysis(
      obs_ts = obs_b,  # obs_b is the detrended GRACE time series
      ensC = ensC,     # CESM2 detrended ensemble
      ensP = ensP,     # IPSL combined (Natural + GHG) detrended ensemble
      Gattrs = Gattrs,
      top_k = 4  # Top 4 modes per time series
    )
    
    # Store full wavelet data
    results$wave_full <- as.data.frame(WaveBasins$data)
    
    # Q1: Dominant modes in GRACE
    if (!is.null(WaveBasins$Q1_dominant_modes) && !is.null(WaveBasins$Q1_dominant_modes$summary)) {
      results$wave_q1_summary <- as.data.frame(attach_attrs(WaveBasins$Q1_dominant_modes$summary, Gattrs))
      results$wave_q1_modes <- as.data.frame(attach_attrs(WaveBasins$Q1_dominant_modes$observed_modes, Gattrs))
      results$wave_q1_interpretation <- data.frame(
        interpretation = WaveBasins$Q1_dominant_modes$interpretation,
        stringsAsFactors = FALSE
      )
      results$wave_q1_interpretation <- as.data.frame(attach_attrs(results$wave_q1_interpretation, Gattrs))
      
      if (verbose) {
        cat(sprintf("   Q1: GRACE dominant mode = %.1f years (%s)\n", 
                    WaveBasins$Q1_dominant_modes$summary$primary_period,
                    WaveBasins$Q1_dominant_modes$summary$primary_class))
      }
    }
    
    # Q2: Timescale representation
    if (!is.null(WaveBasins$Q2_timescale_representation) && nrow(WaveBasins$Q2_timescale_representation) > 0) {
      results$wave_q2_timescale <- as.data.frame(attach_attrs(WaveBasins$Q2_timescale_representation, Gattrs))
      if (verbose) cat("   Q2: Timescale representation assessed\n")
    }
    
    # Q3: Ensemble consistency
    if (!is.null(WaveBasins$Q3_ensemble_consistency) && nrow(WaveBasins$Q3_ensemble_consistency) > 0) {
      results$wave_q3_consistency <- as.data.frame(attach_attrs(WaveBasins$Q3_ensemble_consistency, Gattrs))
      if (verbose) cat("   Q3: Ensemble consistency evaluated\n")
    }
    
    # Q4: Dominant mode reproduction (NEW ANALYSIS)
    if (verbose) cat("   Q4: Checking dominant mode power reproduction...\n")
    
    q4_results <- analyze_dominant_mode_reproduction(WaveBasins$data)
    
    if (!is.null(q4_results$grace_dominant)) {
      results$wave_q4_grace_dominant <- as.data.frame(attach_attrs(q4_results$grace_dominant, Gattrs))
      
      if (!is.null(q4_results$model_results)) {
        results$wave_q4_model_results <- as.data.frame(attach_attrs(q4_results$model_results, Gattrs))
        
        if (verbose) {
          # Quick summary of Q4 results
          cesm_q4 <- q4_results$model_results[q4_results$model_results$model == "CESM2", ]
          ipsl_q4 <- q4_results$model_results[q4_results$model_results$model == "IPSL", ]
          
          if (nrow(cesm_q4) > 0 && cesm_q4$n_members_with_class > 0) {
            cat(sprintf("      CESM2: %.0f%% members within ±25%% of GRACE power\n", 
                        cesm_q4$frac_within_25pct * 100))
          }
          if (nrow(ipsl_q4) > 0 && ipsl_q4$n_members_with_class > 0) {
            cat(sprintf("      IPSL:  %.0f%% members within ±25%% of GRACE power\n", 
                        ipsl_q4$frac_within_25pct * 100))
          }
        }
      }
    }
    
    # Q5: Model comparison
    if (!is.null(WaveBasins$Q5_model_comparison) && !is.null(WaveBasins$Q5_model_comparison$scores)) {
      results$wave_q5_scores <- as.data.frame(attach_attrs(WaveBasins$Q5_model_comparison$scores, Gattrs))
      results$wave_q5_winner <- data.frame(
        winner = WaveBasins$Q5_model_comparison$winner,
        margin = WaveBasins$Q5_model_comparison$margin,
        interpretation = WaveBasins$Q5_model_comparison$interpretation,
        stringsAsFactors = FALSE
      )
      results$wave_q5_winner <- as.data.frame(attach_attrs(results$wave_q5_winner, Gattrs))
      
      if (verbose) {
        cat(sprintf("   Q5: %s\n", WaveBasins$Q5_model_comparison$interpretation))
      }
    }
    
    if (verbose) cat("   ✓ Wavelet analysis complete\n\n")
    
  }, error = function(e) {
    warning(sprintf("Wavelet analysis failed for %s: %s", basin_name, e$message))
    results$wave_full <- data.frame()
    if (verbose) cat("   ✗ Wavelet analysis failed\n\n")
  })
  
  # -----------------------------------------------------------------------------
  # 4. METADATA AND TIMING
  # -----------------------------------------------------------------------------
  
  end_time <- Sys.time()
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  results$metadata <- data.frame(
    basin_id = Gattrs$ID[1],
    basin_name = Gattrs$name[1],
    area_km2 = Gattrs$area[1],
    lon = ifelse("C_lon" %in% names(Gattrs), Gattrs$C_lon[1], NA),
    lat = ifelse("C_lat" %in% names(Gattrs), Gattrs$C_lat[1], NA),
    analysis_time_sec = time_taken,
    analysis_timestamp = Sys.time(),
    n_obs = length(obs_b),
    n_cesm_members = ncol(ensC),
    n_ipsl_members = ncol(ensP),
    stringsAsFactors = FALSE
  )
  
  if (verbose) {
    cat(sprintf("ANALYSIS COMPLETED in %.1f seconds\n", time_taken))
    cat(rep("=", 60), "\n\n", sep = "")
  }
  
  # Return all results
  return(results)
}
