# =============================================================================
# COMPLETE BASIN ANALYSIS FUNCTION WITH DOCUMENTATION
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

analyze_one <- function(Gattrs, obs_b, ensP, ensC, verbose = FALSE) {
    
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

# -----------------------------------------------------------------------------
# HELPER: Process multiple basins with progress tracking
# -----------------------------------------------------------------------------
analyze_multiple_basins <- function(basin_data_list, verbose = TRUE, 
                                   save_progress = FALSE, save_dir = ".") {
  
  n_basins <- length(basin_data_list)
  all_results <- list()
  failed_basins <- c()
  
  total_start <- Sys.time()
  
  cat(sprintf("\nStarting analysis of %d basins\n", n_basins))
  cat(sprintf("Time: %s\n\n", Sys.time()))
  
  for (i in 1:n_basins) {
    if (verbose && i %% 10 == 1) {
      cat(sprintf("\n--- Basin batch %d-%d of %d ---\n", 
                  i, min(i+9, n_basins), n_basins))
    }
    
    tryCatch({
      # Extract data for this basin
      basin_data <- basin_data_list[[i]]
      
      # Analyze one basin
      basin_results <- analyze_one(
        Gattrs = basin_data$Gattrs,
        obs_b = basin_data$obs_b,
        ensP = basin_data$ensP,
        ensC = basin_data$ensC,
        verbose = (i <= 3)  # Only verbose for first 3 basins
      )
      
      all_results[[i]] <- basin_results
      
    }, error = function(e) {
      warning(sprintf("Basin %d failed: %s", i, e$message))
      failed_basins <- c(failed_basins, i)
      all_results[[i]] <- list(error = e$message)
    })
    
    # Save progress periodically
    if (save_progress && i %% 25 == 0) {
      saveRDS(all_results, file.path(save_dir, sprintf("basin_results_progress_%d.rds", i)))
      cat(sprintf("   Progress saved at basin %d\n", i))
    }
    
    # Time estimate every 10 basins
    if (verbose && i %% 10 == 0 && i < n_basins) {
      elapsed <- as.numeric(difftime(Sys.time(), total_start, units = "mins"))
      rate <- elapsed / i
      remaining <- rate * (n_basins - i)
      cat(sprintf("   Elapsed: %.1f min | Remaining: %.1f min | ETA: %s\n", 
                  elapsed, remaining, 
                  format(Sys.time() + remaining * 60, "%H:%M")))
    }
  }
  
  # Final summary
  total_time <- difftime(Sys.time(), total_start, units = "mins")
  
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("ANALYSIS COMPLETE\n")
  cat(sprintf("Total time: %.1f minutes\n", as.numeric(total_time)))
  cat(sprintf("Average per basin: %.1f seconds\n", as.numeric(total_time) * 60 / n_basins))
  cat(sprintf("Successful: %d basins\n", n_basins - length(failed_basins)))
  if (length(failed_basins) > 0) {
    cat(sprintf("Failed: %d basins (indices: %s)\n", 
                length(failed_basins), paste(failed_basins, collapse = ", ")))
  }
  cat(rep("=", 60), "\n\n", sep = "")
  
  return(list(
    results = all_results,
    failed = failed_basins,
    timing = list(
      total_minutes = as.numeric(total_time),
      per_basin_seconds = as.numeric(total_time) * 60 / n_basins
    )
  ))
}

# -----------------------------------------------------------------------------
# HELPER: Extract specific analysis results across all basins
# -----------------------------------------------------------------------------
extract_analysis_type <- function(all_results, analysis_type) {
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

# -----------------------------------------------------------------------------
# Example usage with your data structure:
# -----------------------------------------------------------------------------
# # Your main analysis loop - CLEANED VERSION
# set.seed(1985)
# n_basins <- nrow(attrs)  # 184 basins
# FinalResults <- vector("list", n_basins)
# names(FinalResults) <- attrs$name
# 
# # Track timing
# overall_start <- Sys.time()
# 
# for (ii in 1:n_basins) {
#   # Progress indicator
#   if (ii %% 10 == 1) {
#     cat(sprintf("\n--- Processing basins %d-%d of %d ---\n", 
#                 ii, min(ii+9, n_basins), n_basins))
#   }
#   
#   # Extract basin data
#   Gattrs <- attrs[ii, ]
#   
#   # Get ensemble data (transpose to get time x members)
#   CESM_ensemble <- t(G_CESM[ii, , ])      # Now: time x 90 members
#   IPSL_gh <- t(G_IPSL_GH[ii, , ])        # Greenhouse gas forcing
#   IPSL_Nat <- t(G_ISL_NAT[ii, , ])       # Natural forcing
#   
#   # GRACE observations
#   gfo <- data.frame(
#     tws = gfo_dtrend$median[, ii], 
#     sigma = gfo_dtrend$mad[, ii]
#   )
#   
#   # Detrend all ensembles
#   G_cesm <- detrend_ensemble_full(CESM_ensemble)
#   G_ghg_ipsl <- detrend_ensemble_full(IPSL_gh)
#   G_nat_ipsl <- detrend_ensemble_full(IPSL_Nat)
#   
#   # Extract detrended data
#   obs_b <- gfo$tws                        # GRACE detrended
#   ensC <- G_cesm$detrended                # CESM2 detrended
#   ensP <- cbind(G_nat_ipsl$detrended,    # IPSL combined (Natural + GHG)
#                 G_ghg_ipsl$detrended)
#   
#   # Run analysis
#   FinalResults[[ii]] <- analyze_one(
#     Gattrs = Gattrs,
#     obs_b = obs_b,
#     ensP = ensP,    # IPSL combined
#     ensC = ensC,    # CESM2
#     verbose = (ii <= 3)  # Only verbose for first 3 basins
#   )
#   
#   # Clean up memory
#   rm(CESM_ensemble, IPSL_gh, IPSL_Nat, gfo,
#      G_cesm, G_ghg_ipsl, G_nat_ipsl,
#      obs_b, ensC, ensP)
#   
#   # Progress
#   if (ii %% 10 == 0) {
#     elapsed <- difftime(Sys.time(), overall_start, units = "mins")
#     rate <- as.numeric(elapsed) / ii
#     remaining <- rate * (n_basins - ii)
#     cat(sprintf("  Progress: %d/%d (%.1f%%) | Elapsed: %.1f min | ETA: %.1f min\n",
#                 ii, n_basins, ii/n_basins*100, 
#                 as.numeric(elapsed), remaining))
#   }
# }
# 
# # Save results
# total_time <- difftime(Sys.time(), overall_start, units = "mins")
# cat(sprintf("\nAnalysis complete! Total time: %.1f minutes\n", as.numeric(total_time)))
# 
# saveRDS(FinalResults, "GGFo_vs_MMILEs_Comparison_Jun_29th_2025.rds")


# -----------------------------------------------------------------------------
# Example usage:
# -----------------------------------------------------------------------------
# # Single basin
# results <- analyze_one(Gattrs, obs_b, ensP, ensC, verbose = TRUE)
# 
# # Multiple basins (assuming you have a list of basin data)
# basin_data_list <- list(
#   list(Gattrs = Gattrs1, obs_b = obs1, ensP = ensP1, ensC = ensC1),
#   list(Gattrs = Gattrs2, obs_b = obs2, ensP = ensP2, ensC = ensC2),
#   # ... more basins
# )
# 
# all_results <- analyze_multiple_basins(basin_data_list, verbose = TRUE)
# 
# # Extract specific results
# q4_results <- extract_analysis_type(all_results, "wave_q4")
# extreme_results <- extract_analysis_type(all_results, "extremes")
# =======================================================================
#  basic_spread_metrics()
#  ---------------------------------------------------------------------
#  Compute σ, σ-ratio, amplitude, amplitude-ratio for one basin.
#
#  obs_b   : numeric vector (GRACE anomalies, length N)
#  ensC    : numeric matrix  N × M  (ensemble anomalies)
#  Gattrs  : one-row data-frame with basin attributes
#              ▸ columns at least:  ID, name, bd_id, area, C_lon, C_lat …
#  src     : character string   (“CESM2”, “IPSL”, “Combined”, …)
#
#  Requires helper functions:
#        ▸ summarise_vec2()   – 11-number summary
#        ▸ row_metric()       – wraps obs value vs. ensemble vector
#
#  Returns a 4-row data.frame ready to rbind with other metrics.
# =======================================================================
basic_spread_metrics <- function(obs_b, ensC, Gattrs, src) {
  
  ## ---- Input validation -------------------------------------------------------
  if (!is.data.frame(Gattrs) || nrow(Gattrs) != 1)
    stop("Gattrs must be a single-row data.frame")
  
  if (length(obs_b) < 10)
    warning("Observed time series has < 10 points, results may be unreliable")
  
  if (!is.matrix(ensC) && !is.data.frame(ensC))
    stop("ensC must be a matrix or data.frame")
  
  basin_id   <- Gattrs$ID
  basin_name <- Gattrs$name
  
  ## ---- Drop empty ensemble members --------------------------------------------
  ensC <- as.matrix(ensC)  # Ensure matrix
  #valid_members <- colSums(!is.na(ensC)) > 10  # At least 10 valid points
  #ensC <- ensC[, valid_members, drop = FALSE]
  n_members <- ncol(ensC)
  rows <- list()
  
  ## ---- 1. Standard deviation (temporal variability) --------------------------
  # One value from observations
  sd_obs <- sd(obs_b, na.rm = TRUE)
  
  # One value per ensemble member
  sd_vec <- apply(ensC, 2, sd, na.rm = TRUE)
  sd_ens_mean <- mean(sd_vec, na.rm = TRUE)
  
  if (sd_ens_mean > 0 && !is.na(sd_obs)) {
    rows[[1]] <- row_metric(basin_id, src, "sd", sd_obs, sd_vec)
    rows[[2]] <- row_metric(basin_id, src, "sd_ratio",
                            sd_obs / sd_ens_mean,
                            sd_vec / sd_ens_mean)}
  
  ## ---- 2. Amplitude (max - min over full time period) ------------------------
  # One value from observations
  amp_obs <- diff(range(obs_b, na.rm = TRUE))
  
  # One value per ensemble member
  amp_vec <- apply(ensC, 2, function(x) {
    if (sum(!is.na(x)) < 2) return(NA)
    diff(range(x, na.rm = TRUE))
  })
  amp_ens_mean <- mean(amp_vec, na.rm = TRUE)
  
  if (amp_ens_mean > 0 && !is.na(amp_obs)) {
    rows[[length(rows) + 1]] <- row_metric(basin_id, src, "amp", amp_obs, amp_vec)
    rows[[length(rows) + 1]] <- row_metric(basin_id, src, "amp_ratio",
                                           amp_obs / amp_ens_mean,
                                           amp_vec / amp_ens_mean)
  }
  
  ## ---- 3. Interquartile range (robust measure of spread) ---------------------
  # One value from observations
  iqr_obs <- IQR(obs_b, na.rm = TRUE)
  
  # One value per ensemble member
  iqr_vec <- apply(ensC, 2, IQR, na.rm = TRUE)
  iqr_ens_mean <- mean(iqr_vec, na.rm = TRUE)
  
  if (iqr_ens_mean > 0 && !is.na(iqr_obs)) {
    rows[[length(rows) + 1]] <- row_metric(basin_id, src, "iqr", iqr_obs, iqr_vec)
    rows[[length(rows) + 1]] <- row_metric(basin_id, src, "iqr_ratio",
                                           iqr_obs / iqr_ens_mean,
                                           iqr_vec / iqr_ens_mean)}
  
  ## ---- 4. MAD (Median Absolute Deviation - very robust) ----------------------
  # One value from observations
  mad_obs <- mad(obs_b, constant = 1.4826, na.rm = TRUE)
  # One value per ensemble member
  mad_vec <- apply(ensC, 2, mad, constant = 1.4826, na.rm = TRUE)
  mad_ens_mean <- mean(mad_vec, na.rm = TRUE)
  
  if (mad_ens_mean > 0 && !is.na(mad_obs)) {
    rows[[length(rows) + 1]] <- row_metric(basin_id, src, "mad", mad_obs, mad_vec)
    rows[[length(rows) + 1]] <- row_metric(basin_id, src, "mad_ratio",
                                           mad_obs / mad_ens_mean,
                                           mad_vec / mad_ens_mean)
  }
  
  ## ---- 5. 95th - 5th percentile range (central 90% spread) -------------------
  # One value from observations
  p95_05_obs <- diff(quantile(obs_b, c(0.05, 0.95), na.rm = TRUE))
  
  # One value per ensemble member
  p95_05_vec <- apply(ensC, 2, function(x) {
    if (sum(!is.na(x)) < 10) return(NA)
    q <- quantile(x, c(0.05, 0.95), na.rm = TRUE)
    q[2] - q[1]  # 95th - 5th
  })
  p95_05_ens_mean <- mean(p95_05_vec, na.rm = TRUE)
  
  if (p95_05_ens_mean > 0 && !is.na(p95_05_obs)) {
    rows[[length(rows) + 1]] <- row_metric(basin_id, src, "p95_05_range", p95_05_obs, p95_05_vec)
    rows[[length(rows) + 1]] <- row_metric(basin_id, src, "p95_05_ratio",
                                           p95_05_obs / p95_05_ens_mean,
                                           p95_05_vec / p95_05_ens_mean)
  }
  
  ## ---- Combine all results ----------------------------------------------------
  if (length(rows) == 0) {
    stop("No valid metrics could be computed")
  }
  
  spread_table <- do.call(rbind, rows)
  
  # Remove the 'basin' column to avoid duplication
  spread_table$basin <- NULL
  
  # Add ALL Gattrs columns
  n_rows <- nrow(spread_table)
  spread_table <- cbind(
    Gattrs[rep(1, n_rows), , drop = FALSE],
    spread_table
  )
  
  # Add interpretation flags
  spread_table$good_match <- spread_table$percentile >= 0.05 & 
                             spread_table$percentile <= 0.95
  
  # For ratio metrics, flag if close to 1 (within 20%)
  ratio_rows <- grep("_ratio", spread_table$metric)
  spread_table$ratio_good <- NA
  spread_table$ratio_good[ratio_rows] <- 
    spread_table$obs[ratio_rows] >= 0.8 & 
    spread_table$obs[ratio_rows] <= 1.2
  
  ## ---- Summary: How consistent is the ensemble? -------------------------------
  ensemble_consistency <- cbind(
    Gattrs,
    data.frame(
      source = src,
      n_members = n_members,
      
      # How much do ensemble members agree on variability?
      sd_ensemble_cv = if(length(sd_vec) > 1) sd(sd_vec, na.rm = TRUE) / mean(sd_vec, na.rm = TRUE) else NA,
      amp_ensemble_cv = if(length(amp_vec) > 1) sd(amp_vec, na.rm = TRUE) / mean(amp_vec, na.rm = TRUE) else NA,
      
      # What fraction of members are within 20% of observed?
      frac_good_sd = if(exists("sd_obs") && !is.na(sd_obs)) 
        mean(abs(sd_vec - sd_obs) / sd_obs <= 0.2, na.rm = TRUE) else NA,
      frac_good_amp = if(exists("amp_obs") && !is.na(amp_obs)) 
        mean(abs(amp_vec - amp_obs) / amp_obs <= 0.2, na.rm = TRUE) else NA,
      
      # Key values for reference
      obs_sd = if(exists("sd_obs")) sd_obs else NA,
      obs_amp = if(exists("amp_obs")) amp_obs else NA,
      obs_p95_05 = if(exists("p95_05_obs")) p95_05_obs else NA,
      ens_mean_sd = if(exists("sd_ens_mean")) sd_ens_mean else NA,
      ens_mean_amp = if(exists("amp_ens_mean")) amp_ens_mean else NA,
      ens_mean_p95_05 = if(exists("p95_05_ens_mean")) p95_05_ens_mean else NA,
      
      # Where does obs fall?
      obs_sd_percentile = spread_table$percentile[spread_table$metric == "sd"][1],
      obs_amp_percentile = spread_table$percentile[spread_table$metric == "amp"][1],
      obs_p95_05_percentile = spread_table$percentile[spread_table$metric == "p95_05_range"][1]
    )
  )
  
  # Return everything
  list(
    metrics = spread_table,
    summary = ensemble_consistency,
    interpretation = create_interpretation(spread_table, ensemble_consistency)
  )
}

# Improved interpretation function focused on ensemble assessment
create_interpretation <- function(spread_table, consistency) {
  interp <- list()
  # Extract key values
  sd_pct <- consistency$obs_sd_percentile[1]
  amp_pct <- consistency$obs_amp_percentile[1]
  p95_05_pct <- consistency$obs_p95_05_percentile[1]
  # Basin and model context
  interp$basin <- sprintf(
    "%s | %s | %.0f km²", 
    consistency$ID[1],
    consistency$name[1],
    consistency$area[1]
  )
  
  interp$ensemble <- sprintf(
    "%s ensemble: %d members", 
    consistency$source[1], 
    consistency$n_members[1]
  )
  
  # Create a unified assessment function
  assess_metric <- function(pct, metric_name) {
    if (is.na(pct)) return(list(status = "No data", symbol = "—"))
    
    if (pct < 0.05) {
      list(status = "highly overdispersive", symbol = "⚠️⚠️", 
           detail = sprintf("p=%.3f", pct))
    } else if (pct < 0.10) {
      list(status = "overdispersive", symbol = "⚠️",
           detail = sprintf("p=%.3f", pct))
    } else if (pct > 0.95) {
      list(status = "highly underdispersive", symbol = "⚠️⚠️",
           detail = sprintf("p=%.3f", pct))
    } else if (pct > 0.90) {
      list(status = "underdispersive", symbol = "⚠️",
           detail = sprintf("p=%.3f", pct))
    } else {
      list(status = "well-calibrated", symbol = "✓",
           detail = sprintf("p=%.2f", pct))
    }
  }
  
  # Assess each metric
  var_assess <- assess_metric(sd_pct, "variability")
  amp_assess <- assess_metric(amp_pct, "amplitude")
  tail_assess <- assess_metric(p95_05_pct, "tail spread")
  # Structured assessments
  interp$variability <- sprintf("%s Variability: %s (%s)", 
                               var_assess$symbol, var_assess$status, var_assess$detail)
  
  interp$amplitude <- sprintf("%s Amplitude: %s (%s)", 
                             amp_assess$symbol, amp_assess$status, amp_assess$detail)
  
  interp$tail_behavior <- sprintf("%s Tail behavior: %s (%s)", 
                                 tail_assess$symbol, tail_assess$status, tail_assess$detail)
  
  # Quantitative comparison
  sd_ratio <- consistency$obs_sd[1] / consistency$ens_mean_sd[1]
  amp_ratio <- consistency$obs_amp[1] / consistency$ens_mean_amp[1]
  p95_ratio <- consistency$obs_p95_05[1] / consistency$ens_mean_p95_05[1]
  
  interp$ratios <- sprintf(
    "Observation/Ensemble ratios: σ=%.2f | Amplitude=%.2f | 95-5 range=%.2f",
    sd_ratio, amp_ratio, p95_ratio
  )
  
  # Ensemble internal consistency
  sd_consistency <- consistency$sd_ensemble_cv[1]
  interp$ensemble_consistency <- sprintf(
    "Ensemble internal consistency: CV=%.2f %s",
    sd_consistency,
    ifelse(sd_consistency > 0.3, "(high spread)", "(consistent)")
  )
  
  # Member performance
  interp$member_performance <- sprintf(
    "Members within 20%% of observed: %.0f%% (σ) | %.0f%% (amplitude)",
    ifelse(is.na(consistency$frac_good_sd[1]), 0, consistency$frac_good_sd[1] * 100),
    ifelse(is.na(consistency$frac_good_amp[1]), 0, consistency$frac_good_amp[1] * 100)
  )
  
  # Overall reliability assessment
  well_calibrated <- c(var_assess$status, amp_assess$status, tail_assess$status)
  n_good <- sum(well_calibrated == "well-calibrated")
  n_slightly_off <- sum(well_calibrated %in% c("overdispersive", "underdispersive"))
  n_highly_off <- sum(well_calibrated %in% c("highly overdispersive", "highly underdispersive"))
  
  if (n_highly_off >= 2) {
    interp$reliability <- "✗ Unreliable: Systematic calibration issues"
    interp$recommendation <- "Ensemble poorly represents observed variability"
  } else if (n_good >= 2) {
    interp$reliability <- "✓ Reliable: Well-calibrated ensemble"
    interp$recommendation <- "Suitable for uncertainty quantification"
  } else if (n_good >= 1 && n_highly_off == 0) {
    interp$reliability <- "⚠️ Conditionally reliable: Mixed performance"
    interp$recommendation <- "Use with awareness of specific limitations"
  } else {
    interp$reliability <- "✗ Unreliable: Poor variability representation"
    interp$recommendation <- "Consider alternative models or bias correction"
  }
  # Key findings
  key_findings <- c()
  if (sd_pct < 0.1 && amp_pct < 0.1) {
    key_findings <- c(key_findings, "Systematic overdispersion across metrics")
  } else if (sd_pct > 0.9 && amp_pct > 0.9) {
    key_findings <- c(key_findings, "Systematic underdispersion across metrics")
  } else if (abs(sd_pct - 0.5) > 0.4 && abs(amp_pct - 0.5) < 0.2) {
    key_findings <- c(key_findings, "Variability issues but amplitude well-captured")
  }
  
  if (length(key_findings) > 0) {
    interp$key_findings <- paste(key_findings, collapse="; ")
  }
  
  return(interp)
}
 
# =============================================================================
# AMPLITUDE AND VARIANCE ANALYSIS
# =============================================================================
# Purpose: Assess if climate model ensembles reproduce observed variability
# Key metrics: SD, amplitude (max-min), IQR, MAD, 95-5 percentile range
# Criterion: Observations should fall within 10-90% of ensemble distribution
# =============================================================================

# -----------------------------------------------------------------------------
# 1. HELPER FUNCTION: Compute ensemble statistics
# -----------------------------------------------------------------------------
summarise_vec3 <- function(v) {
  # Input: numeric vector (already cleaned of NAs)
  # Output: named vector of statistics
  
  n <- length(v)
  
  if (n == 0) {
    return(c(min = NA_real_, p05 = NA_real_, q25 = NA_real_,
             median = NA_real_, q75 = NA_real_, p95 = NA_real_,
             max = NA_real_, mean = NA_real_, sd = NA_real_,
             skew = NA_real_, kurt = NA_real_))
  }
  
  # Sort once for efficiency
  v_sorted <- sort(v)
  
  # Compute quantiles directly from sorted vector
  quants <- c(
    min = v_sorted[1],
    p05 = v_sorted[max(1, round(n * 0.05))],
    q25 = v_sorted[max(1, round(n * 0.25))],
    median = if (n %% 2 == 1) v_sorted[(n + 1) / 2] else 
             (v_sorted[n / 2] + v_sorted[n / 2 + 1]) / 2,
    q75 = v_sorted[min(n, round(n * 0.75))],
    p95 = v_sorted[min(n, round(n * 0.95))],
    max = v_sorted[n]
  )
  
  # Basic statistics
  m <- mean(v)
  s <- sd(v)
  
  # Higher moments (only if enough data)
  if (n >= 3 && s > 0) {
    v_centered <- v - m
    m3 <- mean(v_centered^3)
    m4 <- mean(v_centered^4)
    
    # Small sample corrections
    skew_val <- (sqrt(n * (n - 1)) / (n - 2)) * (m3 / s^3)
    kurt_val <- ((n - 1) / ((n - 2) * (n - 3))) * 
                ((n + 1) * (m4 / s^4) - 3 * (n - 1))
  } else {
    skew_val <- NA_real_
    kurt_val <- NA_real_
  }
  
  c(quants, mean = m, sd = s, skew = skew_val, kurt = kurt_val)
}

# -----------------------------------------------------------------------------
# 2. CORE FUNCTION: Compare single observation to ensemble distribution
# -----------------------------------------------------------------------------
row_metric <- function(basin, src, metric, obs, ens_vec) {
  # Inputs:
  # - basin: basin ID
  # - src: source model (CESM2/IPSL)
  # - metric: what we're measuring (sd, amp, etc.)
  # - obs: single observed value
  # - ens_vec: vector of ensemble values (one per member)
  
  # Input validation
  if (!is.numeric(obs) || length(obs) != 1) {
    stop("'obs' must be a single numeric value")
  }
  if (!is.numeric(ens_vec) || length(ens_vec) == 0) {
    stop("'ens_vec' must be a non-empty numeric vector")
  }
  
  # Remove NAs
  ens_clean <- ens_vec[!is.na(ens_vec)]
  
  # Handle edge case: all NAs
  if (length(ens_clean) == 0) {
    return(data.frame(
      basin = basin,
      source = src,
      metric = metric,
      obs = obs,
      percentile = NA_real_,
      p_value = NA_real_,
      n_valid = 0L,
      min = NA_real_, p05 = NA_real_, q25 = NA_real_,
      median = NA_real_, q75 = NA_real_, p95 = NA_real_,
      max = NA_real_, mean = NA_real_, sd = NA_real_,
      skew = NA_real_, kurt = NA_real_,
      row.names = NULL
    ))
  }
  
  # Calculate percentile (0-100 scale)
  if (is.na(obs)) {
    pct <- NA_real_
    p_value <- NA_real_
  } else {
    # Fraction of ensemble values <= observed
    pct_fraction <- mean(ens_clean <= obs)
    pct <- pct_fraction * 100  # Convert to percentage
    
    # Two-sided p-value with continuity correction
    n <- length(ens_clean)
    n_less <- sum(ens_clean < obs)
    n_equal <- sum(ens_clean == obs)
    pct_corrected <- (n_less + 0.5 * n_equal) / n
    p_value <- 2 * min(pct_corrected, 1 - pct_corrected)
  }
  
  # Get ensemble statistics
  ens_stats <- summarise_vec3(ens_clean)
  
  # Return results
  data.frame(
    basin = basin,
    source = src,
    metric = metric,
    obs = obs,
    percentile = pct,
    p_value = p_value,
    n_valid = length(ens_clean),
    min = ens_stats["min"],
    p05 = ens_stats["p05"],
    q25 = ens_stats["q25"],
    median = ens_stats["median"],
    q75 = ens_stats["q75"],
    p95 = ens_stats["p95"],
    max = ens_stats["max"],
    mean = ens_stats["mean"],
    sd = ens_stats["sd"],
    skew = ens_stats["skew"],
    kurt = ens_stats["kurt"],
    row.names = NULL
  )
}
# -----------------------------------------------------------------------------
# 3. MAIN FUNCTION: Compute all variability metrics
# -----------------------------------------------------------------------------
basic_spread_metrics <- function(obs_b, ensC, Gattrs, src) {
  # Inputs:
  # - obs_b: observed time series (vector)
  # - ensC: ensemble matrix (rows = time, cols = members)
  # - Gattrs: single-row data frame with basin attributes
  # - src: model name (CESM2/IPSL)
  
  # Input validation
  if (!is.data.frame(Gattrs) || nrow(Gattrs) != 1) {
    stop("Gattrs must be a single-row data.frame")
  }
  
  if (length(obs_b) < 10) {
    warning("Observed time series has < 10 points, results may be unreliable")
  }
  
  if (!is.matrix(ensC) && !is.data.frame(ensC)) {
    stop("ensC must be a matrix or data.frame")
  }
  
  basin_id <- Gattrs$ID
  basin_name <- Gattrs$name
  
  # Convert to matrix and check validity
  ensC <- as.matrix(ensC)
  valid_members <- colSums(!is.na(ensC)) > 10
  ensC <- ensC[, valid_members, drop = FALSE]
  n_members <- ncol(ensC)
  
  if (n_members == 0) {
    stop(sprintf("Basin %s (%s) / %s: no valid ensemble members",
                 basin_id, basin_name, src))
  }
  
  cat(sprintf("Processing %s for basin %s with %d ensemble members\n", 
              src, basin_name, n_members))
  
  # Initialize results storage
  rows <- list()
  row_count <- 0
  
  # ---- METRIC 1: Standard deviation ----
  sd_obs <- sd(obs_b, na.rm = TRUE)
  sd_vec <- apply(ensC, 2, sd, na.rm = TRUE)
  sd_vec <- sd_vec[!is.na(sd_vec)]  # Remove NA values
  
  if (length(sd_vec) > 0 && !is.na(sd_obs)) {
    sd_ens_mean <- mean(sd_vec)
    if (sd_ens_mean > 0) {
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "sd", sd_obs, sd_vec)
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "sd_ratio",
                                     sd_obs / sd_ens_mean,
                                     sd_vec / sd_ens_mean)
    }
  }
  
  # ---- METRIC 2: Amplitude (max - min) ----
  amp_obs <- diff(range(obs_b, na.rm = TRUE))
  amp_vec <- apply(ensC, 2, function(x) {
    valid_x <- x[!is.na(x)]
    if (length(valid_x) < 2) return(NA)
    diff(range(valid_x))
  })
  amp_vec <- amp_vec[!is.na(amp_vec)]
  
  if (length(amp_vec) > 0 && !is.na(amp_obs)) {
    amp_ens_mean <- mean(amp_vec)
    if (amp_ens_mean > 0) {
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "amp", amp_obs, amp_vec)
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "amp_ratio",
                                     amp_obs / amp_ens_mean,
                                     amp_vec / amp_ens_mean)
    }
  }
  
  # ---- METRIC 3: Interquartile range ----
  iqr_obs <- IQR(obs_b, na.rm = TRUE)
  iqr_vec <- apply(ensC, 2, IQR, na.rm = TRUE)
  iqr_vec <- iqr_vec[!is.na(iqr_vec)]
  
  if (length(iqr_vec) > 0 && !is.na(iqr_obs)) {
    iqr_ens_mean <- mean(iqr_vec)
    if (iqr_ens_mean > 0) {
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "iqr", iqr_obs, iqr_vec)
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "iqr_ratio",
                                     iqr_obs / iqr_ens_mean,
                                     iqr_vec / iqr_ens_mean)
    }
  }
  
  # ---- METRIC 4: MAD (Median Absolute Deviation) ----
  mad_obs <- mad(obs_b, constant = 1.4826, na.rm = TRUE)
  mad_vec <- apply(ensC, 2, mad, constant = 1.4826, na.rm = TRUE)
  mad_vec <- mad_vec[!is.na(mad_vec)]
  
  if (length(mad_vec) > 0 && !is.na(mad_obs)) {
    mad_ens_mean <- mean(mad_vec)
    if (mad_ens_mean > 0) {
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "mad", mad_obs, mad_vec)
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "mad_ratio",
                                     mad_obs / mad_ens_mean,
                                     mad_vec / mad_ens_mean)
    }
  }
  
  # ---- METRIC 5: 95th - 5th percentile range ----
  p95_05_obs <- diff(quantile(obs_b, c(0.05, 0.95), na.rm = TRUE))
  p95_05_vec <- apply(ensC, 2, function(x) {
    valid_x <- x[!is.na(x)]
    if (length(valid_x) < 10) return(NA)
    q <- quantile(valid_x, c(0.05, 0.95))
    q[2] - q[1]
  })
  p95_05_vec <- p95_05_vec[!is.na(p95_05_vec)]
  
  if (length(p95_05_vec) > 0 && !is.na(p95_05_obs)) {
    p95_05_ens_mean <- mean(p95_05_vec)
    if (p95_05_ens_mean > 0) {
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "p95_05_range", p95_05_obs, p95_05_vec)
      row_count <- row_count + 1
      rows[[row_count]] <- row_metric(basin_id, src, "p95_05_ratio",
                                     p95_05_obs / p95_05_ens_mean,
                                     p95_05_vec / p95_05_ens_mean)
    }
  }
  
  # Check if we have any valid metrics
  if (length(rows) == 0) {
    stop("No valid metrics could be computed")
  }
  
  # Combine all metrics
  spread_table <- do.call(rbind, rows)
  spread_table$basin <- NULL  # Remove to avoid duplication
  
  # Add basin attributes
  n_rows <- nrow(spread_table)
  spread_table <- cbind(
    Gattrs[rep(1, n_rows), , drop = FALSE],
    spread_table
  )
  
  # Add quality flags
  # Good match: percentile between 10-90
  spread_table$good_match <- spread_table$percentile >= 10 & 
                            spread_table$percentile <= 90
  
  # For ratios: good if within 20% of 1
  ratio_rows <- grep("_ratio", spread_table$metric)
  spread_table$ratio_good <- NA
  if (length(ratio_rows) > 0) {
    spread_table$ratio_good[ratio_rows] <- 
      spread_table$obs[ratio_rows] >= 0.8 & 
      spread_table$obs[ratio_rows] <= 1.2
  }
  
  # ---- Create ensemble consistency summary ----
  ensemble_consistency <- cbind(
    Gattrs,
    data.frame(
      source = src,
      n_members = n_members,
      
      # Ensemble spread (CV)
      sd_ensemble_cv = if(exists("sd_vec") && length(sd_vec) > 1 && exists("sd_ens_mean") && sd_ens_mean > 0) {
        sd(sd_vec) / sd_ens_mean
      } else NA,
      
      amp_ensemble_cv = if(exists("amp_vec") && length(amp_vec) > 1 && exists("amp_ens_mean") && amp_ens_mean > 0) {
        sd(amp_vec) / amp_ens_mean
      } else NA,
      
      # Fraction within 20% of observed
      frac_good_sd = if(exists("sd_obs") && exists("sd_vec") && !is.na(sd_obs) && sd_obs > 0) {
        mean(abs(sd_vec - sd_obs) / sd_obs <= 0.2)
      } else NA,
      
      frac_good_amp = if(exists("amp_obs") && exists("amp_vec") && !is.na(amp_obs) && amp_obs > 0) {
        mean(abs(amp_vec - amp_obs) / amp_obs <= 0.2)
      } else NA,
      
      # Observed values
      obs_sd = if(exists("sd_obs")) sd_obs else NA,
      obs_amp = if(exists("amp_obs")) amp_obs else NA,
      obs_p95_05 = if(exists("p95_05_obs")) p95_05_obs else NA,
      
      # Ensemble means
      ens_mean_sd = if(exists("sd_ens_mean")) sd_ens_mean else NA,
      ens_mean_amp = if(exists("amp_ens_mean")) amp_ens_mean else NA,
      ens_mean_p95_05 = if(exists("p95_05_ens_mean")) p95_05_ens_mean else NA,
      
      # Percentiles from spread_table
      obs_sd_percentile = spread_table$percentile[spread_table$metric == "sd"][1],
      obs_amp_percentile = spread_table$percentile[spread_table$metric == "amp"][1],
      obs_p95_05_percentile = spread_table$percentile[spread_table$metric == "p95_05_range"][1],
      
      stringsAsFactors = FALSE
    )
  )
  
  # Return results
  list(
    metrics = spread_table,
    summary = ensemble_consistency,
    interpretation = create_interpretation(spread_table, ensemble_consistency)
  )
}
# -----------------------------------------------------------------------------
# 4. INTERPRETATION FUNCTION
# -----------------------------------------------------------------------------
create_interpretation <- function(spread_table, consistency) {
  interp <- list()
  
  # Extract percentiles (0-100 scale)
  sd_pct <- consistency$obs_sd_percentile[1]
  amp_pct <- consistency$obs_amp_percentile[1]
  p95_05_pct <- consistency$obs_p95_05_percentile[1]
  
  # Basin info
  interp$basin <- sprintf(
    "%s | %s | %.0f km²", 
    consistency$ID[1],
    consistency$name[1],
    consistency$area[1]
  )
  
  interp$ensemble <- sprintf(
    "%s ensemble: %d members", 
    consistency$source[1], 
    consistency$n_members[1]
  )
  
  # Assessment function (10-90% criterion for variability)
  assess_metric <- function(pct, metric_name) {
    if (is.na(pct)) return(list(status = "No data", symbol = "—", detail = "NA"))
    
    if (pct < 5) {
      list(status = "highly overdispersive", symbol = "⚠️⚠️", 
           detail = sprintf("p=%.1f%%", pct))
    } else if (pct < 10) {
      list(status = "overdispersive", symbol = "⚠️",
           detail = sprintf("p=%.1f%%", pct))
    } else if (pct > 95) {
      list(status = "highly underdispersive", symbol = "⚠️⚠️",
           detail = sprintf("p=%.1f%%", pct))
    } else if (pct > 90) {
      list(status = "underdispersive", symbol = "⚠️",
           detail = sprintf("p=%.1f%%", pct))
    } else {
      list(status = "well-calibrated", symbol = "✓",
           detail = sprintf("p=%.0f%%", pct))
    }
  }
  
  # Assess each metric
  var_assess <- assess_metric(sd_pct, "variability")
  amp_assess <- assess_metric(amp_pct, "amplitude")
  tail_assess <- assess_metric(p95_05_pct, "tail spread")
  
  # Format assessments
  interp$variability <- sprintf("%s Variability: %s (%s)", 
                               var_assess$symbol, var_assess$status, var_assess$detail)
  
  interp$amplitude <- sprintf("%s Amplitude: %s (%s)", 
                             amp_assess$symbol, amp_assess$status, amp_assess$detail)
  
  interp$tail_behavior <- sprintf("%s 95-5 range: %s (%s)", 
                                 tail_assess$symbol, tail_assess$status, tail_assess$detail)
  
  # Ratios (with NA handling)
  sd_ratio <- if(!is.na(consistency$obs_sd[1]) && !is.na(consistency$ens_mean_sd[1]) && 
                 consistency$ens_mean_sd[1] > 0) {
    consistency$obs_sd[1] / consistency$ens_mean_sd[1]
  } else NA
  
  amp_ratio <- if(!is.na(consistency$obs_amp[1]) && !is.na(consistency$ens_mean_amp[1]) && 
                  consistency$ens_mean_amp[1] > 0) {
    consistency$obs_amp[1] / consistency$ens_mean_amp[1]
  } else NA
  
  p95_ratio <- if(!is.na(consistency$obs_p95_05[1]) && !is.na(consistency$ens_mean_p95_05[1]) && 
                  consistency$ens_mean_p95_05[1] > 0) {
    consistency$obs_p95_05[1] / consistency$ens_mean_p95_05[1]
  } else NA
  
  interp$ratios <- sprintf(
    "Observation/Ensemble ratios: σ=%.2f | Amplitude=%.2f | 95-5 range=%.2f",
    ifelse(is.na(sd_ratio), NA, sd_ratio),
    ifelse(is.na(amp_ratio), NA, amp_ratio),
    ifelse(is.na(p95_ratio), NA, p95_ratio)
  )
  
  # Ensemble consistency
  sd_cv <- consistency$sd_ensemble_cv[1]
  interp$ensemble_consistency <- if(!is.na(sd_cv)) {
    sprintf("Ensemble internal consistency: CV=%.2f %s",
            sd_cv,
            ifelse(sd_cv > 0.3, "(high spread)", "(consistent)"))
  } else {
    "Ensemble internal consistency: NA"
  }
  
  # Member performance
  interp$member_performance <- sprintf(
    "Members within 20%% of observed: %.0f%% (σ) | %.0f%% (amplitude)",
    ifelse(is.na(consistency$frac_good_sd[1]), 0, consistency$frac_good_sd[1] * 100),
    ifelse(is.na(consistency$frac_good_amp[1]), 0, consistency$frac_good_amp[1] * 100)
  )
  
  # Overall reliability
  statuses <- c(var_assess$status, amp_assess$status, tail_assess$status)
  statuses <- statuses[statuses != "No data"]  # Remove NA cases
  
  if (length(statuses) == 0) {
    interp$reliability <- "— Insufficient data"
    interp$recommendation <- "Cannot assess - too many missing values"
  } else {
    n_good <- sum(statuses == "well-calibrated")
    n_highly_off <- sum(statuses %in% c("highly overdispersive", "highly underdispersive"))
    
    if (n_highly_off >= 2) {
      interp$reliability <- "✗ Unreliable: Systematic calibration issues"
      interp$recommendation <- "Ensemble poorly represents observed variability"
    } else if (n_good >= 2) {
      interp$reliability <- "✓ Reliable: Well-calibrated ensemble"
      interp$recommendation <- "Suitable for uncertainty quantification"
    } else if (n_good >= 1 && n_highly_off == 0) {
      interp$reliability <- "⚠️ Conditionally reliable: Mixed performance"
      interp$recommendation <- "Use with awareness of specific limitations"
    } else {
      interp$reliability <- "✗ Unreliable: Poor variability representation"
      interp$recommendation <- "Consider alternative models or bias correction"
    }
  }
  
  # Key findings
  key_findings <- c()
  if (!is.na(sd_pct) && !is.na(amp_pct)) {
    if (sd_pct < 10 && amp_pct < 10) {
      key_findings <- c(key_findings, "Systematic overdispersion")
    } else if (sd_pct > 90 && amp_pct > 90) {
      key_findings <- c(key_findings, "Systematic underdispersion")
    }
  }
  
  if (length(key_findings) > 0) {
    interp$key_findings <- paste(key_findings, collapse = "; ")
  }
  
  return(interp)
}
interpretation_to_df <- function(interp_list, Gattrs) {
  # Extract numeric values from strings using regex
  extract_ratio <- function(text, metric) {
    pattern <- paste0(metric, "=([0-9.]+)")
    as.numeric(gsub(pattern, "\\1", regmatches(text, regexpr(pattern, text))))
  }
  
  extract_cv <- function(text) {
    as.numeric(gsub(".*CV=([0-9.]+).*", "\\1", text))
  }
  
  extract_percent <- function(text, metric) {
    pattern <- paste0("([0-9]+)% \\(", metric, "\\)")
    as.numeric(gsub(pattern, "\\1", regmatches(text, regexpr(pattern, text))))
  }
  
  # Parse the interpretation strings
  ratios_text <- interp_list$ratios
  
  # Create the data frame row
  df <- data.frame(
    # All Gattrs columns
    ID = Gattrs$ID,
    name = Gattrs$name,
    bd_id = Gattrs$bd_id,
    area = Gattrs$area,
    C_lon = Gattrs$C_lon,
    C_lat = Gattrs$C_lat,
    IrrigatPct = Gattrs$IrrigatPct,
    
    # Model info
    model = gsub(" ensemble:.*", "", interp_list$ensemble),
    n_members = as.numeric(gsub(".*: ([0-9]+) members", "\\1", interp_list$ensemble)),
    
    # Variability assessments
    var_status = gsub(".*Variability: ([a-z ]+) \\(.*", "\\1", interp_list$variability),
    var_pvalue = as.numeric(gsub(".*\\(p=([0-9.]+)\\)", "\\1", interp_list$variability)),
    
    amp_status = gsub(".*Amplitude: ([a-z ]+) \\(.*", "\\1", interp_list$amplitude),
    amp_pvalue = as.numeric(gsub(".*\\(p=([0-9.]+)\\)", "\\1", interp_list$amplitude)),
    
    tail_status = gsub(".*Tail behavior: ([a-z ]+) \\(.*", "\\1", interp_list$tail_behavior),
    tail_pvalue = as.numeric(gsub(".*\\(p=([0-9.]+)\\)", "\\1", interp_list$tail_behavior)),
    
    # Ratios
    sd_ratio = extract_ratio(ratios_text, "σ"),
    amp_ratio = extract_ratio(ratios_text, "Amplitude"),
    p95_05_ratio = extract_ratio(ratios_text, "95-5 range"),
    
    # Ensemble consistency
    ensemble_cv = extract_cv(interp_list$ensemble_consistency),
    
    # Member performance
    pct_good_sd = extract_percent(interp_list$member_performance, "σ"),
    pct_good_amp = extract_percent(interp_list$member_performance, "amplitude"),
    
    # Overall assessment
    reliability = gsub("([✓⚠️✗]) ([^:]+):.*", "\\2", interp_list$reliability),
    recommendation = interp_list$recommendation,
    key_findings = ifelse(is.null(interp_list$key_findings), NA, interp_list$key_findings),
    
    stringsAsFactors = FALSE
  )
  
  return(df)}
# Function to combine CESM and IPSL results
 combine_model_interpretations<- function(cesm_result, ipsl_result, Gattrs) {

  # Convert interpretations to data frames
  cesm_df <- interpretation_to_df(cesm_result$interpretation, Gattrs)
  ipsl_df <- interpretation_to_df(ipsl_result$interpretation, Gattrs)
  
  # Combine into one data frame
  combined_df <- rbind(cesm_df, ipsl_df)
  
  return(combined_df)
 }
############   Extremes  ## ####################################################
#  Extremes Extremes Extremes Extremes Extremes Extremes Extremes Extremes
#  Extremes Extremes Extremes Extremes Extremes Extremes Extremes Extremes
# Extreme coverage analysis - consistent with amplitude/variance approach
extreme_coverage_analysis <- function(obs, ens_full, Gattrs, src) {
  #— 1. Input validation -----------------------------------------------------
  if (!is.numeric(obs) || length(obs) < 1) {
    stop("`obs` must be a non-empty numeric vector")
  }
  if (!is.matrix(ens_full) && !is.data.frame(ens_full)) {
    stop("`ens_full` must be a matrix or data.frame")
  }
  if (!is.data.frame(Gattrs) || nrow(Gattrs) != 1) {
    stop("`Gattrs` must be a single-row data.frame")
  }
  
  #— 2. Observed extremes ----------------------------------------------------
  obs_min <- min(obs, na.rm = TRUE)
  obs_max <- max(obs, na.rm = TRUE)
  
  #— 3. Filter ensemble members with ≥10 valid points -----------------------
  ens2 <- as.matrix(ens_full)
  valid <- colSums(!is.na(ens2)) >= 10
  ens2  <- ens2[, valid, drop = FALSE]
  n_members <- ncol(ens2)
  if (n_members < 1) stop("No ensemble members with ≥10 valid points")
  
  #— 4. Compute member-wise extremes ----------------------------------------
  member_mins <- apply(ens2, 2, min, na.rm = TRUE)
  member_maxs <- apply(ens2, 2, max, na.rm = TRUE)
  
  #— 5. Count reachability --------------------------------------------------
  n_reach_min <- sum(member_mins <= obs_min, na.rm = TRUE)
  n_reach_max <- sum(member_maxs >= obs_max, na.rm = TRUE)
  frac_reach_min <- n_reach_min / n_members
  frac_reach_max <- n_reach_max / n_members
  
  #— 6. Classification based on reachability --------------------------------
  extreme_status <- dplyr::case_when(
    frac_reach_min == 0 & frac_reach_max == 0 ~ 
      "Neither extreme reached",
    frac_reach_min == 0 ~ 
      "Minimum never reached",
    frac_reach_max == 0 ~ 
      "Maximum never reached",
    frac_reach_min < 0.05 & frac_reach_max < 0.05 ~ 
      "Both extremes rarely reached (<5%)",
    frac_reach_min < 0.05 ~ 
      "Minimum rarely reached (<5%)",
    frac_reach_max < 0.05 ~ 
      "Maximum rarely reached (<5%)",
    frac_reach_min >= 0.50 & frac_reach_max >= 0.50 ~ 
      "Both extremes well represented (≥50%)",
    frac_reach_min >= 0.50 ~ 
      "Maximum moderate, minimum well represented",
    frac_reach_max >= 0.50 ~ 
      "Minimum moderate, maximum well represented",
    TRUE ~ 
      "Moderate coverage"
  )
  
  #— 7. Assemble and return -------------------------------------------------
  result <- cbind(
    Gattrs,
    data.frame(
      source            = src,
      n_members         = n_members,
      obs_min           = obs_min,
      obs_max           = obs_max,
      ens_min           = min(member_mins, na.rm = TRUE),
      ens_max           = max(member_maxs, na.rm = TRUE),
      n_reach_min       = n_reach_min,
      n_reach_max       = n_reach_max,
      frac_reach_min    = frac_reach_min,
      frac_reach_max    = frac_reach_max,
      extreme_status    = extreme_status,
      stringsAsFactors  = FALSE
    )
  )
  return(result)
}

process_extreme_analysis <- function(results) {
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
# Interpretation function
interpret_extreme_results <- function(analysis_results) {
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
#######  Wavelete analaysis the clock of the  global TWS  ###########
# =============================================================================
# WAVELET ANALYSIS FOR STL-FILTERED CLIMATE DATA
# Climate period classification
classify_period <- function(period_years) {
  case_when(
    period_years >= 2 & period_years < 4   ~ "ENSO-core (2-4y)",
    period_years >= 4 & period_years < 8   ~ "ENSO-extended (4-8y)", 
    period_years >= 8 & period_years < 30  ~ "Decadal (8-30y)",
    period_years >= 30 & period_years < 100 ~ "Multi-decadal (30-100y)",
    period_years >= 100                    ~ "Centennial (100y+)",
    TRUE                                   ~ "Residual (<2y)"
  )
}
# Core wavelet function - handles numeric vectors only
get_top_periods_stl <- function(ts, dt = 1/12, top_k = 4) {
  # ts is ALWAYS a numeric vector:
  # - For observations: obs_ts (single numeric vector)
  # - For ensemble members: ensC[, j] or ensP[, j] (single column = single member)
  
  # Ensure dt is numeric
  dt <- as.numeric(dt)
  top_k <- as.integer(top_k)
  
  # Convert numeric vector to data.frame as required by WaveletComp
  ts_df <- data.frame(y = as.numeric(ts))
  
  w <- WaveletComp::analyze.wavelet(
    ts_df,                      # Properly formatted data.frame
    my.series = "y",            # Column name
    dt = dt,                    # 1/12 for monthly data (12 months = 1 year)
    dj = 1/20,                  # Fine frequency resolution
    lowerPeriod = 2 * dt,       # Minimum ~2 months
    upperPeriod = floor(length(ts)/3) * dt,  # Maximum ~1/3 of time series length
    make.pval = TRUE,          # Skip p-values for speed
    verbose = FALSE,            # Suppress output
    n.sim = 150
  )
  
  per <- w$Period      # Periods automatically in years due to dt = 1/12
  pow <- w$Power.avg   # Global wavelet power (averaged over time)
  
  # Get top k periods by power
  o <- order(pow, decreasing = TRUE)[1:top_k]
  
  data.frame(
    period = per[o],           # Periods in years
    power = pow[o],            # Wavelet power
    period_class = classify_period(per[o])  # Climate classification
  )
}
# Main analysis function
ensemble_wavelet_analysis <- function(obs_ts, ensC, ensP, Gattrs, top_k = 4) {
  
  # Observed periods
  obs_ts <-as.ts(obs_b, freq=12,start=c(2002,01))
  peaks_obs <- get_top_periods_stl(obs_b, top_k = top_k)
  peaks_obs$member <- 0
  peaks_obs$data <- "GRACE(-FO)"
  peaks_obs$rank <- 1:nrow(peaks_obs)
  
library(furrr)
library(progressr)
# Setup
n_cores <- parallel::detectCores() - 1
plan(multisession, workers = n_cores)

# Process CESM2
suppressWarnings({
 with_progress({
   peaks_cesm <- future_map_dfr(
     seq_len(ncol(ensC)),
     function(j) {
       result <- get_top_periods_stl(ensC[, j], top_k = top_k)
       result$member <- j
       result$data <- "CESM2"
       result$rank <- 1:nrow(result)
       return(result)
     },
     .progress = TRUE,
     .options = furrr_options(seed = TRUE, packages = "WaveletComp")
   )
 })
})

# Process IPSL
suppressWarnings({
 with_progress({
   peaks_ipsl <- future_map_dfr(
     seq_len(ncol(ensP)),
     function(j) {
       result <- get_top_periods_stl(ensP[, j], top_k = top_k)
       result$member <- j
       result$data <- "IPSL"
       result$rank <- 1:nrow(result)
       return(result)
     },
     .progress = TRUE,
     .options = furrr_options(seed = TRUE, packages = "WaveletComp")
   )
 })
})

# Reset
plan(sequential)
  # Combine and attach attributes
  Wave <- rbind(peaks_obs, peaks_cesm, peaks_ipsl)
  Wave_full <- attach_attrs(Wave, Gattrs)
  
  # Answer 5 key questions
  list(
    data = Wave_full,
    Q1_dominant_modes = analyze_dominant_modes(Wave_full),
    Q2_timescale_representation = analyze_timescale_representation(Wave_full),
    Q3_ensemble_consistency = analyze_ensemble_consistency(Wave_full),
    Q4_spectral_biases = analyze_dominant_mode_reproduction(Wave_full)
  )
}

# Q1: Dominant modes
analyze_dominant_modes <- function(Wave_full) {
  obs_modes <- Wave_full %>% 
    filter(data == "GRACE(-FO)") %>%
    arrange(rank) %>%
    dplyr::select(rank, period, power, period_class)
  
  dominant_summary <- obs_modes %>%
    summarise(
      primary_period = period[1],
      primary_class = period_class[1],
      primary_power = power[1],
      secondary_period = if(n() > 1) period[2] else NA,
      secondary_class = if(n() > 1) period_class[2] else NA,
      total_modes = n()
    )
  interpretation <- case_when(
    dominant_summary$primary_class == "Centennial (100y+)" ~ 
      "Centennial-scale variability dominates - climate regime shifts",
    dominant_summary$primary_class == "Multi-decadal (30-100y)" ~ 
      "Multi-decadal mode dominates - Atlantic/Pacific basin oscillations (AMO/PDO)",
    dominant_summary$primary_class == "Decadal (8-30y)" ~ 
      "Decadal mode dominates - PDO/AMO cycles, possible ENSO modulation",
    dominant_summary$primary_class == "ENSO-extended (4-8y)" ~ 
      "Extended-ENSO mode dominates - La Niña persistence, ENSO asymmetry",
    dominant_summary$primary_class == "ENSO-core (2-4y)" ~ 
      "Core-ENSO mode dominates - classic El Niño/La Niña cycles",
    TRUE ~ "High-frequency residual variability"
  )
  
  list(
    summary = dominant_summary,
    observed_modes = obs_modes,
    interpretation = interpretation
  )
}
# Q2: Timescale representation
analyze_timescale_representation <- function(Wave_full) {
  obs_periods <- Wave_full %>% 
    filter(data == "GRACE(-FO)") %>% 
    pull(period)
  representation <- Wave_full %>%
    filter(data != "GRACE(-FO)") %>%
    group_by(data) %>%
    summarise(
      min_period = min(period),
      max_period = max(period),
      period_range = max_period - min_period,
      
      obs_periods_covered = sum(sapply(obs_periods, function(p) {
        p >= min_period & p <= max_period
      })),
      obs_coverage_fraction = obs_periods_covered / length(obs_periods),
      
      dominant_obs_period = obs_periods[1],
      dominant_period_percentile = mean(period <= dominant_obs_period) * 100,
      
      .groups = 'drop'
    )
  
  representation$coverage_quality <- case_when(
    representation$obs_coverage_fraction == 1 ~ "Excellent - all periods covered",
    representation$obs_coverage_fraction >= 0.67 ~ "Good - most periods covered", 
    representation$obs_coverage_fraction >= 0.33 ~ "Partial - some periods covered",
    TRUE ~ "Poor - few periods covered"
  )
  
  representation$dominant_period_assessment <- case_when(
    representation$dominant_period_percentile >= 10 & 
    representation$dominant_period_percentile <= 90 ~ "Well-represented",
    representation$dominant_period_percentile < 10 ~ "Underdispersive",
    TRUE ~ "Overdispersive"
  )
  
  return(representation)
}
# Q3: Ensemble consistency
analyze_ensemble_consistency <- function(Wave_full) {
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




# Q4: Spectral biases
# Q4: Spectral power comparison - Does ANY model member capture GRACE's power?
#analyze_spectral_power_reproduction <- function(Wave_full) {
#  obs_data <- Wave_full %>% filter(data == "GRACE(-FO)")
  
#  if (nrow(obs_data) == 0) {
#    return(list())
#  Perfect! Now the analysis asks the right question:
#"For each GRACE-observed period/power combination, does ANY ensemble member reproduce it?"
#This tells us:
#Was the GRACE observation within the ensemble spread? (predictability question)
#Which specific members came closest? (useful for understanding internal variability)
#Does the ensemble envelope encompass GRACE? (if not, models may be missing key processes)
#The key metrics are:
#Success: At least one member within 20% of GRACE power
#Partial: At least one member within 50% of GRACE power
#Ensemble encompasses: Model min/max range includes GRACE value
#Failure: No member comes close
#This is much more meaningful than averaging across members, which would wash out the internal variability signal. 
# If GRACE happened to observe an extreme realization of TWS variability, we want to know if that extreme 
# is possible in the models, not whether it matches the ensemble mean.
# Q4: Spectral biases - CORRECTED for different record lengths

# Q4: How well do model ensembles reproduce GRACE's dominant mode?
# Q4: How well do model ensembles reproduce GRACE's dominant mode?
analyze_dominant_mode_reproduction <- function(Wave_full) {
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
# Q4: How well do model ensembles reproduce GRACE's dominant mode?
analyze_dominant_mode_reproduction <- function(Wave_full) {
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


# Helper function
attach_attrs <- function(Wave, Gattrs) {
  Wave_full <- Wave
  n_rows <- nrow(Wave)
  
  for (col in names(Gattrs)) {
    Wave_full[[col]] <- rep(Gattrs[[col]][1], n_rows)
  }
  
  return(Wave_full)
}

# Summary function
summarize_wavelet_answers <- function(results) {
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

# =============================================================================
# USAGE:
# results <- ensemble_wavelet_analysis(obs_ts, ensC, ensP, Gattrs)
# summarize_wavelet_answers(results)
# =============================================================================