# ==============================================================================
# Function: calculate_ensemble_spread_metrics
# ==============================================================================
# Original name: basic_spread_metrics
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# -----------------------------------------------------------------------------
# 3. MAIN FUNCTION: Compute all variability metrics
# -----------------------------------------------------------------------------
#
# ==============================================================================

calculate_ensemble_spread_metrics <- function(obs_b, ensC, Gattrs, src) {
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
