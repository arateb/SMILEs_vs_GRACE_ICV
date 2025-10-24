# ==============================================================================
# Function: calculate_row_metrics
# ==============================================================================
# Original name: row_metric
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# -----------------------------------------------------------------------------
# 2. CORE FUNCTION: Compare single observation to ensemble distribution
# -----------------------------------------------------------------------------
#
# ==============================================================================

calculate_row_metrics <- function(basin, src, metric, obs, ens_vec) {
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
