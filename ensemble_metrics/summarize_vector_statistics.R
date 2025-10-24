# ==============================================================================
# Function: summarize_vector_statistics
# ==============================================================================
# Original name: summarise_vec3
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
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
#
# ==============================================================================

summarize_vector_statistics <- function(v) {
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
