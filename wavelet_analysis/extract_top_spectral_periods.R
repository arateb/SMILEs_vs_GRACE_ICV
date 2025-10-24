# ==============================================================================
# Function: extract_top_spectral_periods
# ==============================================================================
# Original name: get_top_periods_stl
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Core wavelet function - handles numeric vectors only
#
# ==============================================================================

extract_top_spectral_periods <- function(ts, dt = 1/12, top_k = 4) {
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
