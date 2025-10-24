# ==============================================================================
# Function: perform_ensemble_wavelet_analysis
# ==============================================================================
# Original name: ensemble_wavelet_analysis
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# Main analysis function
#
# ==============================================================================

perform_ensemble_wavelet_analysis <- function(obs_ts, ensC, ensP, Gattrs, top_k = 4) {
  
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
