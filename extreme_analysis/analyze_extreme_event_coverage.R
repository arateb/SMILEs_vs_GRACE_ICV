# ==============================================================================
# Function: analyze_extreme_event_coverage
# ==============================================================================
# Original name: extreme_coverage_analysis
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
############   Extremes  ## ####################################################
#  Extremes Extremes Extremes Extremes Extremes Extremes Extremes Extremes
#  Extremes Extremes Extremes Extremes Extremes Extremes Extremes Extremes
# Extreme coverage analysis - consistent with amplitude/variance approach
#
# ==============================================================================

analyze_extreme_event_coverage <- function(obs, ens_full, Gattrs, src) {
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
