# ==============================================================================
# Function: interpret_model_performance
# ==============================================================================
# Original name: create_interpretation
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# -----------------------------------------------------------------------------
# 4. INTERPRETATION FUNCTION
# -----------------------------------------------------------------------------
#
# ==============================================================================

interpret_model_performance <- function(spread_table, consistency) {
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
