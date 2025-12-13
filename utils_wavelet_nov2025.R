#!/usr/bin/env Rscript
# ============================================================================
# WAVELET UTILITIES & MATCHING FUNCTIONS - NOVEMBER 2025
# ============================================================================
# Morlet wavelet analysis and GRACE–model mode matching for TWS time series
# Author: Ashraf Rateb
# Date: 2025-11-18 (consolidated 2025-11-23)
# ============================================================================

suppressPackageStartupMessages({
  library(WaveletComp)
  library(data.table)
})

# ============================================================================
# CORE WAVELET METRICS
# ============================================================================

#' Compute wavelet metrics for a single time series
#'
#' @param series Numeric vector of TWS anomalies
#' @param time Optional Date or numeric vector (same length as series)
#' @param dt Sampling interval in years (e.g. 1/12 for data sampled 12 times per year)
#' @param omega0 Morlet wavelet parameter (kept for documentation; WaveletComp uses 6)
#'
#' @return List with:
#'   - dominant_periods: numeric(3), top three periods (years)
#'   - dominant_powers:  numeric(3), corresponding powers
#'   - band_class:       character(3), band labels for the three peaks
#'   - global_spectrum:  data.frame(period, power)
#'   - success:          logical flag
#'
compute_wavelet_metrics <- function(series,
                                    time   = NULL,
                                    dt     = 1/12,
                                    omega0 = 6) {

  # Handle missing data
  valid_idx <- !is.na(series)
  if (sum(valid_idx) < 12L) {
    # Not enough data for meaningful wavelet analysis
    return(list(
      dominant_periods = rep(NA_real_, 3),
      dominant_powers  = rep(NA_real_, 3),
      band_class       = rep(NA_character_, 3),
      global_spectrum  = NULL,
      success          = FALSE
    ))
  }

  # Time axis in YEARS
  if (is.null(time)) {
    # Regular sampling with spacing dt (years)
    t_years <- seq_along(series) * dt
  } else if (inherits(time, "Date")) {
    # Convert calendar dates to years from start
    t_years <- as.numeric(difftime(time, min(time, na.rm = TRUE), units = "days")) / 365.25
  } else {
    # Assume user provided time already in years (or consistent units)
    t_years <- as.numeric(time)
  }

  # Restrict to valid entries
  series_valid <- series[valid_idx]
  t_valid      <- t_years[valid_idx]

  if (length(series_valid) < 12L || length(unique(t_valid)) < 2L) {
    return(list(
      dominant_periods = rep(NA_real_, 3),
      dominant_powers  = rep(NA_real_, 3),
      band_class       = rep(NA_character_, 3),
      global_spectrum  = NULL,
      success          = FALSE
    ))
  }

  # Effective sampling interval from time axis (in years)
  dt_eff <- if (length(t_valid) > 1L) median(diff(t_valid)) else dt

  # Continuous wavelet transform using Morlet (WaveletComp default, omega0 ≈ 6)
  out <- tryCatch({

    wt <- analyze.wavelet(
      my.data    = data.frame(time = t_valid, series = series_valid),
      my.series  = "series",
      loess.span = 0,          # no detrending (already filtered upstream)
      dt         = dt_eff,     # years
      dj         = 1/20,
      lowerPeriod = 2,         # 2 years minimum
      upperPeriod = 80,        # 80 years maximum (effectively "long")
      make.pval  = FALSE,
      n.sim      = 100,
      verbose    = FALSE
    )

    global_ws <- wt$Power.avg           # time‑averaged power
    periods   <- wt$Period              # periods in YEARS

    # Peak detection (local maxima); fall back to top powers if no local maxima
    peak_idx <- which(diff(sign(diff(global_ws))) == -2) + 1

    if (length(peak_idx) == 0L) {
      peak_idx <- order(global_ws, decreasing = TRUE)[1:min(3L, length(global_ws))]
    }

    # Sort peaks by descending power
    peak_idx <- peak_idx[order(global_ws[peak_idx], decreasing = TRUE)]
    peak_idx <- peak_idx[1:min(3L, length(peak_idx))]

    dominant_periods <- periods[peak_idx]
    dominant_powers  <- global_ws[peak_idx]

    # Pad to length 3
    if (length(dominant_periods) < 3L) {
      dominant_periods <- c(dominant_periods, rep(NA_real_, 3L - length(dominant_periods)))
      dominant_powers  <- c(dominant_powers,  rep(NA_real_, 3L - length(dominant_powers)))
    }

    # Band classification (periods in YEARS)
    band_class <- classify_period_band(dominant_periods)

    list(
      dominant_periods = dominant_periods,
      dominant_powers  = dominant_powers,
      band_class       = band_class,
      global_spectrum  = data.frame(period = periods, power = global_ws),
      success          = TRUE
    )

  }, error = function(e) {
    list(
      dominant_periods = rep(NA_real_, 3),
      dominant_powers  = rep(NA_real_, 3),
      band_class       = rep(NA_character_, 3),
      global_spectrum  = NULL,
      success          = FALSE,
      error_msg        = e$message
    )
  })

  out
}

#' Classify periods into ENSO / decadal bands (periods in YEARS)
#'
#' @param periods Numeric vector of periods in years
#' @return Character vector of band classifications
#'
#' Bands:
#'   Sub_ENSO       :  p < 2
#'   ENSO_core      :  2 ≤ p < 4
#'   Quasi-decadal  :  4 ≤ p < 8
#'   Decadal        :  8 ≤ p ≤ 30
#'   Multidecadal   :  p > 30
#'
classify_period_band <- function(periods) {
  sapply(periods, function(p) {
    if (is.na(p)) {
      NA_character_
    } else if (p < 2) {
      "Sub_ENSO"
    } else if (p >= 2 && p < 4) {
      "ENSO_core"
    } else if (p >= 4 && p < 8) {
      "Quasi-decadal"
    } else if (p >= 8 && p <= 30) {
      "Decadal"
    } else {  # p > 30
      "Multidecadal"
    }
  })
}

#' Compute percentile rank of value within a distribution
#'
#' @param value Scalar numeric value
#' @param distribution Numeric vector
#' @return Percentile rank (0–100) or NA
#'
compute_percentile_rank <- function(value, distribution) {
  if (is.na(value) || length(distribution) == 0L) {
    return(NA_real_)
  }
  valid_dist <- distribution[!is.na(distribution)]
  if (length(valid_dist) == 0L) {
    return(NA_real_)
  }
  100 * sum(valid_dist <= value) / length(valid_dist)
}

#' Process wavelet metrics for all windows in a basin
#'
#' @param basin_data Matrix [members × time]
#' @param dates Date or numeric vector (length = ncol(basin_data))
#' @param window_length Window length in time steps
#' @param valid_indices Integer indices of valid time steps
#' @param grace_metrics List with GRACE wavelet metrics (from compute_wavelet_metrics)
#'
#' @return List with ensemble distributions and percentile ranks
#'
process_basin_wavelet_windows <- function(basin_data,
                                          dates,
                                          window_length,
                                          valid_indices,
                                          grace_metrics) {

  n_members <- nrow(basin_data)
  max_start <- length(valid_indices) - window_length + 1L

  dominant_periods_all <- list()
  dominant_powers_all  <- list()
  band_class_all       <- list()
  P_LF_all             <- numeric(0)

  for (member in 1:n_members) {
    for (w in 1:max_start) {
      start_idx <- valid_indices[w]
      end_idx   <- valid_indices[w + window_length - 1L]

      window_data  <- basin_data[member, start_idx:end_idx]
      window_dates <- dates[start_idx:end_idx]

      if (all(is.na(window_data))) next

      wm <- compute_wavelet_metrics(window_data, window_dates)

      if (wm$success) {
        dominant_periods_all[[length(dominant_periods_all) + 1L]] <- wm$dominant_periods[1]
        dominant_powers_all[[length(dominant_powers_all) + 1L]]  <- wm$dominant_powers[1]
        band_class_all[[length(band_class_all) + 1L]]            <- wm$band_class[1]
        P_LF_all <- c(P_LF_all, wm$P_LF)
      }
    }
  }

  dominant_periods_vec <- unlist(dominant_periods_all)
  dominant_powers_vec  <- unlist(dominant_powers_all)
  band_class_vec       <- unlist(band_class_all)

  if (length(P_LF_all) > 0L) {
    P_LF_p05 <- quantile(P_LF_all, 0.05, na.rm = TRUE)
    P_LF_p50 <- quantile(P_LF_all, 0.50, na.rm = TRUE)
    P_LF_p95 <- quantile(P_LF_all, 0.95, na.rm = TRUE)
  } else {
    P_LF_p05 <- P_LF_p50 <- P_LF_p95 <- NA_real_
  }

  grace_period   <- grace_metrics$dominant_periods[1]
  grace_P_LF     <- grace_metrics$P_LF
  period_percentile <- compute_percentile_rank(grace_period, dominant_periods_vec)
  P_LF_percentile   <- compute_percentile_rank(grace_P_LF, P_LF_all)

  list(
    P_LF_p05           = P_LF_p05,
    P_LF_p50           = P_LF_p50,
    P_LF_p95           = P_LF_p95,
    P_LF_percentile    = P_LF_percentile,
    period_percentile  = period_percentile,
    dominant_periods_dist = dominant_periods_vec,
    dominant_powers_dist  = dominant_powers_vec,
    band_class_dist       = band_class_vec,
    P_LF_dist             = P_LF_all
  )
}

# ============================================================================
# BAND-BASED GRACE–MEMBER MATCHING
# ============================================================================

#' Compare GRACE and member wavelet modes (band-based matching)
#'
#' @param grace_periods Numeric(3), GRACE dominant periods (years)
#' @param grace_powers  Numeric(3), GRACE dominant powers
#' @param grace_bands   Character(3), GRACE band classes
#' @param member_periods Numeric(3), member dominant periods (years)
#' @param member_powers  Numeric(3), member dominant powers
#' @param member_bands   Character(3), member band classes
#'
#' @return List with:
#'   - grace_matches:   list of length 3 (one per GRACE peak)
#'   - member_unmatched: list of member modes not matched to any GRACE mode
#'
compare_wavelet_modes <- function(grace_periods, grace_powers, grace_bands,
                                  member_periods, member_powers, member_bands) {

  matches <- vector("list", 3L)

  for (i in 1:3) {
    grace_p   <- grace_periods[i]
    grace_pow <- grace_powers[i]
    grace_band <- grace_bands[i]

    if (is.na(grace_p) || is.na(grace_band)) {
      matches[[i]] <- list(
        grace_rank   = i,
        grace_period = NA_real_,
        grace_power  = NA_real_,
        grace_band   = NA_character_,
        matched      = FALSE,
        member_rank  = NA_integer_,
        member_period = NA_real_,
        member_power  = NA_real_,
        member_band   = NA_character_,
        period_diff   = NA_real_,
        power_ratio   = NA_real_,
        band_match    = TRUE  # neutral for missing GRACE band
      )
      next
    }

    # Members with the same band as GRACE mode i
    band_matches <- which(!is.na(member_bands) & member_bands == grace_band)

    if (length(band_matches) > 0L) {
      # Take the highest‑ranked member mode (lowest index)
      closest_idx <- band_matches[1]
      member_p    <- member_periods[closest_idx]
      member_pow  <- member_powers[closest_idx]
      member_band <- member_bands[closest_idx]

      matches[[i]] <- list(
        grace_rank   = i,
        grace_period = grace_p,
        grace_power  = grace_pow,
        grace_band   = grace_band,
        matched      = TRUE,
        member_rank  = closest_idx,
        member_period = member_p,
        member_power  = member_pow,
        member_band   = member_band,
        period_diff   = abs(member_p - grace_p),
        power_ratio   = member_pow / grace_pow,
        band_match    = TRUE
      )
    } else {
      matches[[i]] <- list(
        grace_rank   = i,
        grace_period = grace_p,
        grace_power  = grace_pow,
        grace_band   = grace_band,
        matched      = FALSE,
        member_rank  = NA_integer_,
        member_period = NA_real_,
        member_power  = NA_real_,
        member_band   = NA_character_,
        period_diff   = NA_real_,
        power_ratio   = NA_real_,
        band_match    = FALSE
      )
    }
  }

  # Member modes not matched to any GRACE mode
  member_unmatched <- list()
  for (j in 1:3) {
    member_p   <- member_periods[j]
    member_pow <- member_powers[j]
    member_band <- member_bands[j]

    if (is.na(member_p)) next

    is_matched <- any(vapply(matches, function(m) {
      isTRUE(m$matched) && !is.na(m$member_rank) && m$member_rank == j
    }, logical(1)))

    if (!is_matched) {
      member_unmatched[[length(member_unmatched) + 1L]] <- list(
        member_rank  = j,
        member_period = member_p,
        member_power  = member_pow,
        member_band   = member_band
      )
    }
  }

  list(
    grace_matches   = matches,
    member_unmatched = member_unmatched
  )
}

#' Process GRACE–member matching for all members in a basin
#'
#' @param basin_data Matrix [members × time]
#' @param dates Date or numeric vector
#' @param grace_metrics List from compute_wavelet_metrics for this basin
#' @param dt Sampling interval in years (default 1/12)
#' @param omega0 Morlet parameter (unused, kept for consistency)
#'
#' @return data.table with one row per (member × GRACE mode) combination
#'
process_basin_member_matching <- function(basin_data,
                                          dates,
                                          grace_metrics,
                                          dt     = 1/12,
                                          omega0 = 6) {

  n_members    <- nrow(basin_data)
  results_list <- list()

  grace_periods <- grace_metrics$dominant_periods
  grace_powers  <- grace_metrics$dominant_powers
  grace_bands   <- grace_metrics$band_class

  for (member in 1:n_members) {
    member_series <- basin_data[member, ]

    member_wm <- compute_wavelet_metrics(member_series, dates, dt = dt, omega0 = omega0)

    if (!member_wm$success) {
      for (i in 1:3) {
        results_list[[length(results_list) + 1L]] <- data.table(
          member        = member,
          grace_rank    = i,
          grace_period  = grace_periods[i],
          grace_power   = grace_powers[i],
          grace_band    = grace_bands[i],
          matched       = FALSE,
          member_rank   = NA_integer_,
          member_period = NA_real_,
          member_power  = NA_real_,
          member_band   = NA_character_,
          period_diff   = NA_real_,
          power_ratio   = NA_real_,
          band_match    = FALSE,
          member_success = FALSE
        )
      }
      next
    }

    comparison <- compare_wavelet_modes(
      grace_periods, grace_powers, grace_bands,
      member_wm$dominant_periods, member_wm$dominant_powers, member_wm$band_class
    )

    for (match in comparison$grace_matches) {
      results_list[[length(results_list) + 1L]] <- data.table(
        member        = member,
        grace_rank    = match$grace_rank,
        grace_period  = match$grace_period,
        grace_power   = match$grace_power,
        grace_band    = match$grace_band,
        matched       = match$matched,
        member_rank   = match$member_rank,
        member_period = match$member_period,
        member_power  = match$member_power,
        member_band   = match$member_band,
        period_diff   = match$period_diff,
        power_ratio   = match$power_ratio,
        band_match    = match$band_match,
        member_success = TRUE
      )
    }
  }

  rbindlist(results_list)
}

#' Summarize band-specific oscillation characteristics across members
#'
#' @param member_results data.table from process_basin_member_matching()
#' @return data.table with band-level summaries
#'
summarize_band_oscillations <- function(member_results) {

  matched <- member_results[matched == TRUE]

  if (nrow(matched) == 0L) {
    return(data.table(
      band               = character(0),
      n_members_with_band = integer(0),
      mean_period        = numeric(0),
      sd_period          = numeric(0),
      mean_power         = numeric(0),
      sd_power           = numeric(0),
      mean_power_ratio   = numeric(0),
      band_match_rate    = numeric(0)
    ))
  }

  matched[, .(
    n_members_with_band = .N,
    mean_period         = mean(member_period, na.rm = TRUE),
    sd_period           = sd(member_period, na.rm = TRUE),
    mean_power          = mean(member_power, na.rm = TRUE),
    sd_power            = sd(member_power, na.rm = TRUE),
    mean_power_ratio    = mean(power_ratio, na.rm = TRUE),
    band_match_rate     = mean(band_match, na.rm = TRUE)
  ), by = .(band = grace_band)]
}

# ============================================================================
# MEMBER WAVELETS & MODE COUNTS
# ============================================================================

#' Extract all member wavelet metrics (dominant periods/powers)
#'
#' @param basin_data Matrix [members × time]
#' @param dates Date or numeric vector
#' @param dt Sampling interval in years (default 1/12)
#' @param omega0 Morlet parameter (unused)
#'
#' @return data.table with one row per member
#'
extract_all_member_wavelets <- function(basin_data,
                                        dates,
                                        dt     = 1/12,
                                        omega0 = 6) {

  n_members   <- nrow(basin_data)
  all_results <- vector("list", n_members)

  for (member in 1:n_members) {
    member_series <- basin_data[member, ]
    wm <- compute_wavelet_metrics(member_series, dates, dt = dt, omega0 = omega0)

    if (wm$success) {
      all_results[[member]] <- data.table(
        member   = member,
        period_1 = wm$dominant_periods[1],
        period_2 = wm$dominant_periods[2],
        period_3 = wm$dominant_periods[3],
        power_1  = wm$dominant_powers[1],
        power_2  = wm$dominant_powers[2],
        power_3  = wm$dominant_powers[3],
        band_1   = wm$band_class[1],
        band_2   = wm$band_class[2],
        band_3   = wm$band_class[3],
        P_LF     = wm$P_LF,
        success  = TRUE
      )
    } else {
      all_results[[member]] <- data.table(
        member   = member,
        period_1 = NA_real_,
        period_2 = NA_real_,
        period_3 = NA_real_,
        power_1  = NA_real_,
        power_2  = NA_real_,
        power_3  = NA_real_,
        band_1   = NA_character_,
        band_2   = NA_character_,
        band_3   = NA_character_,
        P_LF     = NA_real_,
        success  = FALSE
      )
    }
  }

  rbindlist(all_results)
}

#' Count band-class matches between GRACE and a model window (simple)
#'
#' @param grace_bands Character vector (e.g. c("ENSO_core","Quasi-decadal","Decadal"))
#' @param model_bands Character vector of same form
#' @return Integer count of matching bands (0–3) or NA if one side has no valid bands
#'
count_band_matches <- function(grace_bands, model_bands) {

  grace_clean <- grace_bands[!is.na(grace_bands)]
  model_clean <- model_bands[!is.na(model_bands)]

  if (length(grace_clean) == 0L || length(model_clean) == 0L) {
    return(NA_integer_)
  }

  sum(model_clean %in% grace_clean)
}

#' Count unique oscillation modes across members (by band)
#'
#' @param basin_data Matrix [members × time]
#' @param dates Date or numeric vector
#' @param dt Sampling interval in years (default 1/12)
#' @param omega0 Morlet parameter (unused)
#'
#' @return data.table with counts and basic statistics per band
#'
count_member_oscillations <- function(basin_data,
                                      dates,
                                      dt     = 1/12,
                                      omega0 = 6) {

  n_members <- nrow(basin_data)
  all_modes <- list()

  for (member in 1:n_members) {
    member_series <- basin_data[member, ]
    wm <- compute_wavelet_metrics(member_series, dates, dt = dt, omega0 = omega0)

    if (wm$success) {
      for (i in 1:3) {
        if (!is.na(wm$dominant_periods[i])) {
          all_modes[[length(all_modes) + 1L]] <- data.table(
            member   = member,
            mode_rank = i,
            period   = wm$dominant_periods[i],
            power    = wm$dominant_powers[i],
            band     = wm$band_class[i]
          )
        }
      }
    }
  }

  if (length(all_modes) == 0L) {
    return(data.table(
      band       = character(0),
      n_modes    = integer(0),
      n_members  = integer(0),
      mean_period = numeric(0),
      sd_period   = numeric(0),
      mean_power  = numeric(0),
      sd_power    = numeric(0)
    ))
  }

  all_modes_dt <- rbindlist(all_modes)

  all_modes_dt[, .(
    n_modes     = .N,
    n_members   = uniqueN(member),
    mean_period = mean(period, na.rm = TRUE),
    sd_period   = sd(period,   na.rm = TRUE),
    mean_power  = mean(power,  na.rm = TRUE),
    sd_power    = sd(power,    na.rm = TRUE)
  ), by = band]
}
