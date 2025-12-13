# ============================================================================
# WINDOWING FUNCTIONS - NOVEMBER 2025 (REVISED)
# ============================================================================
# Functions for generating contiguous windows and computing dispersion metrics
# Author: Ashraf Rateb
# Date: 2025-11-18 (updated)
# ============================================================================

#' Compute dispersion metrics directly without storing all windows
#'
#' @param x 3D array [basins × members × time]
#' @param dates Date vector of length = dim(x)[3]
#' @param window_length Integer, length of window in time steps
#' @param date_start Start date for valid windows (e.g., "1900-01-01")
#' @param date_end End date for valid windows (e.g., "2100-12-31")
#' @param grace_metrics data.table with GRACE A and sigma per basin
#' @param attrs Basin attributes data.frame/data.table (optional)
#' @param model_name Character, e.g. "CESM2" or "IPSL"
#' @return data.table with basin-level summary (no individual windows stored)
#'
compute_dispersion_summary_direct <- function(x, dates, window_length, date_start, date_end,
                                              grace_metrics, attrs = NULL, model_name) {
  require(data.table)

  n_basins  <- dim(x)[1L]
  n_members <- dim(x)[2L]
  n_time    <- dim(x)[3L]

  # Find valid date range
  valid_idx <- which(dates >= as.Date(date_start) & dates <= as.Date(date_end))

  if (length(valid_idx) < window_length) {
    stop("Not enough time points in valid date range for specified window length.")
  }

  # Generate all possible window start positions
  max_start <- length(valid_idx) - window_length + 1L

  cat("    Processing", n_basins, "basins,", n_members, "members,", max_start, "windows each\n")
  cat("    Total windows:", n_basins * n_members * max_start, "\n")

  # Initialize results list
  results_list <- vector("list", n_basins)

  # Process each basin
  for (basin in seq_len(n_basins)) {
    if (basin %% 20L == 0L) cat("      Basin", basin, "/", n_basins, "\n")

    # Pre-allocate storage for this basin's windows
    n_win_max <- n_members * max_start
    A_vals    <- numeric(n_win_max)
    sigma_vals <- numeric(n_win_max)
    k <- 0L

    # Process each member
    for (member in seq_len(n_members)) {
      # Process each window
      for (w in seq_len(max_start)) {
        start_idx <- valid_idx[w]
        end_idx   <- valid_idx[w + window_length - 1L]

        # Extract window data
        window_data <- x[basin, member, start_idx:end_idx]

        # Skip if all NA
        if (all(is.na(window_data))) next

        # Store metrics
        k <- k + 1L
        A_vals[k]    <- diff(range(window_data, na.rm = TRUE))
        sigma_vals[k] <- sd(window_data, na.rm = TRUE)
      }
    }

    # Trim to actual size and compute summary statistics for this basin
    if (k > 0L) {
      A_vals    <- A_vals[seq_len(k)]
      sigma_vals <- sigma_vals[seq_len(k)]
      A_grace_basin <- grace_metrics$A_grace[basin]

      # Compute percentile rank of GRACE within model distribution
      A_percentile <- mean(A_vals < A_grace_basin, na.rm = TRUE)
      sigma_percentile <- mean(sigma_vals < grace_metrics$sigma_grace[basin], na.rm = TRUE)

      results_list[[basin]] <- data.table(
        basin       = basin,
        basin_id    = grace_metrics$basin_id[basin],
        basin_name  = grace_metrics$basin_name[basin],
        A_grace     = A_grace_basin,
        sigma_grace = grace_metrics$sigma_grace[basin],
        n_obs       = grace_metrics$n_obs[basin],
        A_p05       = quantile(A_vals,    0.05, na.rm = TRUE),
        A_p50       = quantile(A_vals,    0.50, na.rm = TRUE),
        A_p95       = quantile(A_vals,    0.95, na.rm = TRUE),
        sigma_p05   = quantile(sigma_vals, 0.05, na.rm = TRUE),
        sigma_p50   = quantile(sigma_vals, 0.50, na.rm = TRUE),
        sigma_p95   = quantile(sigma_vals, 0.95, na.rm = TRUE),
        n_windows   = length(A_vals),
        # n_total kept for backward compatibility (identical to n_windows)
        n_exceed    = sum(A_vals >= A_grace_basin, na.rm = TRUE),
        n_total     = length(A_vals),
        A_percentile = A_percentile,
        sigma_percentile = sigma_percentile
      )
    } else {
      results_list[[basin]] <- NULL
    }
  }

  # Combine results (skip NULL basins)
  results_list <- results_list[!sapply(results_list, is.null)]
  if (length(results_list) == 0L) {
    warning("No basins with valid windows in compute_dispersion_summary_direct().")
    return(data.table())
  }

  summary_dt <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

  # Compute return period: T_A
  # If A_grace is defined and n_exceed == 0 → T_A = Inf (event not seen in ensemble)
  # If A_grace is missing → T_A = NA (undefined)
  summary_dt[, T_A := ifelse(n_exceed > 0L, n_total / n_exceed, Inf)]
  summary_dt[is.na(A_grace), `:=`(
    n_exceed = NA_integer_,
    T_A      = NA_real_
  )]

  # Coverage flags (GRACE value inside [5%, 95%] envelope)
  summary_dt[, amplitude_covered :=
               (A_grace >= A_p05) & (A_grace <= A_p95)]
  summary_dt[, variance_covered :=
               (sigma_grace >= sigma_p05) & (sigma_grace <= sigma_p95)]

  # Add model name
  summary_dt[, model := model_name]

  # Attach basin attributes if provided (assumes consistent ordering of attrs)
  if (!is.null(attrs)) {
    attrs_dt <- data.table(attrs)
    attrs_dt[, basin := seq_len(.N)]
    summary_dt <- merge(
      summary_dt, attrs_dt,
      by = "basin", all.x = TRUE, suffixes = c("", "_attr")
    )
  }

  summary_dt[]
}

#' Compute amplitude and standard deviation for all windows
#'
#' @param x 3D array [basins × members × time]
#' @param window_meta data.table from make_model_windows()
#'        with columns basin, member, t_start_idx, t_end_idx
#' @return data.table with amplitude and sigma added
#'
compute_dispersion_metrics <- function(x, window_meta) {
  require(data.table)

  # Add columns for metrics (NA initialisation)
  window_meta[, `:=`(
    amplitude = NA_real_,
    sigma     = NA_real_
  )]

  # Compute for each window
  for (i in seq_len(nrow(window_meta))) {
    b       <- window_meta$basin[i]
    m       <- window_meta$member[i]
    t_start <- window_meta$t_start_idx[i]
    t_end   <- window_meta$t_end_idx[i]

    # Extract window
    window_data <- x[b, m, t_start:t_end]

    # Skip if all NA
    if (all(is.na(window_data))) next

    # Compute metrics
    window_meta$amplitude[i] <- diff(range(window_data, na.rm = TRUE))
    window_meta$sigma[i]     <- sd(window_data, na.rm = TRUE)
  }

  window_meta[]
}

#' Compute GRACE metrics for reference period
#'
#' @param x_grace 2D array [time × basins]
#' @param dates_grace Date vector (length = nrow(x_grace))
#' @param attrs Basin attributes dataframe
#' @return data.table with basin-level GRACE metrics
#'
compute_grace_metrics <- function(x_grace, dates_grace, attrs) {
  require(data.table)

  n_basins <- ncol(x_grace)

  grace_metrics <- data.table(
    basin       = seq_len(n_basins),
    basin_id    = attrs$ID,
    basin_name  = attrs$name,
    A_grace     = NA_real_,
    sigma_grace = NA_real_,
    n_obs       = NA_integer_
  )

  for (b in seq_len(n_basins)) {
    ts <- x_grace[, b]

    # Skip if all NA
    if (all(is.na(ts))) next

    grace_metrics$A_grace[b]     <- diff(range(ts, na.rm = TRUE))
    grace_metrics$sigma_grace[b] <- sd(ts, na.rm = TRUE)
    grace_metrics$n_obs[b]       <- sum(!is.na(ts))
  }

  grace_metrics[]
}

#' Summarize dispersion by basin using precomputed window metrics
#'
#' @param dispersion_windows data.table from compute_dispersion_metrics()
#' @param grace_metrics data.table from compute_grace_metrics()
#' @param model_name Character string ("CESM2" or "IPSL")
#' @param attrs Basin attributes dataframe (optional; will be attached to summary)
#' @return data.table with basin-level summary statistics
#'
summarize_dispersion_by_basin <- function(dispersion_windows, grace_metrics,
                                          model_name, attrs = NULL) {
  require(data.table)

  # Compute percentiles for each basin (using all members × windows)
  summary_dt <- dispersion_windows[!is.na(amplitude), .(
    A_p05     = quantile(amplitude, 0.05, na.rm = TRUE),
    A_p50     = quantile(amplitude, 0.50, na.rm = TRUE),
    A_p95     = quantile(amplitude, 0.95, na.rm = TRUE),
    sigma_p05 = quantile(sigma,     0.05, na.rm = TRUE),
    sigma_p50 = quantile(sigma,     0.50, na.rm = TRUE),
    sigma_p95 = quantile(sigma,     0.95, na.rm = TRUE),
    n_windows = .N
  ), by = basin]

  # Merge with GRACE metrics
  summary_dt <- merge(summary_dt, grace_metrics, by = "basin", all.x = TRUE)

  # Compute return periods using model amplitude vs basin-specific A_grace
  disp_with_grace <- merge(
    dispersion_windows[!is.na(amplitude), .(basin, amplitude, sigma)],
    grace_metrics[, .(basin, A_grace)],
    by = "basin",
    all.x = TRUE
  )

  return_period_dt <- disp_with_grace[, .(
    n_exceed = sum(amplitude >= A_grace, na.rm = TRUE),
    n_total  = .N
  ), by = basin]

  summary_dt <- merge(summary_dt, return_period_dt, by = "basin", all.x = TRUE)

  # T_A logic as in direct function
  summary_dt[, T_A := ifelse(n_exceed > 0L, n_total / n_exceed, Inf)]
  summary_dt[is.na(A_grace), `:=`(
    n_exceed = NA_integer_,
    T_A      = NA_real_
  )]

  # Coverage flags
  summary_dt[, amplitude_covered :=
               (A_grace >= A_p05) & (A_grace <= A_p95)]
  summary_dt[, variance_covered :=
               (sigma_grace >= sigma_p05) & (sigma_grace <= sigma_p95)]

  # Add model name
  summary_dt[, model := model_name]

  # Attach basin attributes if provided
  if (!is.null(attrs)) {
    attrs_dt <- data.table(attrs)
    attrs_dt[, basin := seq_len(.N)]
    summary_dt <- merge(
      summary_dt, attrs_dt,
      by = "basin", all.x = TRUE, suffixes = c("", "_attr")
    )
  }

  summary_dt[]
}

#' Classify basins based on envelope coverage (CESM2 vs IPSL)
#'
#' @param summary_cesm data.table from summarize_dispersion_by_basin() for CESM2
#' @param summary_ipsl data.table from summarize_dispersion_by_basin() for IPSL
#' @return data.table with basin classification
#'
classify_basin_coverage <- function(summary_cesm, summary_ipsl) {
  require(data.table)

  # Extract coverage flags from each model
  cesm_cov <- summary_cesm[, .(
    basin, basin_id, basin_name,
    cesm_amp_covered = amplitude_covered,
    cesm_var_covered = variance_covered
  )]

  ipsl_cov <- summary_ipsl[, .(
    basin,
    ipsl_amp_covered = amplitude_covered,
    ipsl_var_covered = variance_covered
  )]

  # Merge by basin to avoid positional mismatches
  coverage_dt <- merge(
    cesm_cov, ipsl_cov,
    by = "basin", all = TRUE
  )

  # Classify based on amplitude coverage
  coverage_dt[, basin_class_amplitude := fcase(
    cesm_amp_covered & ipsl_amp_covered,                      "both",
    cesm_amp_covered & !ipsl_amp_covered,                     "cesm_only",
    !cesm_amp_covered & ipsl_amp_covered,                     "ipsl_only",
    !cesm_amp_covered & !ipsl_amp_covered,                    "neither",
    default = NA_character_
  )]

  # Classify based on variance coverage
  coverage_dt[, basin_class_variance := fcase(
    cesm_var_covered & ipsl_var_covered,                      "both",
    cesm_var_covered & !ipsl_var_covered,                     "cesm_only",
    !cesm_var_covered & ipsl_var_covered,                     "ipsl_only",
    !cesm_var_covered & !ipsl_var_covered,                    "neither",
    default = NA_character_
  )]

  # Classify based on BOTH amplitude AND variance
  coverage_dt[, basin_class_strict := fcase(
    (cesm_amp_covered & cesm_var_covered) &
      (ipsl_amp_covered & ipsl_var_covered),                  "both",
    (cesm_amp_covered & cesm_var_covered) &
      !(ipsl_amp_covered & ipsl_var_covered),                 "cesm_only",
    !(cesm_amp_covered & cesm_var_covered) &
      (ipsl_amp_covered & ipsl_var_covered),                  "ipsl_only",
    !(cesm_amp_covered & cesm_var_covered) &
      !(ipsl_amp_covered & ipsl_var_covered),                 "neither",
    default = NA_character_
  )]

  coverage_dt[]
}

#' Compute dispersion summary in parallel (one basin per worker)
#'
#' @param x 3D array [basins × members × time]
#' @param dates Date vector of length = dim(x)[3]
#' @param window_length Integer, length of window in time steps
#' @param date_start Start date for valid windows (e.g., "1900-01-01")
#' @param date_end End date for valid windows (e.g., "2100-12-31")
#' @param grace_metrics data.table with GRACE A and sigma per basin
#' @param attrs Basin attributes dataframe (optional)
#' @param model_name Character, "CESM2" or "IPSL"
#' @return data.table with basin-level summary (no individual windows stored)
#'
compute_dispersion_summary_parallel <- function(x, dates, window_length, date_start, date_end,
                                                grace_metrics, attrs = NULL, model_name) {
  require(data.table)
  require(foreach)
  require(doParallel)

  n_basins  <- dim(x)[1L]
  n_members <- dim(x)[2L]
  n_time    <- dim(x)[3L]

  # Find valid date range
  valid_idx <- which(dates >= as.Date(date_start) & dates <= as.Date(date_end))

  if (length(valid_idx) < window_length) {
    stop("Not enough time points in valid date range for specified window length.")
  }

  # Generate all possible window start positions
  max_start <- length(valid_idx) - window_length + 1L

  cat("    Processing", n_basins, "basins,", n_members, "members,",
      max_start, "windows each (PARALLEL)\n")
  cat("    Total windows:", n_basins * n_members * max_start, "\n")

  # Process each basin in parallel
  results <- foreach(basin = seq_len(n_basins), .packages = c("data.table")) %dopar% {

    # Pre-allocate storage for this basin's windows
    n_win_max <- n_members * max_start
    A_vals    <- numeric(n_win_max)
    sigma_vals <- numeric(n_win_max)
    k <- 0L

    # Process each member
    for (member in seq_len(n_members)) {
      # Process each window
      for (w in seq_len(max_start)) {
        start_idx <- valid_idx[w]
        end_idx   <- valid_idx[w + window_length - 1L]

        # Extract window data
        window_data <- x[basin, member, start_idx:end_idx]

        # Skip if all NA
        if (all(is.na(window_data))) next

        # Store metrics
        k <- k + 1L
        A_vals[k]    <- diff(range(window_data, na.rm = TRUE))
        sigma_vals[k] <- sd(window_data, na.rm = TRUE)
      }
    }

    # Trim to actual size and compute summary statistics for this basin
    if (k > 0L) {
      A_vals    <- A_vals[seq_len(k)]
      sigma_vals <- sigma_vals[seq_len(k)]
      A_grace_basin <- grace_metrics$A_grace[basin]

      # Compute percentile rank of GRACE within model distribution
      A_percentile <- mean(A_vals < A_grace_basin, na.rm = TRUE)
      sigma_percentile <- mean(sigma_vals < grace_metrics$sigma_grace[basin], na.rm = TRUE)

      data.table(
        basin       = basin,
        basin_id    = grace_metrics$basin_id[basin],
        basin_name  = grace_metrics$basin_name[basin],
        A_grace     = A_grace_basin,
        sigma_grace = grace_metrics$sigma_grace[basin],
        n_obs       = grace_metrics$n_obs[basin],
        A_p05       = quantile(A_vals,    0.05, na.rm = TRUE),
        A_p50       = quantile(A_vals,    0.50, na.rm = TRUE),
        A_p95       = quantile(A_vals,    0.95, na.rm = TRUE),
        sigma_p05   = quantile(sigma_vals, 0.05, na.rm = TRUE),
        sigma_p50   = quantile(sigma_vals, 0.50, na.rm = TRUE),
        sigma_p95   = quantile(sigma_vals, 0.95, na.rm = TRUE),
        n_windows   = length(A_vals),
        # n_total kept for backward compatibility (identical to n_windows)
        n_exceed    = sum(A_vals >= A_grace_basin, na.rm = TRUE),
        n_total     = length(A_vals),
        A_percentile = A_percentile,
        sigma_percentile = sigma_percentile
      )
    } else {
      NULL
    }
  }

  # Combine results (remove NULLs)
  results <- results[!sapply(results, is.null)]
  if (length(results) == 0L) {
    warning("No basins with valid windows in compute_dispersion_summary_parallel().")
    return(data.table())
  }

  summary_dt <- rbindlist(results, use.names = TRUE, fill = TRUE)

  # Compute return period
  summary_dt[, T_A := ifelse(n_exceed > 0L, n_total / n_exceed, Inf)]
  summary_dt[is.na(A_grace), `:=`(
    n_exceed = NA_integer_,
    T_A      = NA_real_
  )]

  # Coverage flags
  summary_dt[, amplitude_covered :=
               (A_grace >= A_p05) & (A_grace <= A_p95)]
  summary_dt[, variance_covered :=
               (sigma_grace >= sigma_p05) & (sigma_grace <= sigma_p95)]

  # Add model name
  summary_dt[, model := model_name]

  # Attach basin attributes if provided
  if (!is.null(attrs)) {
    attrs_dt <- data.table(attrs)
    attrs_dt[, basin := seq_len(.N)]
    summary_dt <- merge(
      summary_dt, attrs_dt,
      by = "basin", all.x = TRUE, suffixes = c("", "_attr")
    )
  }

  summary_dt[]
}
