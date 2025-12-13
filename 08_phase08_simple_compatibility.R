#!/usr/bin/env Rscript
# ============================================================================
# PHASE 08 - SIMPLE 4-METRIC COMPATIBILITY INDEX
# ============================================================================
# Uses 4 key metrics with per-member variability:
#   1. A (amplitude from dispersion) - max-min TWS range
#   2. sigma (variance from dispersion) - standard deviation of TWS
#   3. H_max (maximum pluvial height from events)
#   4. D_max (maximum drought depth from events)
#
# Author: Ashraf Rateb
# Date: 2025-12-02
# ============================================================================

library(data.table)

cat("============================================================================\n")
cat("PHASE 08: SIMPLE 4-METRIC COMPATIBILITY INDEX\n")
cat("============================================================================\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================

RIDGE_LAMBDA <- 1e-6
METRIC_NAMES <- c("A", "sigma", "H_max", "D_max")

cat("Configuration:\n")
cat("  Ridge regularization (lambda):", RIDGE_LAMBDA, "\n")
cat("  Metrics:", paste(METRIC_NAMES, collapse = ", "), "\n\n")

# ============================================================================
# HELPER FUNCTION
# ============================================================================

compute_compatibility <- function(M_grace, M_model, ridge_lambda = 1e-6) {

  complete_rows <- complete.cases(M_model)
  M_model_clean <- M_model[complete_rows, , drop = FALSE]
  n_metrics <- ncol(M_model_clean)

  if (nrow(M_model_clean) < n_metrics + 2) {
    return(list(n_members = nrow(M_model), n_complete = nrow(M_model_clean),
                d_mahal = NA_real_, C_b = NA_real_, compat_class = NA_character_))
  }

  mu_model <- colMeans(M_model_clean, na.rm = TRUE)
  Sigma_model <- cov(M_model_clean, use = "complete.obs")
  Sigma_model_reg <- Sigma_model + diag(ridge_lambda, nrow = ncol(Sigma_model))

  if (any(is.na(M_grace))) {
    return(list(n_members = nrow(M_model), n_complete = nrow(M_model_clean),
                d_mahal = NA_real_, C_b = NA_real_, compat_class = NA_character_))
  }

  d_grace <- tryCatch({
    mahalanobis(M_grace, center = mu_model, cov = Sigma_model_reg)
  }, error = function(e) NA_real_)

  d_model <- tryCatch({
    mahalanobis(M_model_clean, center = mu_model, cov = Sigma_model_reg)
  }, error = function(e) rep(NA_real_, nrow(M_model_clean)))

  C_b <- if (!is.na(d_grace) && !any(is.na(d_model))) {
    mean(d_model <= d_grace, na.rm = TRUE)
  } else NA_real_

  compat_class <- if (is.na(C_b)) NA_character_ else {
    if (abs(C_b - 0.5) > 0.45) "incompatible"
    else if (abs(C_b - 0.5) > 0.40) "marginal"
    else "compatible"
  }

  list(n_members = nrow(M_model), n_complete = nrow(M_model_clean),
       d_mahal = sqrt(d_grace), C_b = C_b, compat_class = compat_class)
}

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading data...\n")

# Dispersion per-member data (A, sigma)
cat("  Loading per-member dispersion data...\n")
cesm_disp <- readRDS("outputs/phase08_cesm_dispersion_member.rds")
ipsl_disp <- readRDS("outputs/phase08_ipsl_dispersion_member.rds")

# Events: H_max, D_max per basin × member
cat("  Loading events data...\n")
events_models <- readRDS("outputs/phase06_events_models.rds")

# Aggregate events to basin × model × member
cat("  Computing H_max, D_max per member...\n")
events_member <- events_models[, .(
  H_max = max(pluvial_height, na.rm = TRUE),
  D_max = min(drought_depth, na.rm = TRUE)
), by = .(basin_id, model, member)]
events_member[is.infinite(H_max), H_max := NA]
events_member[is.infinite(D_max), D_max := NA]
events_member[, D_max := abs(D_max)]  # Make positive for comparability

rm(events_models)
gc()

# GRACE metrics from dispersion summary
disp_sum <- fread("outputs/dispersion_summary.csv")
ev_sum <- fread("outputs/phase06_event_summary.csv")

grace_metrics <- merge(
  disp_sum[, .(basin_id, basin_name = basin, bd_id, A_grace, sigma_grace)],
  ev_sum[, .(basin_id, H_max_grace, D_max_grace = abs(D_max_grace))],
  by = "basin_id"
)

cat("  GRACE metrics for", nrow(grace_metrics), "basins\n")

# ============================================================================
# CONSTRUCT MODEL METRIC MATRICES
# ============================================================================

cat("Constructing model metric matrices...\n")

# CESM2: merge dispersion (A, sigma) with events (H_max, D_max)
cesm_events_member <- events_member[model == "CESM2", .(basin_id, member, H_max, D_max)]

cesm_metrics <- merge(cesm_disp, cesm_events_member, by = c("basin_id", "member"), all = TRUE)
cat("  CESM2:", nrow(cesm_metrics), "basin-member combinations\n")

# IPSL: same
ipsl_events_member <- events_member[model == "IPSL", .(basin_id, member, H_max, D_max)]

ipsl_metrics <- merge(ipsl_disp, ipsl_events_member, by = c("basin_id", "member"), all = TRUE)
cat("  IPSL:", nrow(ipsl_metrics), "basin-member combinations\n\n")

# ============================================================================
# COMPUTE COMPATIBILITY PER BASIN
# ============================================================================

cat("Computing compatibility indices...\n")

results <- list()
basins <- unique(grace_metrics$basin_id)
pb <- txtProgressBar(min = 0, max = length(basins), style = 3)

for (i in seq_along(basins)) {
  bid <- basins[i]
  g <- grace_metrics[basin_id == bid]

  M_grace <- c(g$A_grace, g$sigma_grace, g$H_max_grace, g$D_max_grace)

  # CESM2
  cesm_b <- cesm_metrics[basin_id == bid]
  M_cesm <- as.matrix(cesm_b[, .(A, sigma, H_max, D_max)])
  res_cesm <- compute_compatibility(M_grace, M_cesm, RIDGE_LAMBDA)

  # IPSL
  ipsl_b <- ipsl_metrics[basin_id == bid]
  M_ipsl <- as.matrix(ipsl_b[, .(A, sigma, H_max, D_max)])
  res_ipsl <- compute_compatibility(M_grace, M_ipsl, RIDGE_LAMBDA)

  results[[i]] <- data.table(
    basin_id = bid, basin_name = g$basin_name, bd_id = g$bd_id,
    A_grace = g$A_grace, sigma_grace = g$sigma_grace,
    H_max_grace = g$H_max_grace, D_max_grace = g$D_max_grace,
    n_members_cesm = res_cesm$n_members, n_complete_cesm = res_cesm$n_complete,
    d_mahal_cesm = res_cesm$d_mahal, C_b_cesm = res_cesm$C_b,
    compat_class_cesm = res_cesm$compat_class,
    n_members_ipsl = res_ipsl$n_members, n_complete_ipsl = res_ipsl$n_complete,
    d_mahal_ipsl = res_ipsl$d_mahal, C_b_ipsl = res_ipsl$C_b,
    compat_class_ipsl = res_ipsl$compat_class
  )
  setTxtProgressBar(pb, i)
}
close(pb)

compat <- rbindlist(results)

# ============================================================================
# RESULTS SUMMARY
# ============================================================================

cat("\n\n============================================================================\n")
cat("RESULTS SUMMARY\n")
cat("============================================================================\n\n")

cat("CESM2 Compatibility:\n")
cat("  Valid basins:", sum(!is.na(compat$C_b_cesm)), "/", nrow(compat), "\n")
cat("  C_b median:", round(median(compat$C_b_cesm, na.rm=TRUE), 3), "\n")
cat("  C_b mean:", round(mean(compat$C_b_cesm, na.rm=TRUE), 3), "\n")
cat("  Incompatible (C_b < 0.05 or > 0.95):",
    sum(compat$C_b_cesm < 0.05 | compat$C_b_cesm > 0.95, na.rm=TRUE), "\n")
cat("  Compatible (0.10 < C_b < 0.90):",
    sum(compat$C_b_cesm > 0.10 & compat$C_b_cesm < 0.90, na.rm=TRUE), "\n\n")

cat("IPSL Compatibility:\n")
cat("  Valid basins:", sum(!is.na(compat$C_b_ipsl)), "/", nrow(compat), "\n")
cat("  C_b median:", round(median(compat$C_b_ipsl, na.rm=TRUE), 3), "\n")
cat("  C_b mean:", round(mean(compat$C_b_ipsl, na.rm=TRUE), 3), "\n")
cat("  Incompatible (C_b < 0.05 or > 0.95):",
    sum(compat$C_b_ipsl < 0.05 | compat$C_b_ipsl > 0.95, na.rm=TRUE), "\n")
cat("  Compatible (0.10 < C_b < 0.90):",
    sum(compat$C_b_ipsl > 0.10 & compat$C_b_ipsl < 0.90, na.rm=TRUE), "\n\n")

cat("Classification:\n")
cat("  CESM2:\n")
print(table(compat$compat_class_cesm, useNA = "ifany"))
cat("\n  IPSL:\n")
print(table(compat$compat_class_ipsl, useNA = "ifany"))

# ============================================================================
# SAVE
# ============================================================================

cat("\nSaving results...\n")
fwrite(compat, "outputs/phase08_compatibility_4metrics.csv")
saveRDS(compat, "outputs/phase08_compatibility_4metrics.rds")
fwrite(compat, "outputs/phase08_compatibility_basin.csv")

cat("  Saved to outputs/phase08_compatibility_4metrics.csv\n")
cat("  Updated outputs/phase08_compatibility_basin.csv\n\n")

cat("============================================================================\n")
cat("PHASE 08 COMPLETE\n")
cat("============================================================================\n")
