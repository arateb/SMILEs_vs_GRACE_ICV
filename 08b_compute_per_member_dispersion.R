#!/usr/bin/env Rscript
# ============================================================================
# COMPUTE PER-MEMBER DISPERSION METRICS (A, sigma)
# ============================================================================
# Computes amplitude (A) and variance (sigma) per basin × member
# for use in multivariate compatibility analysis (Phase 08)
#
# Author: Ashraf Rateb
# Date: 2025-12-02
# ============================================================================

library(data.table)

cat("============================================================================\n")
cat("COMPUTING PER-MEMBER DISPERSION METRICS\n")
cat("============================================================================\n\n")

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading filtered data...\n")
filtered_data <- readRDS("data/Processed_Filtered_Nov2025.rds")

# Extract arrays: [basin × member × time]
G_CESM <- filtered_data$G_CESM_filtered
G_IPSL <- filtered_data$G_IPSL_combined
date_cesm <- filtered_data$date_cesm
date_ipsl <- filtered_data$date_ipsl

# Load GRACE reference
raw_data <- readRDS("data/Enhanced_GGFO_MMLEs_Nov2025.rds")
dates_grace <- raw_data$dates_grace
attrs <- raw_data$attrs

cat("  CESM2 dimensions:", paste(dim(G_CESM), collapse=" × "), "\n")
cat("  IPSL dimensions:", paste(dim(G_IPSL), collapse=" × "), "\n")
cat("  GRACE period:", as.character(range(dates_grace)), "\n")
cat("  GRACE length:", length(dates_grace), "months\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Use GRACE-equivalent period: 2002-2024
# Find indices in model data that correspond to 2002-2024
grace_start <- as.Date("2002-04-01")
grace_end <- as.Date("2024-06-01")
Lg <- length(dates_grace)  # GRACE window length

cat("Configuration:\n")
cat("  Target period: 2002-04 to 2024-06 (", Lg, " months)\n")
cat("  Computing A = max - min (amplitude)\n")
cat("  Computing sigma = sd (standard deviation)\n\n")

# ============================================================================
# COMPUTE CESM2 PER-MEMBER METRICS
# ============================================================================

cat("Computing CESM2 per-member metrics...\n")

n_basins <- dim(G_CESM)[1]
n_members_cesm <- dim(G_CESM)[2]

# Find GRACE-equivalent window in CESM2 timeline
cesm_grace_idx <- which(date_cesm >= grace_start & date_cesm <= grace_end)
cat("  CESM2 GRACE-equivalent window:", length(cesm_grace_idx), "months\n")

cesm_member_results <- list()
k <- 0

for (b in 1:n_basins) {
  if (b %% 50 == 0) cat("    Basin", b, "/", n_basins, "\n")

  for (m in 1:n_members_cesm) {
    # Extract GRACE-equivalent window for this member
    window_data <- G_CESM[b, m, cesm_grace_idx]

    if (all(is.na(window_data))) next

    k <- k + 1
    cesm_member_results[[k]] <- data.table(
      basin_id = b,
      basin_name = attrs$name[b],
      model = "CESM2",
      member = m,
      A = diff(range(window_data, na.rm = TRUE)),
      sigma = sd(window_data, na.rm = TRUE)
    )
  }
}

cesm_disp_member <- rbindlist(cesm_member_results)
cat("  CESM2 member-basin combinations:", nrow(cesm_disp_member), "\n\n")

# ============================================================================
# COMPUTE IPSL PER-MEMBER METRICS
# ============================================================================

cat("Computing IPSL per-member metrics...\n")

n_members_ipsl <- dim(G_IPSL)[2]

# Find GRACE-equivalent window in IPSL timeline
ipsl_grace_idx <- which(date_ipsl >= grace_start & date_ipsl <= grace_end)
cat("  IPSL GRACE-equivalent window:", length(ipsl_grace_idx), "months\n")

ipsl_member_results <- list()
k <- 0

for (b in 1:n_basins) {
  if (b %% 50 == 0) cat("    Basin", b, "/", n_basins, "\n")

  for (m in 1:n_members_ipsl) {
    # Extract GRACE-equivalent window for this member
    window_data <- G_IPSL[b, m, ipsl_grace_idx]

    if (all(is.na(window_data))) next

    k <- k + 1
    ipsl_member_results[[k]] <- data.table(
      basin_id = b,
      basin_name = attrs$name[b],
      model = "IPSL",
      member = m,
      A = diff(range(window_data, na.rm = TRUE)),
      sigma = sd(window_data, na.rm = TRUE)
    )
  }
}

ipsl_disp_member <- rbindlist(ipsl_member_results)
cat("  IPSL member-basin combinations:", nrow(ipsl_disp_member), "\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("Saving results...\n")

saveRDS(cesm_disp_member, "outputs/phase08_cesm_dispersion_member.rds")
saveRDS(ipsl_disp_member, "outputs/phase08_ipsl_dispersion_member.rds")

cat("  Saved: outputs/phase08_cesm_dispersion_member.rds\n")
cat("  Saved: outputs/phase08_ipsl_dispersion_member.rds\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("============================================================================\n")
cat("SUMMARY\n")
cat("============================================================================\n\n")

cat("CESM2:\n")
cat("  Basins:", length(unique(cesm_disp_member$basin_id)), "\n")
cat("  Members per basin:", n_members_cesm, "\n")
cat("  A median:", round(median(cesm_disp_member$A), 1), "mm\n")
cat("  sigma median:", round(median(cesm_disp_member$sigma), 1), "mm\n\n")

cat("IPSL:\n")
cat("  Basins:", length(unique(ipsl_disp_member$basin_id)), "\n")
cat("  Members per basin:", n_members_ipsl, "\n")
cat("  A median:", round(median(ipsl_disp_member$A), 1), "mm\n")
cat("  sigma median:", round(median(ipsl_disp_member$sigma), 1), "mm\n\n")

cat("============================================================================\n")
cat("PER-MEMBER DISPERSION COMPLETE\n")
cat("============================================================================\n")
