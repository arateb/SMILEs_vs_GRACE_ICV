# ============================================================================
# MAIN ANALYSIS PIPELINE - NOVEMBER 2025
# ============================================================================
# Clean implementation for GRACE-FO vs CESM2/IPSL comparison
# Author: Ashraf Rateb
# Date: 2025-11-18
# ============================================================================

# Load enhanced clean dataset
cat("Loading Enhanced_GGFO_MMLEs_Nov2025.rds...\n")
data <- readRDS("data/Enhanced_GGFO_MMLEs_Nov2025.rds")

# Extract components
attrs <- data$attrs
G_CESM <- data$G_CESM
G_IPSL_GH <- data$G_IPSL_GH
G_IPSL_NAT <- data$G_IPSL_NAT
gfo_wtrend <- data$gfo_wtrend
gfo_dtrend <- data$gfo_dtrend
dates_grace <- data$dates_grace
date_cesm <- data$date_cesm
date_ipsl <- data$date_ipsl

# Load functions
list2env(data$functions, .GlobalEnv)

cat("✓ Data loaded successfully\n")
cat("  Created:", as.character(data$created_date), "\n")
cat("  Basins:", nrow(attrs), "\n")
cat("  CESM2:", paste(dim(G_CESM), collapse=" × "), "\n")
cat("  IPSL-GH:", paste(dim(G_IPSL_GH), collapse=" × "), "\n")
cat("  IPSL-NAT:", paste(dim(G_IPSL_NAT), collapse=" × "), "\n")
cat("  Functions loaded:", length(data$functions), "\n\n")

cat("Ready to analyze.\n")
