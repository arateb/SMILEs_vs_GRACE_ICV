# ============================================================================
# MASTER SCRIPT: Run Complete Comprehensive Analysis
# ============================================================================
# This script runs both parts of the comprehensive GRACE vs CESM2 analysis
# ============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  COMPREHENSIVE GRACE vs CESM2 ANALYSIS                        ║\n")
cat("║  Publication Figures for AGU & Nature Geoscience              ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")
cat("\n")

start_time <- Sys.time()

# Run Part 1 (Parts 1-2 of analysis)
cat("Running Part 1 (Main Comparisons & Performance Summary)...\n")
source("codes/comprehensive_grace_cesm2_analysis.R")

# Run Part 2 (Parts 3-8 of analysis)
cat("\nRunning Part 2 (Classification, Bias, Coverage, Irrigation, Cross-Metric, Spread)...\n")
source("codes/comprehensive_grace_cesm2_analysis_part2.R")

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║  ANALYSIS COMPLETE                                            ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat(sprintf("Total Time: %.1f minutes\n", elapsed))
cat(sprintf("Output: %s/\n", today_folder))
cat("\nAll figures ready for publication!\n\n")
