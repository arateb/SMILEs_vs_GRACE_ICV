#!/usr/bin/env Rscript
# ============================================================================
# RUN ALL FIGURE SCRIPTS - 2025-11-27
# ============================================================================
# Master script to execute all figure generation scripts using latest outputs
# ============================================================================

cat("================================================================================\n")
cat("RUNNING ALL FIGURE SCRIPTS - 2025-11-27\n")
cat("================================================================================\n\n")

# Create output directories
dir.create("outputs/figs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figure_data", showWarnings = FALSE, recursive = TRUE)

# List of figure scripts to run
figure_scripts <- c(
  "10b_fig2_dispersion_maps.R",
  "11_fig3_timescale_persistence.R",
  "11b_fig3_timescale_directional.R",
  "11c_fig3_period_matching.R",
  "11d_fig3_period_bands.R",
  "11e_fig3_period_extended.R",
  "12_fig4_event_morphology.R",
  "12_fig_persistence_scatters.R",
  "13_fig5_spatial_dependence.R",
  "14_fig6_compatibility.R",
  "16_fig_mode_counts.R",
  "17_fig_matching_success.R",
  "18_fig_persistence_period.R",
  "19_fig_persistence_maps.R",
  "20_fig_event_maps.R",
  "21_fig_event_comparison.R",
  "22_fig_event_percentiles.R",
  "23_fig_event_scatter.R",
  "24_fig_spatial_correlation.R",
  "25_fig_correlation_network.R",
  "26_fig_compatibility_maps.R",
  "27_fig_metric_scatter.R",
  "28_fig_compatibility_methods.R"
)

# Track results
results <- data.frame(
  script = character(),
  status = character(),
  error = character(),
  stringsAsFactors = FALSE
)

# Run each script
for (script in figure_scripts) {
  script_path <- file.path("src", script)

  if (!file.exists(script_path)) {
    cat("SKIP: ", script, " (not found)\n")
    results <- rbind(results, data.frame(script = script, status = "skipped", error = "file not found"))
    next
  }

  cat("\n----------------------------------------\n")
  cat("Running: ", script, "\n")
  cat("----------------------------------------\n")

  tryCatch({
    source(script_path)
    cat("SUCCESS: ", script, "\n")
    results <- rbind(results, data.frame(script = script, status = "success", error = ""))
  }, error = function(e) {
    cat("ERROR: ", script, "\n")
    cat("  Message: ", conditionMessage(e), "\n")
    results <<- rbind(results, data.frame(script = script, status = "error", error = conditionMessage(e)))
  })
}

cat("\n================================================================================\n")
cat("SUMMARY\n")
cat("================================================================================\n\n")

n_success <- sum(results$status == "success")
n_error <- sum(results$status == "error")
n_skip <- sum(results$status == "skipped")

cat("Total scripts: ", nrow(results), "\n")
cat("Success: ", n_success, "\n")
cat("Errors: ", n_error, "\n")
cat("Skipped: ", n_skip, "\n\n")

if (n_error > 0) {
  cat("Scripts with errors:\n")
  print(results[results$status == "error", c("script", "error")])
}

cat("\n================================================================================\n")
cat("COMPLETE\n")
cat("================================================================================\n")
