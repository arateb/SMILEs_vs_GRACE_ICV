# ============================================================================
# PHASE 03 - VALIDATION TESTS
# ============================================================================
# Quick tests to verify code correctness before running full analysis
# ============================================================================

library(data.table)
library(tidyverse)

cat("============================================================================\n")
cat("PHASE 03 VALIDATION TESTS\n")
cat("============================================================================\n\n")

# ============================================================================
# TEST 1: Load functions
# ============================================================================

cat("TEST 1: Loading functions...\n")
tryCatch({
  source("src/functions/windowing_functions_nov2025.R")
  cat("  ✓ Functions loaded successfully\n\n")
}, error = function(e) {
  cat("  ✗ ERROR loading functions:", conditionMessage(e), "\n")
  stop("Failed to load functions")
})

# ============================================================================
# TEST 2: Load data
# ============================================================================

cat("TEST 2: Loading data...\n")
tryCatch({
  data <- readRDS("data/Enhanced_GGFO_MMLEs_Nov2025.rds")
  attrs <- data$attrs
  dates_grace <- data$dates_grace
  cat("  ✓ Data loaded successfully\n")
  cat("    Basins:", nrow(attrs), "\n")
  cat("    GRACE obs:", length(dates_grace), "\n\n")
}, error = function(e) {
  cat("  ✗ ERROR loading data:", conditionMessage(e), "\n")
  stop("Failed to load data")
})

# ============================================================================
# TEST 3: Test window generation on small subset
# ============================================================================

cat("TEST 3: Testing window generation (1 basin, 2 members, 600 months)...\n")

# Create small test array
test_array <- array(rnorm(1 * 2 * 600), dim = c(1, 2, 600))
test_dates <- seq.Date(as.Date("1900-01-01"), by = "month", length.out = 600)
test_attrs <- data.frame(
  ID = 1,
  name = "Test Basin",
  bd_id = "bd0001",
  area = 1000000,
  C_lon = 0,
  C_lat = 0,
  IrrigatPct = 5
)

tryCatch({
  test_windows <- make_model_windows(
    x = test_array,
    dates = test_dates,
    window_length = 273,
    date_start = "1900-01-01",
    date_end = "1950-12-31",
    attrs = test_attrs
  )

  cat("  ✓ Windows generated successfully\n")
  cat("    Total windows:", nrow(test_windows), "\n")
  cat("    Columns:", paste(names(test_windows), collapse = ", "), "\n")
  cat("    Has attrs:", "name" %in% names(test_windows), "\n\n")
}, error = function(e) {
  cat("  ✗ ERROR generating windows:", conditionMessage(e), "\n")
  stop("Window generation failed")
})

# ============================================================================
# TEST 4: Test dispersion metrics
# ============================================================================

cat("TEST 4: Testing dispersion metrics computation...\n")

tryCatch({
  test_disp <- compute_dispersion_metrics(
    x = test_array,
    window_meta = test_windows
  )

  n_valid <- sum(!is.na(test_disp$amplitude))

  cat("  ✓ Metrics computed successfully\n")
  cat("    Windows with valid amplitude:", n_valid, "/", nrow(test_disp), "\n")
  cat("    Mean amplitude:", round(mean(test_disp$amplitude, na.rm = TRUE), 2), "\n")
  cat("    Mean sigma:", round(mean(test_disp$sigma, na.rm = TRUE), 2), "\n\n")
}, error = function(e) {
  cat("  ✗ ERROR computing metrics:", conditionMessage(e), "\n")
  stop("Metrics computation failed")
})

# ============================================================================
# TEST 5: Test GRACE metrics
# ============================================================================

cat("TEST 5: Testing GRACE metrics computation...\n")

# Create test GRACE data
test_grace <- matrix(rnorm(273 * 1), nrow = 273, ncol = 1)
test_grace_dates <- seq.Date(as.Date("2002-04-01"), by = "month", length.out = 273)

tryCatch({
  test_grace_metrics <- compute_grace_metrics(
    x_grace = test_grace,
    dates_grace = test_grace_dates,
    attrs = test_attrs
  )

  cat("  ✓ GRACE metrics computed successfully\n")
  cat("    A_grace:", round(test_grace_metrics$A_grace[1], 2), "\n")
  cat("    sigma_grace:", round(test_grace_metrics$sigma_grace[1], 2), "\n")
  cat("    n_obs:", test_grace_metrics$n_obs[1], "\n\n")
}, error = function(e) {
  cat("  ✗ ERROR computing GRACE metrics:", conditionMessage(e), "\n")
  stop("GRACE metrics computation failed")
})

# ============================================================================
# TEST 6: Test summary computation
# ============================================================================

cat("TEST 6: Testing basin summary computation...\n")

tryCatch({
  test_summary <- summarize_dispersion_by_basin(
    dispersion_windows = test_disp,
    grace_metrics = test_grace_metrics,
    model_name = "TEST",
    attrs = test_attrs
  )

  cat("  ✓ Summary computed successfully\n")
  cat("    Basins:", nrow(test_summary), "\n")
  cat("    Columns:", ncol(test_summary), "\n")
  cat("    A_p50:", round(test_summary$A_p50[1], 2), "\n")
  cat("    T_A:", round(test_summary$T_A[1], 2), "\n")
  cat("    Amplitude covered:", test_summary$amplitude_covered[1], "\n")
  cat("    Has attrs:", "name" %in% names(test_summary), "\n\n")
}, error = function(e) {
  cat("  ✗ ERROR computing summary:", conditionMessage(e), "\n")
  stop("Summary computation failed")
})

# ============================================================================
# TEST 7: Test classification
# ============================================================================

cat("TEST 7: Testing basin classification...\n")

# Create second summary for classification
test_summary2 <- copy(test_summary)
test_summary2$amplitude_covered <- FALSE  # Force different coverage

tryCatch({
  test_class <- classify_basin_coverage(
    summary_cesm = test_summary,
    summary_ipsl = test_summary2
  )

  cat("  ✓ Classification computed successfully\n")
  cat("    Basin class (amplitude):", as.character(test_class$basin_class_amplitude[1]), "\n")
  cat("    Basin class (variance):", as.character(test_class$basin_class_variance[1]), "\n")
  cat("    Basin class (strict):", as.character(test_class$basin_class_strict[1]), "\n\n")
}, error = function(e) {
  cat("  ✗ ERROR in classification:", conditionMessage(e), "\n")
  stop("Classification failed")
})

# ============================================================================
# SUMMARY
# ============================================================================

cat("============================================================================\n")
cat("ALL TESTS PASSED\n")
cat("============================================================================\n\n")

cat("Phase 03 code is ready to run!\n")
cat("Execute: Rscript src/03_phase03_dispersion_windows_nov2025.R\n\n")
