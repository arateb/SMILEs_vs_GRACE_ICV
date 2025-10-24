# ==============================================================================
# Function: analyze_multiple_basins_parallel
# ==============================================================================
# Original name: analyze_multiple_basins
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# -----------------------------------------------------------------------------
# HELPER: Process multiple basins with progress tracking
# -----------------------------------------------------------------------------
#
# ==============================================================================

analyze_multiple_basins_parallel <- function(basin_data_list, verbose = TRUE, 
                                   save_progress = FALSE, save_dir = ".") {
  
  n_basins <- length(basin_data_list)
  all_results <- list()
  failed_basins <- c()
  
  total_start <- Sys.time()
  
  cat(sprintf("\nStarting analysis of %d basins\n", n_basins))
  cat(sprintf("Time: %s\n\n", Sys.time()))
  
  for (i in 1:n_basins) {
    if (verbose && i %% 10 == 1) {
      cat(sprintf("\n--- Basin batch %d-%d of %d ---\n", 
                  i, min(i+9, n_basins), n_basins))
    }
    
    tryCatch({
      # Extract data for this basin
      basin_data <- basin_data_list[[i]]
      
      # Analyze one basin
      basin_results <- analyze_one(
        Gattrs = basin_data$Gattrs,
        obs_b = basin_data$obs_b,
        ensP = basin_data$ensP,
        ensC = basin_data$ensC,
        verbose = (i <= 3)  # Only verbose for first 3 basins
      )
      
      all_results[[i]] <- basin_results
      
    }, error = function(e) {
      warning(sprintf("Basin %d failed: %s", i, e$message))
      failed_basins <- c(failed_basins, i)
      all_results[[i]] <- list(error = e$message)
    })
    
    # Save progress periodically
    if (save_progress && i %% 25 == 0) {
      saveRDS(all_results, file.path(save_dir, sprintf("basin_results_progress_%d.rds", i)))
      cat(sprintf("   Progress saved at basin %d\n", i))
    }
    
    # Time estimate every 10 basins
    if (verbose && i %% 10 == 0 && i < n_basins) {
      elapsed <- as.numeric(difftime(Sys.time(), total_start, units = "mins"))
      rate <- elapsed / i
      remaining <- rate * (n_basins - i)
      cat(sprintf("   Elapsed: %.1f min | Remaining: %.1f min | ETA: %s\n", 
                  elapsed, remaining, 
                  format(Sys.time() + remaining * 60, "%H:%M")))
    }
  }
  
  # Final summary
  total_time <- difftime(Sys.time(), total_start, units = "mins")
  
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("ANALYSIS COMPLETE\n")
  cat(sprintf("Total time: %.1f minutes\n", as.numeric(total_time)))
  cat(sprintf("Average per basin: %.1f seconds\n", as.numeric(total_time) * 60 / n_basins))
  cat(sprintf("Successful: %d basins\n", n_basins - length(failed_basins)))
  if (length(failed_basins) > 0) {
    cat(sprintf("Failed: %d basins (indices: %s)\n", 
                length(failed_basins), paste(failed_basins, collapse = ", ")))
  }
  cat(rep("=", 60), "\n\n", sep = "")
  
  return(list(
    results = all_results,
    failed = failed_basins,
    timing = list(
      total_minutes = as.numeric(total_time),
      per_basin_seconds = as.numeric(total_time) * 60 / n_basins
    )
  ))
}
