# ==============================================================================
# Function: generate_all_manuscript_figures
# ==============================================================================
# Original name: make_all_figures
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# MAIN FUNCTION: GENERATE ALL FIGURES
# =============================================================================

#
# ==============================================================================

generate_all_manuscript_figures <- function(amplsd_aug, basins_sf, 
                             q10_cross = NULL,
                             wave_results = NULL, 
                             network_results = NULL,
                             output_dir = "/mnt/user-data/outputs/figures") {
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  GENERATING ALL NATURE-QUALITY FIGURES\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("\n")
  
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Validate geometries
  basins_sf <- st_make_valid(basins_sf)
  
  # Generate main figures (ALWAYS)
  fig1 <- make_figure1(amplsd_aug, basins_sf)
  fig2 <- make_figure2(amplsd_aug, basins_sf)
  fig3 <- make_figure3(amplsd_aug, basins_sf)
  fig4 <- make_figure4(amplsd_aug, basins_sf)
  fig5 <- make_figure5(amplsd_aug)
  fig6 <- make_figure6(amplsd_aug)
  
  # Generate optional figures
  fig7 <- make_figure7(wave_results, basins_sf)
  fig8 <- make_figure8(q10_cross, basins_sf)
  fig9 <- make_figure9(network_results, basins_sf)
  
  # Save figures
  cat("\nSaving figures...\n")
  
  save_fig <- function(fig, name, num) {
    if (!is.null(fig)) {
      ggsave(file.path(output_dir, paste0(name, ".pdf")), fig,
             width = 183, height = 240, units = "mm", dpi = 350, device = cairo_pdf)
      ggsave(file.path(output_dir, paste0(name, ".png")), fig,
             width = 183, height = 240, units = "mm", dpi = 350)
      cat(sprintf("  ✓ Figure %d saved\n", num))
    }
  }
  
  save_fig(fig1, "Figure1_Global_Patterns", 1)
  save_fig(fig2, "Figure2_Model_Performance_1_12_08", 2)
  save_fig(fig3, "Figure3_Extreme_Coverage_5_95", 3)
  save_fig(fig4, "Figure4_Ensemble_Spread", 4)
  save_fig(fig5, "Figure5_PIT_Calibration", 5)
  save_fig(fig6, "Figure6_Cross_Model", 6)
  save_fig(fig7, "Figure7_Wavelet_Modes", 7)
  save_fig(fig8, "Figure8_Ensemble_Position", 8)
  save_fig(fig9, "Figure9_Extremal_Networks", 9)
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  ✅ ALL FIGURES COMPLETE!\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat(sprintf("\nOutput: %s\n", output_dir))
  cat("Format: PDF (vector) + PNG (raster) at 300 DPI\n")
  cat("\nGenerated figures:\n")
  cat("  • Figure 1: Global Patterns (6 panels)\n")
  cat("  • Figure 2: Model Performance (4 panels)\n")
  cat("  • Figure 3: Extreme Coverage (6 panels)\n")
  cat("  • Figure 4: Ensemble Spread (6 panels)\n")
  cat("  • Figure 5: PIT Calibration (4 panels)\n")
  cat("  • Figure 6: Cross-Model Consistency (4 panels)\n")
  if (!is.null(fig7)) cat("  • Figure 7: Wavelet Modes (4 panels)\n")
  if (!is.null(fig8)) cat("  • Figure 8: Ensemble Position Analysis (6 panels)\n")
  if (!is.null(fig9)) cat("  • Figure 9: Extremal Networks (4 panels)\n")
  cat("\n")
  
  return(list(fig1 = fig1, fig2 = fig2, fig3 = fig3, fig4 = fig4, 
              fig5 = fig5, fig6 = fig6, fig7 = fig7, fig8 = fig8, fig9 = fig9))
}
