# ==============================================================================
# Function: create_temporal_patterns_figure
# ==============================================================================
# Original name: make_figure6# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 6: CROSS-MODEL CONSISTENCY
# =============================================================================

#
# ==============================================================================

create_temporal_patterns_figure <- function(amplsd_aug) {
  
  cat("Creating Figure 6: Cross-Model Consistency...\n")
  
  # Prepare data
  wide_data <- amplsd_aug %>%
    select(bd_ID, model_name, amp_grace, median_amp, IQR_amp, nCRPS_amp) %>%
    pivot_wider(
      id_cols = c(bd_ID, amp_grace),
      names_from = model_name,
      values_from = c(median_amp, IQR_amp, nCRPS_amp)
    ) %>%
    mutate(
      cesm_adequate = abs(amp_grace / median_amp_CESM2 - 1) <= 0.2,
      ipsl_adequate = abs(amp_grace / median_amp_IPSL - 1) <= 0.2,
      consistency = case_when(
        cesm_adequate & ipsl_adequate ~ "Both succeed",
        !cesm_adequate & !ipsl_adequate ~ "Both fail",
        cesm_adequate ~ "CESM2 only",
        ipsl_adequate ~ "IPSL only"
      )
    )
  
  # Panel A: Consistency pie chart
  consistency_summary <- wide_data %>%
    count(consistency) %>%
    mutate(pct = n / sum(n) * 100)
  
  p6a <- ggplot(consistency_summary, aes(x = "", y = pct, fill = consistency)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c("Both succeed" = "#1B7837",
                                 "Both fail" = "#762A83",
                                 "CESM2 only" = "#2C7BB6",
                                 "IPSL only" = "#D7191C")) +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              position = position_stack(vjust = 0.5), size = 3) +
    labs(title = "a. Cross-model consistency") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "bottom")
  
  # Panel B: Model comparison scatter
  p6b <- ggplot(wide_data, aes(x = median_amp_CESM2, y = median_amp_IPSL, 
                                color = consistency)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = c("Both succeed" = "#1B7837",
                                  "Both fail" = "#762A83",
                                  "CESM2 only" = "#2C7BB6",
                                  "IPSL only" = "#D7191C")) +
    labs(title = "b. Model agreement", 
         x = "CESM2 amplitude (mm)", 
         y = "IPSL amplitude (mm)") +
    theme_nature() +
    theme(legend.position = "none")
  
  # Panel C: Spread comparison
  p6c <- ggplot(wide_data, aes(x = IQR_amp_CESM2, y = IQR_amp_IPSL, 
                                color = consistency)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = c("Both succeed" = "#1B7837",
                                  "Both fail" = "#762A83",
                                  "CESM2 only" = "#2C7BB6",
                                  "IPSL only" = "#D7191C")) +
    labs(title = "c. Spread comparison", 
         x = "CESM2 IQR (mm)", 
         y = "IPSL IQR (mm)") +
    theme_nature() +
    theme(legend.position = "none")
  
  # Panel D: CRPS comparison
  p6d <- ggplot(wide_data, aes(x = nCRPS_amp_CESM2, y = nCRPS_amp_IPSL, 
                                color = consistency)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = c("Both succeed" = "#1B7837",
                                  "Both fail" = "#762A83",
                                  "CESM2 only" = "#2C7BB6",
                                  "IPSL only" = "#D7191C")) +
    labs(title = "d. CRPS comparison", 
         x = "CESM2 CRPS", 
         y = "IPSL CRPS") +
    theme_nature()
  
  fig6 <- (p6a + p6b) / (p6c + p6d)
  
  cat("  ✓ Figure 6 complete\n")
  return(fig6)
}
