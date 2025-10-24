# ==============================================================================
# Function: apply_nature_journal_theme
# ==============================================================================
# Original name: theme_nature
# Source file: Claude_18_Qs_results_amplitude_Extremes.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#
# ==============================================================================

apply_nature_journal_theme <- function() {
  theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}
