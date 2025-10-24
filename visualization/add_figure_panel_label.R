# ==============================================================================
# Function: add_figure_panel_label
# ==============================================================================
# Original name: add_panel_label
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#' Add panel labels (a, b, c, etc.)
#
# ==============================================================================

add_figure_panel_label <- function(label, x = 0.02, y = 0.98) {
  annotation_custom(
    grob = textGrob(label, x = x, y = y, hjust = 0, vjust = 1,
                    gp = gpar(fontsize = NATURE_SPECS$panel_label_size, 
                             fontface = "bold"))
  )
}
