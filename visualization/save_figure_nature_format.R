# ==============================================================================
# Function: save_figure_nature_format
# ==============================================================================
# Original name: save_nature_figure
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#' Save figure with Nature specifications
#
# ==============================================================================

save_figure_nature_format <- function(plot, filename, width_type = "single", dpi = 400, 
format = "pdf") {
  size <- nature_figure_size(width_type)
  
  # Validate DPI
  if (dpi < 300) {
    warning("Nature requires minimum 300 dpi. Setting to 300.")
    dpi <- 300
  }
  
  ggsave(filename, plot, 
         width = size$width, 
         height = size$height, 
         dpi = dpi, 
         units = "in", 
         device = format)
  
  cat(sprintf("Figure saved: %s (%s format, %d dpi)\n", filename, format, dpi))
}
