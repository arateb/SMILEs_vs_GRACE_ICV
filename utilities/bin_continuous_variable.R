# ==============================================================================
# Function: bin_continuous_variable
# ==============================================================================
# Original name: bin_cut
# Source file: Claude_18_Qs_results_amplitude_Extremes.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# F3. Member hit fractions (stacked bins)
#
# ==============================================================================

bin_continuous_variable <- function(x) cut(x, breaks=c(-Inf,0,0.25,0.75,Inf),
