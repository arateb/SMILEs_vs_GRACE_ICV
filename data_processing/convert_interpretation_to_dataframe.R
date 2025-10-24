# ==============================================================================
# Function: convert_interpretation_to_dataframe
# ==============================================================================
# Original name: interpretation_to_df
# Source file: F01_Functions_MMLEs_vs_GGFO_Jun2025.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#
# ==============================================================================

convert_interpretation_to_dataframe <- function(interp_list, Gattrs) {
  # Extract numeric values from strings using regex
  extract_ratio <- function(text, metric) {
    pattern <- paste0(metric, "=([0-9.]+)")
    as.numeric(gsub(pattern, "\\1", regmatches(text, regexpr(pattern, text))))
  }
  
  extract_cv <- function(text) {
    as.numeric(gsub(".*CV=([0-9.]+).*", "\\1", text))
  }
  
  extract_percent <- function(text, metric) {
    pattern <- paste0("([0-9]+)% \\(", metric, "\\)")
    as.numeric(gsub(pattern, "\\1", regmatches(text, regexpr(pattern, text))))
  }
  
  # Parse the interpretation strings
  ratios_text <- interp_list$ratios
  
  # Create the data frame row
  df <- data.frame(
    # All Gattrs columns
    ID = Gattrs$ID,
    name = Gattrs$name,
    bd_id = Gattrs$bd_id,
    area = Gattrs$area,
    C_lon = Gattrs$C_lon,
    C_lat = Gattrs$C_lat,
    IrrigatPct = Gattrs$IrrigatPct,
    
    # Model info
    model = gsub(" ensemble:.*", "", interp_list$ensemble),
    n_members = as.numeric(gsub(".*: ([0-9]+) members", "\\1", interp_list$ensemble)),
    
    # Variability assessments
    var_status = gsub(".*Variability: ([a-z ]+) \\(.*", "\\1", interp_list$variability),
    var_pvalue = as.numeric(gsub(".*\\(p=([0-9.]+)\\)", "\\1", interp_list$variability)),
    
    amp_status = gsub(".*Amplitude: ([a-z ]+) \\(.*", "\\1", interp_list$amplitude),
    amp_pvalue = as.numeric(gsub(".*\\(p=([0-9.]+)\\)", "\\1", interp_list$amplitude)),
    
    tail_status = gsub(".*Tail behavior: ([a-z ]+) \\(.*", "\\1", interp_list$tail_behavior),
    tail_pvalue = as.numeric(gsub(".*\\(p=([0-9.]+)\\)", "\\1", interp_list$tail_behavior)),
    
    # Ratios
    sd_ratio = extract_ratio(ratios_text, "σ"),
    amp_ratio = extract_ratio(ratios_text, "Amplitude"),
    p95_05_ratio = extract_ratio(ratios_text, "95-5 range"),
    
    # Ensemble consistency
    ensemble_cv = extract_cv(interp_list$ensemble_consistency),
    
    # Member performance
    pct_good_sd = extract_percent(interp_list$member_performance, "σ"),
    pct_good_amp = extract_percent(interp_list$member_performance, "amplitude"),
    
    # Overall assessment
    reliability = gsub("([✓⚠️✗]) ([^:]+):.*", "\\2", interp_list$reliability),
    recommendation = interp_list$recommendation,
    key_findings = ifelse(is.null(interp_list$key_findings), NA, interp_list$key_findings),
    
    stringsAsFactors = FALSE
  )
  
  return(df)}
