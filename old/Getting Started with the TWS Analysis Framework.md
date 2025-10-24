# Getting Started with the TWS Analysis Framework

This guide will help you get started with using the TWS Analysis Framework for comparing GRACE satellite observations with climate model ensemble outputs.

## Quick Start

### 1. Installation

First, ensure you have R (version ≥ 4.0) installed. Then install the required packages:

```R
# Install required packages
install.packages(c(
  "tidyverse",  # Data manipulation and visualization
  "sf",         # Spatial data handling
  "viridis",    # Color palettes
  "gridExtra",  # Arranging plots
  "scales"      # Plot scaling
))
```

### 2. Loading Functions

You can source individual functions or entire categories:

```R
# Source a single function
source("core_analysis/analyze_basin_tws.R")

# Source all functions in a category
r_files <- list.files("wavelet_analysis", pattern = "\\.R$", full.names = TRUE)
sapply(r_files, source)

# Or create a helper function to load all
load_tws_framework <- function(base_path = ".") {
  categories <- c("core_analysis", "data_processing", "ensemble_metrics",
                  "extreme_analysis", "figures", "interpretation",
                  "mapping", "utilities", "visualization", "wavelet_analysis")
  
  for (category in categories) {
    cat_path <- file.path(base_path, category)
    if (dir.exists(cat_path)) {
      r_files <- list.files(cat_path, pattern = "\\.R$", full.names = TRUE)
      sapply(r_files, source, .GlobalEnv)
      cat(sprintf("Loaded %d functions from %s\n", length(r_files), category))
    }
  }
}

# Use it
load_tws_framework()
```

### 3. Basic Workflow

Here's a typical analysis workflow:

```R
# 1. Load your data
# Assuming you have:
# - obs_data: GRACE observations for your basin
# - cesm_ensemble: CESM2 ensemble members
# - ipsl_ensemble: IPSL ensemble members
# - basin_attributes: Metadata about the basin

# 2. Run analysis for a single basin
results <- analyze_basin_tws(
  Gattrs = basin_attributes,
  obs_b = obs_data,
  ensC = cesm_ensemble,
  ensP = ipsl_ensemble,
  verbose = TRUE
)

# 3. Extract specific results
amplitude_metrics <- results$amplitude_metrics
extreme_results <- results$extremes_summary
wavelet_data <- results$wave_full

# 4. For multiple basins, use parallel processing
all_results <- analyze_multiple_basins_parallel(
  basin_list = list_of_basins,
  obs_list = list_of_observations,
  cesm_list = list_of_cesm_data,
  ipsl_list = list_of_ipsl_data,
  n_cores = 4
)
```

## Data Requirements

### Input Data Format

The framework expects the following data structures:

#### 1. Observation Data (`obs_b`)
- **Type:** Numeric vector
- **Description:** Time series of GRACE TWS anomalies
- **Units:** Typically cm or mm of equivalent water height
- **Length:** Should match the temporal extent of your analysis period

#### 2. Ensemble Data (`ensC`, `ensP`)
- **Type:** Numeric matrix
- **Dimensions:** time × ensemble_members
- **Description:** Each column represents one ensemble member's TWS time series
- **Units:** Same as observation data

#### 3. Basin Attributes (`Gattrs`)
- **Type:** Data frame with one row
- **Required columns:**
  - `ID`: Unique basin identifier
  - `name`: Basin name
  - `area`: Basin area in km²
  - `C_lon`, `C_lat`: Centroid coordinates (optional but recommended)

Example:
```R
basin_attributes <- data.frame(
  ID = 12345,
  name = "Amazon Basin",
  area = 6100000,
  C_lon = -62.5,
  C_lat = -5.0
)
```

## Understanding the Output

The `analyze_basin_tws()` function returns a list with multiple components:

### Amplitude Analysis
- `amplitude_metrics`: Detailed metrics comparing GRACE amplitude with ensemble spread
- `amplitude_summary`: Summary statistics
- `amplitude_interpretation`: Qualitative interpretation of model performance

### Extreme Analysis
- `extremes_model`: Model-level extreme coverage statistics
- `extremes_basin`: Basin-specific extreme event analysis
- `extremes_comparison`: Cross-model comparison
- `extremes_summary`: Summary of extreme coverage

### Wavelet Analysis
- `wave_full`: Complete wavelet transform data
- `wave_q1_summary`: Dominant modes in GRACE
- `wave_q2_timescale`: Timescale representation assessment
- `wave_q3_consistency`: Ensemble consistency metrics
- `wave_q4_grace_dominant`: Dominant mode reproduction analysis
- `wave_q5_scores`: Model comparison scores

### Metadata
- `metadata`: Analysis metadata including timing, basin info, and data dimensions

## Creating Figures

The framework includes functions to generate publication-quality figures:

```R
# Load spatial data (shapefile)
basin_shapefile <- st_read("path/to/basins.shp")

# Extract and merge results with spatial data
spatial_results <- extract_and_merge_spatial_data(
  all_results = all_results,
  shp = basin_shapefile,
  result_type = "amplitude_metrics"
)

# Create a map
map_plot <- create_robinson_projection_map(
  data = spatial_results$merged_shp,
  variable = "amplitude_ratio",
  title = "GRACE vs. Model Amplitude Ratio"
)

# Save with Nature journal specifications
save_figure_nature_format(
  plot = map_plot,
  filename = "figure1_amplitude_map.pdf",
  width_type = "double",
  dpi = 400
)
```

## Common Issues and Solutions

### Issue: Missing Dependencies
**Solution:** Ensure all required packages are installed. Some functions may require additional packages not listed in the main dependencies.

### Issue: Data Dimension Mismatch
**Solution:** Verify that observation and ensemble time series have the same length. Use `length(obs_b)` and `nrow(ensC)` to check.

### Issue: Memory Limitations with Large Ensembles
**Solution:** Consider processing basins in batches or reducing the number of ensemble members for initial testing.

## Next Steps

1. Review the `FUNCTION_CATALOG.md` for detailed function documentation
2. Examine example scripts in the original source files
3. Adapt the workflow to your specific data and research questions
4. Consult the associated paper for methodological details

## Support

For questions or issues:
1. Check the function documentation in `FUNCTION_CATALOG.md`
2. Review the original source files for usage examples
3. Refer to the associated publication for methodological details

## Citation

If you use this framework in your research, please cite the associated paper:

> [Placeholder for Paper Citation]

