# Function Catalog

This document provides a complete reference to all functions in the TWS Analysis Framework, organized by category.

## Overview

The framework contains 48 functions organized into 10 categories. Each function has been refactored from the original codebase with improved naming conventions and comprehensive documentation.


## Core Analysis

Location: `core_analysis/`

### `analyze_basin_tws()`

**Original name:** `analyze_one`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

Purpose: Comprehensive TWS analysis comparing GRACE observations withCESM2 and IPSL climate model ensemblesAnalyses performed:

**File:** `core_analysis/analyze_basin_tws.R`

### `analyze_multiple_basins_parallel()`

**Original name:** `analyze_multiple_basins`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `core_analysis/analyze_multiple_basins_parallel.R`

### `extract_result_by_type()`

**Original name:** `extract_analysis_type`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `core_analysis/extract_result_by_type.R`


## Data Processing

Location: `data_processing/`

### `attach_basin_attributes()`

**Original name:** `attach_attrs`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `data_processing/attach_basin_attributes.R`

### `convert_interpretation_to_dataframe()`

**Original name:** `interpretation_to_df`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `data_processing/convert_interpretation_to_dataframe.R`

### `extract_all_analysis_results()`

**Original name:** `extract_all_results`  
**Source:** SourceNaturePlots.R  

**File:** `data_processing/extract_all_analysis_results.R`

### `extract_and_merge_spatial_data()`

**Original name:** `extract_and_merge_clean`  
**Source:** SourceNaturePlots.R  

**File:** `data_processing/extract_and_merge_spatial_data.R`


## Ensemble Metrics

Location: `ensemble_metrics/`

### `calculate_ensemble_spread_metrics()`

**Original name:** `basic_spread_metrics`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `ensemble_metrics/calculate_ensemble_spread_metrics.R`

### `calculate_row_metrics()`

**Original name:** `row_metric`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `ensemble_metrics/calculate_row_metrics.R`

### `summarize_vector_statistics()`

**Original name:** `summarise_vec3`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

Purpose: Assess if climate model ensembles reproduce observed variabilityKey metrics: SD, amplitude (max-min), IQR, MAD, 95-5 percentile rangeCriterion: Observations should fall within 10-90% of ensemble distribution

**File:** `ensemble_metrics/summarize_vector_statistics.R`


## Extreme Analysis

Location: `extreme_analysis/`

### `analyze_extreme_event_coverage()`

**Original name:** `extreme_coverage_analysis`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `extreme_analysis/analyze_extreme_event_coverage.R`

### `interpret_extreme_analysis()`

**Original name:** `interpret_extreme_results`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `extreme_analysis/interpret_extreme_analysis.R`

### `process_extreme_results()`

**Original name:** `process_extreme_analysis`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `extreme_analysis/process_extreme_results.R`


## Figures

Location: `figures/`

### `create_amplitude_variance_figure()`

**Original name:** `make_figure1`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_amplitude_variance_figure.R`

### `create_basin_summary_figure()`

**Original name:** `make_figure5`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_basin_summary_figure.R`

### `create_ensemble_spread_figure()`

**Original name:** `make_figure7`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_ensemble_spread_figure.R`

### `create_extreme_coverage_figure()`

**Original name:** `make_figure3`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_extreme_coverage_figure.R`

### `create_model_comparison_figure()`

**Original name:** `make_figure2`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_model_comparison_figure.R`

### `create_model_skill_figure()`

**Original name:** `make_figure9`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_model_skill_figure.R`

### `create_spectral_analysis_figure()`

**Original name:** `make_figure4`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_spectral_analysis_figure.R`

### `create_temporal_patterns_figure()`

**Original name:** `make_figure6`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_temporal_patterns_figure.R`

### `create_wavelet_power_figure()`

**Original name:** `make_figure8`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/create_wavelet_power_figure.R`

### `generate_all_manuscript_figures()`

**Original name:** `make_all_figures`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `figures/generate_all_manuscript_figures.R`


## Interpretation

Location: `interpretation/`

### `interpret_model_performance()`

**Original name:** `create_interpretation`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `interpretation/interpret_model_performance.R`


## Mapping

Location: `mapping/`

### `create_main_analysis_figure()`

**Original name:** `create_figure1_clean`  
**Source:** SourceNaturePlots.R  

**File:** `mapping/create_main_analysis_figure.R`

### `create_robinson_projection_map()`

**Original name:** `plot_robin_map`  
**Source:** SourceNaturePlots.R  

**File:** `mapping/create_robinson_projection_map.R`

### `generate_all_spatial_maps()`

**Original name:** `generate_all_maps_clean`  
**Source:** SourceNaturePlots.R  

**File:** `mapping/generate_all_spatial_maps.R`

### `get_robinson_projection_crs()`

**Original name:** `robin_crs`  
**Source:** SourceNaturePlots.R  

**File:** `mapping/get_robinson_projection_crs.R`

### `get_world_basemap_robinson()`

**Original name:** `get_world_robin`  
**Source:** SourceNaturePlots.R  

**File:** `mapping/get_world_basemap_robinson.R`

### `plot_cesm_data_robinson()`

**Original name:** `plot_cesm_robin`  
**Source:** SourceNaturePlots.R  

**File:** `mapping/plot_cesm_data_robinson.R`


## Utilities

Location: `utilities/`

### `bin_continuous_variable()`

**Original name:** `bin_cut`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `utilities/bin_continuous_variable.R`

### `classify_model_data_ratio()`

**Original name:** `classify_ratio`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `utilities/classify_model_data_ratio.R`

### `safely_rename_column()`

**Original name:** `safe_rename`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `utilities/safely_rename_column.R`


## Visualization

Location: `visualization/`

### `add_figure_panel_label()`

**Original name:** `add_panel_label`  
**Source:** SourceNaturePlots.R  

**File:** `visualization/add_figure_panel_label.R`

### `apply_nature_color_scale()`

**Original name:** `scale_color_nature`  
**Source:** core_tws_functions.R  

**File:** `visualization/apply_nature_color_scale.R`

### `apply_nature_fill_scale()`

**Original name:** `scale_fill_nature`  
**Source:** core_tws_functions.R  

**File:** `visualization/apply_nature_fill_scale.R`

### `apply_nature_journal_theme()`

**Original name:** `theme_nature`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `visualization/apply_nature_journal_theme.R`

### `apply_nature_map_theme()`

**Original name:** `theme_nature_map`  
**Source:** Claude_18_Qs_results_amplitude_Extremes.R  

**File:** `visualization/apply_nature_map_theme.R`

### `get_nature_figure_dimensions()`

**Original name:** `nature_figure_size`  
**Source:** SourceNaturePlots.R  

**File:** `visualization/get_nature_figure_dimensions.R`

### `save_figure_nature_format()`

**Original name:** `save_nature_figure`  
**Source:** SourceNaturePlots.R  

**File:** `visualization/save_figure_nature_format.R`


## Wavelet Analysis

Location: `wavelet_analysis/`

### `assess_ensemble_spectral_consistency()`

**Original name:** `analyze_ensemble_consistency`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `wavelet_analysis/assess_ensemble_spectral_consistency.R`

### `classify_temporal_period()`

**Original name:** `classify_period`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `wavelet_analysis/classify_temporal_period.R`

### `evaluate_dominant_mode_reproduction()`

**Original name:** `analyze_dominant_mode_reproduction`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `wavelet_analysis/evaluate_dominant_mode_reproduction.R`

### `evaluate_timescale_representation()`

**Original name:** `analyze_timescale_representation`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `wavelet_analysis/evaluate_timescale_representation.R`

### `extract_top_spectral_periods()`

**Original name:** `get_top_periods_stl`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `wavelet_analysis/extract_top_spectral_periods.R`

### `identify_dominant_spectral_modes()`

**Original name:** `analyze_dominant_modes`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `wavelet_analysis/identify_dominant_spectral_modes.R`

### `perform_ensemble_wavelet_analysis()`

**Original name:** `ensemble_wavelet_analysis`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `wavelet_analysis/perform_ensemble_wavelet_analysis.R`

### `summarize_wavelet_analysis_results()`

**Original name:** `summarize_wavelet_answers`  
**Source:** F01_Functions_MMLEs_vs_GGFO_Jun2025.R  

**File:** `wavelet_analysis/summarize_wavelet_analysis_results.R`


## Usage Notes

- All functions follow consistent naming conventions using descriptive verbs and nouns
- Each function file includes comprehensive header documentation
- Functions are designed to be modular and can be sourced independently
- See README.md for workflow examples and usage patterns

## Function Dependencies

Many functions depend on others within the framework. Key dependencies include:

- **Core Analysis** functions call functions from most other categories
- **Figures** functions depend on **Visualization** and **Mapping** functions
- **Wavelet Analysis** functions work together as a pipeline
- **Data Processing** functions are used throughout the framework

## Contributing

When adding new functions:
1. Follow the existing naming conventions
2. Include comprehensive header documentation
3. Place in the appropriate category directory
4. Update this catalog
