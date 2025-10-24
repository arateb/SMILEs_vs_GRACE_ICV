# TWS Analysis Framework: GRACE vs. Climate Model Ensembles

This repository provides a comprehensive R-based analysis framework for comparing Terrestrial Water Storage (TWS) data from the GRACE satellite mission with outputs from large climate model ensembles, specifically CESM2 and IPSL. The code is designed to reproduce the analyses and figures for the associated research paper on climate model fidelity in representing TWS variability.

## Associated Publication

This code supports the findings of the following paper. If you use this code in your research, please cite:

> [Placeholder for Paper Citation: Authors, Title, Journal, Year, DOI]

## Overview

The framework is built to perform a multi-faceted comparison between observational data and climate model simulations, focusing on:

1.  **Amplitude & Variance:** Assesses how well models capture the magnitude and spread of TWS variability compared to GRACE observations.
2.  **Hydroclimatic Extremes:** Evaluates whether the full range of model ensemble members encompasses the droughts and pluvials observed by GRACE.
3.  **Spectral Characteristics:** Uses wavelet analysis to compare the dominant timescales of variability between GRACE and the model ensembles.

The code is structured to facilitate basin-by-basin analysis and generate publication-quality figures consistent with the style of *Nature* journals.

## Repository Structure

The R code has been refactored from monolithic scripts into a modular, function-oriented structure. All functions are organized into categories within this main directory.

```
refactored_r_code/
├── core_analysis/         # Main functions for running the TWS analysis
├── data_processing/       # Functions for data extraction, merging, and formatting
├── ensemble_metrics/      # Functions for calculating spread and statistical metrics
├── extreme_analysis/      # Functions dedicated to extreme event analysis
├── figures/               # High-level functions to generate specific manuscript figures
├── interpretation/        # Functions for generating qualitative interpretations of results
├── mapping/               # Functions for creating and handling geographic maps
├── utilities/             # Helper and utility functions
├── visualization/         # Functions for plotting and theming (e.g., Nature style)
├── wavelet_analysis/      # Functions for performing and analyzing wavelet transforms
├── FUNCTION_CATALOG.md    # A complete list of all functions, their original names, and source files
└── README.md              # This file
```

## Dependencies

This analysis code is written in R. The following packages are required:

*   `tidyverse` (includes `ggplot2`, `dplyr`)
*   `sf` (for spatial data handling)
*   `viridis` (for color palettes)
*   `grid` & `gridExtra` (for arranging plots)
*   `scales` (for plot scaling)

You can install all required packages using:

```R
install.packages(c("tidyverse", "sf", "viridis", "gridExtra", "scales"))
```

## Usage

The general workflow for using this framework is as follows:

1.  **Load Data:** Load your pre-processed GRACE and climate model ensemble data for the basins of interest.
2.  **Run Analysis:** Use the `analyze_basin_tws()` function for a single basin or `analyze_multiple_basins_parallel()` to run the analysis across many basins. These functions serve as the main entry points and will execute the full suite of amplitude, extreme, and wavelet analyses.
3.  **Extract Results:** Use the `extract_all_analysis_results()` function to collate the results from the analysis runs into tidy data frames.
4.  **Generate Figures:** Use the functions in the `figures/` and `visualization/` directories to create the manuscript figures from the processed results.

## Function Catalog

A detailed catalog of every function in this repository is available in `FUNCTION_CATALOG.md`. This file provides a cross-reference between the original function names and the new, more descriptive names, and indicates which source file they originated from.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

