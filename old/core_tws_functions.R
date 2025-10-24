# =============================================================================
# CORE FUNCTIONS FOR TWS ANALYSIS RESULTS PROCESSING AND VISUALIZATION
# =============================================================================
# These functions process all_results from TWS analysis and create Nature-compliant figures
# Author: Functions extracted and corrected from original analysis code
# Requirements: ggplot2, dplyr, sf, viridis, grid, gridExtra, scales
# =============================================================================

library(ggplot2)
library(dplyr)
library(sf)
library(viridis)
library(grid)
library(gridExtra)
library(scales)

# =============================================================================
# PART 1: DATA EXTRACTION AND MERGING FUNCTIONS
# =============================================================================

#' Extract and merge clean data - MAIN FUNCTION FOR PROCESSING RESULTS
#' Removes basins with missing data to prevent NA issues in plots
extract_and_merge_clean <- function(all_results, shp, result_type) {
  
  cat(sprintf("\nExtracting %s...\n", result_type))
  
  # Extract specified result type from all basins
  extracted_data <- lapply(seq_along(all_results), function(i) {
    result <- all_results[[i]]
    
    # Skip NULL or error results
    if (is.null(result) || !is.null(result$error)) {
      return(NULL)
    }
    
    # Extract based on result type
    data <- switch(result_type,
      # Amplitude analysis
      "amplitude_metrics" = result$amplitude_metrics,
      "amplitude_summary" = result$amplitude_summary,
      "amplitude_interpretation" = result$amplitude_interpretation,
      
      # Extremes analysis
      "extremes_model" = result$extremes_model,
      "extremes_basin" = result$extremes_basin,
      "extremes_comparison" = result$extremes_comparison,
      "extremes_summary" = result$extremes_summary,
      
      # Wavelet analysis
      "wave_full" = result$wave_full,
      "wave_q1_summary" = result$wave_q1_summary,
      "wave_q1_modes" = result$wave_q1_modes,
      "wave_q1_interpretation" = result$wave_q1_interpretation,
      "wave_q2_timescale" = result$wave_q2_timescale,
      "wave_q3_consistency" = result$wave_q3_consistency,
      "wave_q4_grace_dominant" = result$wave_q4_grace_dominant,
      "wave_q4_model_results" = result$wave_q4_model_results,
      
      # Metadata
      "metadata" = result$metadata,
      
      # Default - try to access by name
      result[[result_type]]
    )
    
    # Return only valid data frames with rows
    if (!is.null(data) && is.data.frame(data) && nrow(data) > 0) {
      return(data)
    }
    return(NULL)
  })
  
  # Remove NULL results
  extracted_data <- extracted_data[!sapply(extracted_data, is.null)]
  
  # Handle case with no valid data
  if (length(extracted_data) == 0) {
    warning(paste("No valid", result_type, "data found"))
    return(list(
      merged_shp = shp[0,],  # Empty sf object
      combined_data = data.frame(),
      n_basins = 0,
      merge_key = NA
    ))
  }
  
  # Combine all valid data frames
  combined_data <- bind_rows(extracted_data)
  
  cat(sprintf("Combined data: %d rows\n", nrow(combined_data)))
  if ("bd_id" %in% names(combined_data)) {
    cat(sprintf("Unique basins in combined data: %d\n", 
                length(unique(combined_data$bd_id))))
  }
  
  # Merge with shapefile - INNER JOIN to remove NAs
  merge_key <- NULL
  merged_shp <- NULL
  
  if ("bd_id" %in% names(combined_data)) {
    merge_key <- "bd_id"
    merged_shp <- shp %>%
      inner_join(combined_data, by = c("bd_ID" = "bd_id"))
  } else if ("ID" %in% names(combined_data)) {
    merge_key <- "ID"
    merged_shp <- shp %>%
      inner_join(combined_data, by = c("OBJECTID" = "ID"))
  } else {
    stop("No valid merge key found in data!")
  }
  
  # Report results
  n_unique_basins <- length(unique(merged_shp$bd_ID))
  cat(sprintf("Merged shapefile: %d rows, %d unique basins\n", 
              nrow(merged_shp), n_unique_basins))
  cat(sprintf("Excluded %d basins with missing data\n", 
              nrow(shp) - n_unique_basins))
  
  # Return both merged shapefile and combined data
  return(list(
    merged_shp = merged_shp,
    combined_data = combined_data,
    n_basins = n_unique_basins,
    merge_key = merge_key
  ))
}

#' Extract all result types at once
extract_all_results <- function(all_results, shp) {
  
  result_types <- c(
    "amplitude_metrics", "amplitude_summary", "amplitude_interpretation",
    "extremes_model", "extremes_basin", "extremes_comparison", "extremes_summary",
    "wave_full", "wave_q1_summary", "wave_q1_modes", "wave_q1_interpretation",
    "wave_q2_timescale", "wave_q3_consistency", "wave_q4_grace_dominant", 
    "wave_q4_model_results", "metadata"
  )
  
  all_extractions <- list()
  
  for (type in result_types) {
    cat(sprintf("\n--- Processing %s ---\n", type))
    
    tryCatch({
      result <- extract_and_merge_clean(all_results, shp, type)
      all_extractions[[type]] <- result
    }, error = function(e) {
      cat(sprintf("Error with %s: %s\n", type, e$message))
      all_extractions[[type]] <- NULL
    })
  }
  
  return(all_extractions)
}

# =============================================================================
# PART 2: NATURE PUBLICATION THEME FUNCTIONS
# =============================================================================

# Nature specifications constants
NATURE_SPECS <- list(
  font_size = 6,  # 5-7pt range
  panel_label_size = 8,  # 8pt bold lowercase
  single_width = 89,  # mm
  double_width = 180,  # mm
  max_height = 240  # mm
)

# Nature color palette (Wong 2011 colorblind-friendly)
nature_colors <- c(
  "#0072B2",  # Blue
  "#D55E00",  # Vermillion
  "#009E73",  # Green
  "#CC79A7",  # Pink
  "#F0E442",  # Yellow
  "#56B4E9",  # Sky blue
  "#E69F00",  # Orange
  "#000000"   # Black
)

# Color schemes for maps
nature_color_schemes <- list(
  RdBu = c("#2166AC", "#F7F7F7", "#B2182B"),
  BrBG = c("#543005", "#F5F5F5", "#003C30"),
  PRGn = c("#762A83", "#F7F7F7", "#1B7837"),
  PiYG = c("#C51B7D", "#F7F7F7", "#4D9221"),
  Spectral = c("#5E4FA2", "#FFFFBF", "#9E0142"),
  Viridis = c("#440154", "#21908C", "#FDE725"),
  Cividis = c("#00204D", "#7C7B78", "#FFEA46")
)

#' Main Nature theme for plots
theme_nature <- function(base_size = NATURE_SPECS$font_size, base_family = "sans") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      # Panel
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Axes
      axis.text = element_text(size = base_size, colour = "black"),
      axis.title = element_text(size = base_size, colour = "black"),
      axis.ticks = element_line(colour = "black", linewidth = 0.25),
      axis.ticks.length = unit(0.1, "cm"),
      
      # Legend
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key = element_rect(fill = "white", colour = NA),
      legend.key.size = unit(0.7, "lines"),
      legend.text = element_text(size = base_size * 0.9),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.position = "top",
      
      # Plot
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title = element_text(size = base_size * 1.2, face = "bold", hjust = 0),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
    )
}

#' Theme for maps (no axes)
theme_nature_map <- function(base_size = NATURE_SPECS$font_size, base_family = "sans") {
  theme_nature(base_size, base_family) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 0.3),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")
    )
}

#' Add panel labels (a, b, c, etc.)
add_panel_label <- function(label, x = 0.02, y = 0.98) {
  annotation_custom(
    grob = textGrob(label, x = x, y = y, hjust = 0, vjust = 1,
                    gp = gpar(fontsize = NATURE_SPECS$panel_label_size, 
                             fontface = "bold"))
  )
}

#' Scale functions for Nature colors
scale_color_nature <- function(...) {
  discrete_scale("colour", "nature", palette = function(n) nature_colors[1:n], ...)
}

scale_fill_nature <- function(...) {
  discrete_scale("fill", "nature", palette = function(n) nature_colors[1:n], ...)
}

#' Get Nature figure dimensions
nature_figure_size <- function(width_type = "single") {
  mm_to_in <- 1/25.4
  
  sizes <- list(
    single = list(
      width = NATURE_SPECS$single_width * mm_to_in, 
      height = NATURE_SPECS$single_width * mm_to_in
    ),
    double = list(
      width = NATURE_SPECS$double_width * mm_to_in, 
      height = NATURE_SPECS$max_height * mm_to_in
    ),
    full_width = list(
      width = NATURE_SPECS$double_width * mm_to_in, 
      height = 170 * mm_to_in
    )
  )
  
  return(sizes[[width_type]])
}

#' Save figure with Nature specifications
save_nature_figure <- function(plot, filename, width_type = "single", dpi = 400, format = "pdf") {
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

# =============================================================================
# PART 3: MAP PROJECTION FUNCTIONS
# =============================================================================

#' Robinson projection CRS
robin_crs <- function() {
  st_crs("+proj=robin")
}

#' Get world background in Robinson projection
get_world_robin <- function(scale = "medium", 
                           fill = "grey95", 
                           colour = "grey70", 
                           linewidth = 0.15,
                           add_graticules = TRUE) {
  
  require(rnaturalearth)
  
  world <- ne_countries(scale = scale, returnclass = "sf")
  world_robin <- st_transform(world, robin_crs())
  
  geoms <- list()
  
  if (add_graticules) {
    grat <- st_graticule(lat = seq(-90, 90, 30), lon = seq(-180, 180, 60))
    grat_robin <- st_transform(grat, robin_crs())
    
    geoms$graticules <- geom_sf(data = grat_robin,
                                color = "grey85",
                                linewidth = 0.1,
                                alpha = 0.5)
  }
  
  geoms$world <- geom_sf(data = world_robin,
                        fill = fill,
                        colour = colour,
                        linewidth = linewidth)
  
  return(list(data = world_robin, geoms = geoms))
}

#' Main Robinson projection map function
plot_robin_map <- function(data,
                          fill_var = NULL,
                          scale_limits = NULL,
                          scale_midpoint = NULL,
                          scale_colors = c("darkblue", "white", "darkred"),
                          scale_breaks = NULL,
                          scale_labels = NULL,
                          scale_name = NULL,
                          world_fill = "white",
                          world_colour = "grey40",
                          data_color = NA,
                          data_size = 0.1,
                          add_graticules = TRUE,
                          legend_position = c(0.1, 0.3)) {
  
  # Get world background
  world_bg <- get_world_robin(fill = world_fill, 
                              colour = world_colour, 
                              add_graticules = add_graticules)
  
  # Transform data to Robinson
  data_robin <- st_transform(data, robin_crs())
  
  # Build plot
  p <- ggplot()
  
  # Add graticules if requested
  if (add_graticules && !is.null(world_bg$geoms$graticules)) {
    p <- p + world_bg$geoms$graticules
  }
  
  # Add world background
  p <- p + world_bg$geoms$world
  
  # Add data layer
  if (!is.null(fill_var)) {
    # Create aesthetic with fill variable
    p <- p + geom_sf(data = data_robin,
                     aes_string(fill = fill_var),
                     color = data_color,
                     size = data_size)
    
    # Add color scale if provided
    if (!is.null(scale_limits) && !is.null(scale_midpoint)) {
      p <- p + scale_fill_gradient2(
        low = scale_colors[1],
        mid = scale_colors[2],
        high = scale_colors[3],
        midpoint = scale_midpoint,
        limits = scale_limits,
        oob = scales::squish,
        breaks = scale_breaks,
        labels = scale_labels,
        name = scale_name,
        guide = guide_colorbar(
          barwidth = 0.7,
          barheight = 10,
          title.position = "top",
          title.hjust = 0.5
        )
      )
    }
  } else {
    p <- p + geom_sf(data = data_robin,
                     color = data_color,
                     size = data_size)
  }
  
  # Apply theme and coordinate system
  p <- p + 
    coord_sf(crs = robin_crs(), expand = FALSE) +
    theme_nature_map() +
    theme(
      legend.position = legend_position,
      legend.background = element_rect(fill = "white", color = NA)
    )
  
  return(p)
}

#' Create CESM2 Robinson map with specific styling
plot_cesm_robin <- function(cesm_data,
                           ratio_limits = c(0.5, 2),
                           ratio_midpoint = 1,
                           title = "CESM2: SD ratio",
                           panel_label = NULL) {
  
  p <- plot_robin_map(
    data = cesm_data,
    fill_var = "ratio",
    scale_limits = ratio_limits,
    scale_midpoint = ratio_midpoint,
    scale_colors = nature_color_schemes$RdBu,
    scale_name = "Obs/Model",
    scale_breaks = c(0.5, 0.75, 1, 1.5, 2),
    scale_labels = c("0.5", "0.75", "1", "1.5", "2"),
    world_fill = "white",
    world_colour = "grey40"
  ) +
    labs(title = title) +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
  
  if (!is.null(panel_label)) {
    p <- p + add_panel_label(panel_label)
  }
  
  return(p)
}

# =============================================================================
# PART 4: FIGURE CREATION FUNCTIONS FOR YOUR RESULTS
# =============================================================================

#' FIGURE 1: Global Variability Maps
create_figure1_clean <- function(all_results, shp) {
  
  cat("\nFIGURE 1: Global Variability Assessment\n")
  
  # Extract amplitude data - removes NAs automatically
  amp_result <- extract_and_merge_clean(all_results, shp, "amplitude_summary")
  amp_data <- amp_result$merged_shp
  
  if (nrow(amp_data) == 0) {
    warning("No amplitude data available")
    return(NULL)
  }
  
  # Separate by model
  cesm_data <- amp_data %>% filter(source == "CESM2")
  ipsl_data <- amp_data %>% filter(source == "IPSL")
  
  # Panel A: Observed variability
  p1a <- ggplot(cesm_data) +
    geom_sf(data = shp, fill = "grey90", color = "grey70", size = 0.1) +
    geom_sf(aes(fill = obs_sd), color = "grey50", size = 0.1) +
    scale_fill_viridis_c(
      name = expression(sigma[obs]~"(mm)"),
      option = "viridis",
      na.value = "transparent"
    ) +
    theme_nature_map() +
    add_panel_label("a")
  
  # Panel B: CESM2 model variability
  p1b <- ggplot(cesm_data) +
    geom_sf(data = shp, fill = "grey90", color = "grey70", size = 0.1) +
    geom_sf(aes(fill = model_sd), color = "grey50", size = 0.1) +
    scale_fill_viridis_c(
      name = expression(sigma[CESM2]~"(mm)"),
      option = "plasma",
      na.value = "transparent"
    ) +
    theme_nature_map() +
    add_panel_label("b")
  
  # Panel C: IPSL model variability
  p1c <- ggplot(ipsl_data) +
    geom_sf(data = shp, fill = "grey90", color = "grey70", size = 0.1) +
    geom_sf(aes(fill = model_sd), color = "grey50", size = 0.1) +
    scale_fill_viridis_c(
      name = expression(sigma[IPSL]~"(mm)"),
      option = "plasma",
      na.value = "transparent"
    ) +
    theme_nature_map() +
    add_panel_label("c")
  
  # Panel D: Ratio (Obs/Model)
  p1d <- ggplot(cesm_data) +
    geom_sf(data = shp, fill = "grey90", color = "grey70", size = 0.1) +
    geom_sf(aes(fill = ratio), color = "grey50", size = 0.1) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "#F7F7F7",
      high = "#B2182B",
      midpoint = 1,
      limits = c(0.5, 2),
      oob = scales::squish,
      name = "Obs/Model"
    ) +
    theme_nature_map() +
    add_panel_label("d")
  
  # Combine all panels
  combined <- grid.arrange(p1a, p1b, p1c, p1d, ncol = 2)
  
  return(combined)
}

#' Generate all maps with clean data (no NAs)
generate_all_maps_clean <- function(all_results, shp) {
  
  cat("\n========================================\n")
  cat("GENERATING CLEAN MAPS (NO MISSING DATA)\n")
  cat("========================================\n")
  
  # Test data extraction
  test_result <- extract_and_merge_clean(all_results, shp, "amplitude_summary")
  cat(sprintf("\nBasins with complete data: %d/%d\n", 
              test_result$n_basins, nrow(shp)))
  
  # Generate Figure 1
  fig1 <- create_figure1_clean(all_results, shp)
  
  # Save if successful
  if (!is.null(fig1)) {
    save_nature_figure(fig1, "figure1_variability.pdf", width_type = "double")
  }
  
  # Return test data for verification
  return(test_result$merged_shp)
}

# =============================================================================
# PART 5: USAGE INSTRUCTIONS
# =============================================================================

# HOW TO USE THESE FUNCTIONS:
# 
# 1. Load your analysis results and shapefile:
#    all_results <- readRDS("your_results.rds")
#    shp <- st_read("your_shapefile.shp")
#
# 2. Extract specific result type:
#    amplitude_data <- extract_and_merge_clean(all_results, shp, "amplitude_summary")
#
# 3. Extract all result types:
#    all_extractions <- extract_all_results(all_results, shp)
#
# 4. Create Figure 1 (variability maps):
#    fig1 <- create_figure1_clean(all_results, shp)
#    save_nature_figure(fig1, "figure1.pdf", width_type = "double")
#
# 5. Generate all maps at once:
#    test_data <- generate_all_maps_clean(all_results, shp)
#
# 6. Create custom CESM2 map:
#    cesm_result <- extract_and_merge_clean(all_results, shp, "amplitude_summary")
#    cesm_data <- cesm_result$merged_shp %>% filter(source == "CESM2")
#    p <- plot_cesm_robin(cesm_data)
#    save_nature_figure(p, "cesm_map.pdf", width_type = "single")

# =============================================================================
# END OF FILE
# =============================================================================