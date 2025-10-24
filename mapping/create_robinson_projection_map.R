# ==============================================================================
# Function: create_robinson_projection_map
# ==============================================================================
# Original name: plot_robin_map
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#
# ==============================================================================

create_robinson_projection_map <- function(data,
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
                          legend_position = "bottom") {
  
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
    # Create aesthetic with fill variable - using aes() with .data pronoun for modern ggplot2
    p <- p + geom_sf(data = data_robin,
                     aes(fill = .data[[fill_var]]),
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
          barwidth = 12,  # Increased width for horizontal layout
          barheight = 0.5,  # Reduced height for horizontal bar
          title.position = "top",
          title.hjust = 0.5,
          direction = "horizontal",  # Make legend horizontal
          label.position = "bottom"  # Labels below the bar
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
      legend.position = legend_position,  # Now accepts "bottom" as string
      legend.background = element_rect(fill = "white", color = NA),
      legend.box = "horizontal",
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.title = element_text(size = 6, face = "bold"),
      legend.text = element_text(size = 5)
    )
  
  return(p)
}
