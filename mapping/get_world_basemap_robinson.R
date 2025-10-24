# ==============================================================================
# Function: get_world_basemap_robinson
# ==============================================================================
# Original name: get_world_robin
# Source file: SourceNaturePlots.R
# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
#' Get world background in Robinson projection
#
# ==============================================================================

get_world_basemap_robinson <- function(scale = "medium", 
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
