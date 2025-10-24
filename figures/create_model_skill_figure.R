# ==============================================================================
# Function: create_model_skill_figure
# ==============================================================================
# Original name: make_figure9# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 9: EXTREMAL NETWORK DEGREE (if network_results provided)
# =============================================================================

#
# ==============================================================================

create_model_skill_figure <- function(network_results, basins_sf) {
  
  cat("Creating Figure 9: Extremal Networks...\n")
  
  if (is.null(network_results)) {
    cat("  ⚠ Skipping Figure 9: No network data provided\n")
    return(NULL)
  }
  
  # Calculate node degree
  cesm_degree <- bind_rows(
    network_results$cesm$edges %>% count(basin_i, name = "degree") %>% rename(OBJECTID = basin_i),
    network_results$cesm$edges %>% count(basin_j, name = "degree") %>% rename(OBJECTID = basin_j)
  ) %>%
    group_by(OBJECTID) %>%
    summarise(degree = sum(degree), .groups = "drop")
  
  ipsl_degree <- bind_rows(
    network_results$ipsl$edges %>% count(basin_i, name = "degree") %>% rename(OBJECTID = basin_i),
    network_results$ipsl$edges %>% count(basin_j, name = "degree") %>% rename(OBJECTID = basin_j)
  ) %>%
    group_by(OBJECTID) %>%
    summarise(degree = sum(degree), .groups = "drop")
  
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = "+proj=robin")
  basins_robin <- basins_sf %>% st_transform(crs = "+proj=robin")
  
  spatial_cesm <- basins_robin %>%
    left_join(cesm_degree, by = "OBJECTID") %>%
    mutate(degree = replace_na(degree, 0))
  
  spatial_ipsl <- basins_robin %>%
    left_join(ipsl_degree, by = "OBJECTID") %>%
    mutate(degree = replace_na(degree, 0))
  
  # Panel A: CESM2 Network
  p9a <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = filter(spatial_cesm, degree > 0), 
            aes(fill = degree), color = "grey40", size = 0.1) +
    scale_fill_gradientn(colors = earth_colors$amplitude, name = "Degree",
                        trans = "log1p") +
    coord_sf(datum = NA) +
    labs(title = "a. CESM2 network degree") +
    theme_nature_map()
  
  # Panel B: IPSL Network
  p9b <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = filter(spatial_ipsl, degree > 0), 
            aes(fill = degree), color = "grey40", size = 0.1) +
    scale_fill_gradientn(colors = earth_colors$amplitude, name = "Degree",
                        trans = "log1p") +
    coord_sf(datum = NA) +
    labs(title = "b. IPSL network degree") +
    theme_nature_map()
  
  # Panel C: Degree distribution
  degree_comp <- bind_rows(
    cesm_degree %>% mutate(model = "CESM2"),
    ipsl_degree %>% mutate(model = "IPSL")
  )
  
  p9c <- ggplot(degree_comp, aes(x = degree, fill = model)) +
    geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
    scale_fill_manual(values = earth_colors$models) +
    scale_x_continuous(trans = "log1p") +
    labs(title = "c. Degree distribution", x = "Network degree", y = "Count") +
    theme_nature()
  
  # Panel D: Summary stats
  net_stats <- bind_rows(
    data.frame(model = "CESM2", 
               edges = nrow(network_results$cesm$edges),
               mean_degree = mean(cesm_degree$degree)),
    data.frame(model = "IPSL",
               edges = nrow(network_results$ipsl$edges),
               mean_degree = mean(ipsl_degree$degree))
  ) %>%
    pivot_longer(cols = -model, names_to = "metric", values_to = "value")
  
  p9d <- ggplot(net_stats, aes(x = metric, y = value, fill = model)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = earth_colors$models) +
    labs(title = "d. Network statistics", x = NULL, y = "Value") +
    theme_nature()
  
  fig9 <- (p9a + p9b) / (p9c + p9d)
  
  cat("  ✓ Figure 9 complete\n")
  return(fig9)
}
