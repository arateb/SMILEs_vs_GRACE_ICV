# ==============================================================================
# Function: create_amplitude_variance_figure
# ==============================================================================
# Original name: make_figure1# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 1: GLOBAL TWS AMPLITUDE AND VARIANCE PATTERNS
# =============================================================================

#
# ==============================================================================

create_amplitude_variance_figure <- function(amplsd_aug, basins_sf) {
  cat("Creating Figure 1: Global TWS Patterns...\n")
  # Prepare data
  fig1_data <- amplsd_aug %>%
    mutate(
      amp_ratio = amp_grace / median_amp,
      sd_ratio = sd_grace / median_sd)
  
  # Get world and transform
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = "+proj=robin")
  
  basins_robin <- basins_sf %>% st_transform(crs = "+proj=robin")
  
  # Spatial data for CESM2
  spatial_cesm <- basins_robin %>%
    left_join(filter(fig1_data, model_name == "CESM2"), by = "bd_ID")
  
  # Spatial data for IPSL
  spatial_ipsl <- basins_robin %>%
    left_join(filter(fig1_data, model_name == "IPSL"), by = "bd_ID")
  
  # Panel A: GRACE Amplitude
  p1a <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_cesm, aes(fill = amp_grace), color = "grey40", size = 0.1) +
    scale_fill_gradientn(colors = earth_colors$amplitude, name = "Amp\n(mm)",
                        limits = c(0, 200), oob = squish) +
    coord_sf(datum = NA) +
    labs(title = "a. Observed amplitude (GRACE)") +
    theme_nature_map()
  
  # Panel B: GRACE Variance
  p1b <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_cesm, aes(fill = sd_grace), color = "grey40", size = 0.1) +
    scale_fill_gradientn(colors = earth_colors$water, name = "Var\n(mm)",
                        limits = c(0, 50), oob = squish) +
    coord_sf(datum = NA) +
    labs(title = "b. Observed variance (GRACE)") +
    theme_nature_map()
  
  # Panel C: CESM2 Amplitude Ratio
  p1c <- library(scales)

# Define a diverging color palette centered at ratio = 1
ratio_colors <- c(
  "#67a9cf",  # blue (underestimate)
  "#d1e5f0",  # light blue
  "#f7f7f7",  # near-white (neutral at 1.0)
  "#fddbc7",  # light orange
  "#ef8a62"   # orange-red (overestimate)
)

p1c<-ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_cesm, aes(fill = amp_ratio), color = "grey40", size = 0.1) +
    scale_fill_gradientn(
        colors = ratio_colors,
        name = "Ratio",
        limits = c(0.4, 1.8),
        values = scales::rescale(c(0.4, 0.7, 1.0, 1.3, 1.8)),  # center at 1.0
        breaks = c(0.5, 0.75, 1.0, 1.25, 1.5),
        oob = squish
    ) +
    coord_sf(datum = NA) +
    labs(title = "c. CESM2 amplitude (median) ratio") +
    theme_nature_map()
  # Panel D: CESM2 Variance Ratio
p1d <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
  geom_sf(data = spatial_cesm, aes(fill = sd_ratio), color = "grey40", size = 0.1) +
  scale_fill_gradientn(
    colors = ratio_colors,
    name = "Ratio",
    limits = c(0.4, 1.8),
    values = scales::rescale(c(0.4, 0.7, 1.0, 1.3, 1.8)),
    breaks = c(0.5, 0.75, 1.0, 1.25, 1.5),
    oob = squish
  ) +
  coord_sf(datum = NA) +
  labs(title = "d. CESM2 variance (median) ratio") +
  theme_nature_map()

# Panel E: IPSL Amplitude Ratio
p1e <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
  geom_sf(data = spatial_ipsl, aes(fill = amp_ratio), color = "grey40", size = 0.1) +
  scale_fill_gradientn(
    colors = ratio_colors,
    name = "Ratio",
    limits = c(0.4, 1.8),
    values = scales::rescale(c(0.4, 0.7, 1.0, 1.3, 1.8)),
    breaks = c(0.5, 0.75, 1.0, 1.25, 1.5),
    oob = squish
  ) +
  coord_sf(datum = NA) +
  labs(title = "e. IPSL amplitude (median) ratio") +
  theme_nature_map()

# Panel F: IPSL Variance Ratio
p1f <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
  geom_sf(data = spatial_ipsl, aes(fill = sd_ratio), color = "grey40", size = 0.1) +
  scale_fill_gradientn(
    colors = ratio_colors,
    name = "Ratio",
    limits = c(0.4, 1.8),
    values = scales::rescale(c(0.4, 0.7, 1.0, 1.3, 1.8)),
    breaks = c(0.5, 0.75, 1.0, 1.25, 1.5),
    oob = squish
  ) +
  coord_sf(datum = NA) +
  labs(title = "f. IPSL variance (median) ratio") +
  theme_nature_map()
  # Combine
  fig1 <- (p1a + p1b) / (p1c + p1d) / (p1e + p1f)
  
  cat("  ✓ Figure 1 complete\n")
  return(fig1)
}
