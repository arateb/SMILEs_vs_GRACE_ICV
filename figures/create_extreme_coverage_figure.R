# ==============================================================================
# Function: create_extreme_coverage_figure
# ==============================================================================
# Original name: make_figure3# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 3: EXTREME COVERAGE
# =============================================================================
#
# ==============================================================================

create_extreme_coverage_figure <- function(amplsd_aug, basins_sf) {
  
  cat("Creating Figure 3: Extreme Coverage...\n")
  
  # Prepare data
  fig3_data <- amplsd_aug %>%
    mutate(
      drought_covered = min_grace >= q5_min & min_grace <= q95_min,
      pluvial_covered = max_grace >= q5_max & max_grace <= q95_max,
      drought_severity = ifelse(!drought_covered & min_grace < q5_min,
                                (q5_min - min_grace) / IQR_min, 0),
      pluvial_severity = ifelse(!pluvial_covered & max_grace > q95_max,
                                (max_grace - q95_max) / IQR_max, 0),
      max_severity = pmax(drought_severity, pluvial_severity),
      coverage_class = case_when(
        !drought_covered & !pluvial_covered ~ "Both uncovered",
        !drought_covered ~ "Drought uncovered",
        !pluvial_covered ~ "Pluvial uncovered",
        TRUE ~ "Both covered"
      ),
      coverage_class = factor(coverage_class, 
                             levels = c("Both covered", "Drought uncovered", 
                                      "Pluvial uncovered", "Both uncovered"))
    )
  
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = "+proj=robin")
  basins_robin <- basins_sf %>% st_transform(crs = "+proj=robin")
  
  spatial_cesm <- basins_robin %>%
    left_join(filter(fig3_data, model_name == "CESM2"), by = "bd_ID")
  
  spatial_ipsl <- basins_robin %>%
    left_join(filter(fig3_data, model_name == "IPSL"), by = "bd_ID")
  
  # SOFTER, LIGHTER COLOR SCHEMES
  coverage_colors <- c(
    "Both covered" = "#31613dff",        # soft green
    "Drought uncovered" = "#d5b309ff",   # soft orange
    "Pluvial uncovered" = "#1599e0ff",   # soft blue
    "Both uncovered" = "#6c6a69ff"       # soft red
  )
  
  # Lighter severity gradient
  severity_colors <- c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", 
                      "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")
  
  # Softer model colors
  model_colors <- c("CESM2" = "#6baed6", "IPSL" = "#fc8d59")
  
  # Panel A: CESM2 Coverage
  p3a <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_cesm, aes(fill = coverage_class), color = "grey50", size = 0.1) +
    scale_fill_manual(values = coverage_colors, name = "Coverage") +
    coord_sf(datum = NA) +
    labs(title = "a. CESM2 coverage") +
    theme_nature_map()
  
  # Panel B: IPSL Coverage
  p3b <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = spatial_ipsl, aes(fill = coverage_class), color = "grey50", size = 0.1) +
    scale_fill_manual(values = coverage_colors, name = "Coverage") +
    coord_sf(datum = NA) +
    labs(title = "b. IPSL coverage") +
    theme_nature_map()
  
  # Panel C: CESM2 Severity
  p3c <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = filter(spatial_cesm, max_severity > 0), 
            aes(fill = max_severity), color = "grey50", size = 0.1) +
    scale_fill_gradientn(
      colors = severity_colors,
      name = "Severity\n(IQR)",
      limits = c(0, 5),
      breaks = seq(0, 5, 1),
      oob = squish
    ) +
    coord_sf(datum = NA) +
    labs(title = "c. CESM2 severity") +
    theme_nature_map()
  
  # Panel D: IPSL Severity
  p3d <- ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", size = 0.1) +
    geom_sf(data = filter(spatial_ipsl, max_severity > 0), 
            aes(fill = max_severity), color = "grey50", size = 0.1) +
    scale_fill_gradientn(
      colors = severity_colors,
      name = "Severity\n(IQR)",
      limits = c(0, 5),
      breaks = seq(0, 5, 1),
      oob = squish
    ) +
    coord_sf(datum = NA) +
    labs(title = "d. IPSL severity") +
    theme_nature_map()
  severity_med <- severity_long %>%
  group_by(type, model_name) %>%
  summarise(median_sev = median(severity), .groups = "drop")
  # Panel E: Severity violin (SOFTER)
  severity_long <- fig3_data %>%
    filter(max_severity > 0) %>%
    dplyr::select(bd_ID, model_name, drought_severity, pluvial_severity) %>%
    pivot_longer(cols = c(drought_severity, pluvial_severity),
                 names_to = "type", values_to = "severity") %>%
    filter(severity > 0) %>%
    mutate(
      type = ifelse(type == "drought_severity", "Drought", "Pluvial"),
      type = factor(type, levels = c("Drought", "Pluvial"))
    )
  
  p3e <- ggplot(severity_long, aes(x = severity, fill = model_name, color = model_name)) +
  geom_density(alpha = 0.5, size = 0.6, adjust = 1.5) +
  scale_fill_manual(values = model_colors, name = "Model") +
  scale_color_manual(values = model_colors, name = "Model") +
  scale_x_continuous(
    limits = c(0, 5),
    breaks = seq(0, 5, 1),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    breaks = pretty_breaks(n = 4)
  ) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  labs(
    title = "e. Severity distribution",
    x = "Severity (IQR units)",
    y = "Density"
  ) +
  theme_nature() +
  theme(
    panel.grid.major.x = element_line(color = "grey90", size = 0.25),
    panel.grid.major.y = element_line(color = "grey90", size = 0.25),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(size = 9, face = "plain", hjust = 0),
    axis.ticks = element_line(color = "grey30", size = 0.3),
    axis.ticks.length = unit(2, "pt"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5)
  )
  # Panel F: Summary (SOFTER)
  summary_stats <- fig3_data %>%
    group_by(model_name) %>%
    summarise(
      Drought = mean(!drought_covered) * 100,
      Pluvial = mean(!pluvial_covered) * 100,
      Both = mean(!drought_covered & !pluvial_covered) * 100,
      .groups = "drop"
    ) %>%
    pivot_longer(cols = -model_name, names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, levels = c("Drought", "Pluvial", "Both")))
  
  p3f <- ggplot(summary_stats, aes(x = metric, y = value, fill = model_name)) +
  geom_col(position = position_dodge(width = 0.75), alpha = 0.7, 
           width = 0.65, color = "grey60", size = 0.3) +
  geom_text(aes(label = sprintf("%.1f%%", value)),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3, color = "grey20", fontface = "plain") +
  scale_fill_manual(values = model_colors, name = "Model") +
  scale_y_continuous(
    limits = c(0, max(summary_stats$value) * 1.15),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    title = "f. Coverage summary",
    x = NULL,
    y = "Uncovered basins (%)"
  ) +
  theme_nature() +
  theme(
    panel.grid.major.y = element_line(color = "grey90", size = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "grey30", size = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5)
  )
  fig3 <- (p3a + p3b) / (p3c + p3d) / (p3e + p3f)
  
  cat("  ✓ Figure 3 complete\n")
  return(fig3)
}
