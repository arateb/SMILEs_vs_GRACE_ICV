# ============================================================================
# FIGURE 19: Persistence Maps and Distributions
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. What is the spatial pattern of TWS persistence timescales globally?
#   2. How does low-frequency amplitude vary across basins?
#   3. How well do models reproduce observed persistence characteristics?
#   4. Is there a relationship between autocorrelation and persistence?
#
# Panels:
#   (a) Map: GRACE persistence timescale (tau_grace) in months
#   (b) Map: GRACE low-frequency amplitude (A_LF_grace) in mm
#   (c) Histogram: Persistence timescale comparison (GRACE vs models)
#   (d) Scatter: Lag-1 autocorrelation vs persistence timescale
#
# Output:
#   outputs/figs/fig19_persistence_maps.png (400 dpi)
#   outputs/figs/fig19_persistence_maps.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 19: PERSISTENCE MAPS AND DISTRIBUTIONS\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading persistence data...\n")
persistence <- readRDS("outputs/phase05_persistence_summary.rds")

cat("  Persistence data:", nrow(persistence), "basins\n\n")

# Load basin shapefile
basins_shp <- st_read("/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)
basins_shp <- st_simplify(basins_shp, dTolerance = 0.1, preserveTopology = TRUE)

# Load country borders
sf::sf_use_s2(FALSE)
world_borders <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
sf::sf_use_s2(TRUE)

# ============================================================================
# PANEL A: Map of GRACE Persistence Timescale
# ============================================================================

cat("Creating Panel A: Map of GRACE persistence timescale...\n")

# Merge with shapefile
basins_shp_tau <- merge(basins_shp,
                         persistence %>%
                           select(bd_ID = bd_id, tau_grace),
                         by = "bd_ID", all.x = TRUE)

# Create discrete persistence categories
basins_shp_tau <- basins_shp_tau %>%
  mutate(
    tau_category = cut(tau_grace,
                      breaks = c(0, 10, 20, 40, 60, 150),
                      labels = c("<10", "10-20", "20-40", "40-60", ">60"),
                      include.lowest = TRUE)
  )

panel_a <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_tau %>% filter(!is.na(tau_category)),
          aes(fill = tau_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Persistence\n(months)",
    values = c(
      "<10" = "#FEE5D9",      # Very light orange
      "10-20" = "#FCAE91",    # Light orange
      "20-40" = "#FB6A4A",    # Orange
      "40-60" = "#DE2D26",    # Dark orange
      ">60" = "#A50F15"       # Dark red
    ),
    drop = FALSE
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Map of Low-Frequency Amplitude
# ============================================================================

cat("Creating Panel B: Map of low-frequency amplitude...\n")

# Merge with shapefile
basins_shp_alf <- merge(basins_shp,
                         persistence %>%
                           select(bd_ID = bd_id, A_LF_grace),
                         by = "bd_ID", all.x = TRUE)

# Create discrete amplitude categories
basins_shp_alf <- basins_shp_alf %>%
  mutate(
    alf_category = cut(A_LF_grace,
                      breaks = c(0, 30, 60, 100, 150, 500),
                      labels = c("<30", "30-60", "60-100", "100-150", ">150"),
                      include.lowest = TRUE)
  )

panel_b <- ggplot() +
  geom_sf(data = world_borders, fill = NA, color = "grey70", linewidth = 0.2) +
  geom_sf(data = basins_shp_alf %>% filter(!is.na(alf_category)),
          aes(fill = alf_category),
          color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "LF amplitude\n(mm)",
    values = c(
      "<30" = "#EFF3FF",      # Very light blue
      "30-60" = "#BDD7E7",    # Light blue
      "60-100" = "#6BAED6",   # Medium blue
      "100-150" = "#3182BD",  # Dark blue
      ">150" = "#08519C"      # Very dark blue
    ),
    drop = FALSE
  ) +
  coord_sf(crs = "+proj=robin", xlim = c(-1.5e7, 1.5e7), ylim = c(-8e6, 8e6)) +
  theme_nature_map(base_size = 8) +
  theme(
    legend.position = c(0.08, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.height = unit(3.5, "mm"),
    legend.key.width = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5)
  ) +
  annotate("text", x = -1.4e7, y = 7.5e6, label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Persistence Histogram (GRACE vs Models)
# ============================================================================

cat("Creating Panel C: Persistence histogram...\n")

# Prepare data for histogram
tau_data <- bind_rows(
  persistence %>%
    filter(!is.na(tau_grace)) %>%
    select(tau = tau_grace) %>%
    mutate(source = "GRACE"),
  persistence %>%
    filter(!is.na(tau_p50_cesm)) %>%
    select(tau = tau_p50_cesm) %>%
    mutate(source = "CESM2"),
  persistence %>%
    filter(!is.na(tau_p50_ipsl)) %>%
    select(tau = tau_p50_ipsl) %>%
    mutate(source = "IPSL")
)

cat("  Tau ranges:\n")
cat("    GRACE: ", round(min(persistence$tau_grace, na.rm=TRUE), 1), " to ",
    round(max(persistence$tau_grace, na.rm=TRUE), 1), " months\n", sep="")
cat("    CESM2 median: ", round(min(persistence$tau_p50_cesm, na.rm=TRUE), 1), " to ",
    round(max(persistence$tau_p50_cesm, na.rm=TRUE), 1), " months\n", sep="")
cat("    IPSL median: ", round(min(persistence$tau_p50_ipsl, na.rm=TRUE), 1), " to ",
    round(max(persistence$tau_p50_ipsl, na.rm=TRUE), 1), " months\n", sep="")

panel_c <- ggplot(tau_data, aes(x = tau, fill = source)) +
  geom_density(alpha = 0.5, linewidth = 0.6) +
  scale_fill_manual(
    name = "Dataset",
    values = c("GRACE" = "#E69F00", "CESM2" = "#0072B2", "IPSL" = "#009E73"),
    labels = c("GRACE" = "GRACE (obs)", "CESM2" = "CESM2 (median)", "IPSL" = "IPSL (median)")
  ) +
  scale_x_log10(
    name = "Persistence timescale (months)",
    breaks = c(1, 3, 10, 30, 100),
    limits = c(1, 150)
  ) +
  scale_y_continuous(name = "Density") +
  annotation_logticks(sides = "b", size = 0.3, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 1.2, y = Inf, label = "c",
           size = 5, fontface = "bold", hjust = 0, vjust = 1.5)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Lag-1 ACF vs Persistence
# ============================================================================

cat("Creating Panel D: Lag-1 ACF vs persistence...\n")

# Prepare GRACE data
grace_acf <- persistence %>%
  filter(!is.na(lag1_acf_grace) & !is.na(tau_grace)) %>%
  select(lag1_acf = lag1_acf_grace, tau = tau_grace) %>%
  mutate(source = "GRACE")

# Prepare model data
cesm_acf <- persistence %>%
  filter(!is.na(lag1_acf_p50_cesm) & !is.na(tau_p50_cesm)) %>%
  select(lag1_acf = lag1_acf_p50_cesm, tau = tau_p50_cesm) %>%
  mutate(source = "CESM2")

ipsl_acf <- persistence %>%
  filter(!is.na(lag1_acf_p50_ipsl) & !is.na(tau_p50_ipsl)) %>%
  select(lag1_acf = lag1_acf_p50_ipsl, tau = tau_p50_ipsl) %>%
  mutate(source = "IPSL")

acf_data <- bind_rows(grace_acf, cesm_acf, ipsl_acf)

# Compute correlation for GRACE
cor_grace <- cor(grace_acf$lag1_acf, log10(grace_acf$tau),
                 use = "complete.obs", method = "spearman")

panel_d <- ggplot(acf_data, aes(x = lag1_acf, y = tau, color = source)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(data = grace_acf, method = "lm", se = TRUE,
              color = "grey30", linewidth = 0.8, fill = "grey80", alpha = 0.3) +
  scale_color_manual(
    name = "Dataset",
    values = c("GRACE" = "#E69F00", "CESM2" = "#0072B2", "IPSL" = "#009E73"),
    labels = c("GRACE" = "GRACE", "CESM2" = "CESM2 (median)", "IPSL" = "IPSL (median)")
  ) +
  scale_x_continuous(
    name = "Lag-1 autocorrelation",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_y_log10(
    name = "Persistence (months)",
    breaks = c(1, 3, 10, 30, 100),
    limits = c(1, 150)
  ) +
  annotation_logticks(sides = "l", size = 0.3, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("text", x = 0.05, y = 120, size = 3,
           label = sprintf("d\nGRACE ρ = %.2f", cor_grace),
           hjust = 0, vjust = 1, fontface = "bold")

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig19 <- (panel_a | panel_b) /
  (panel_c | panel_d) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig19, "fig19_persistence_maps", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 19 PERSISTENCE MAPS COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig19_persistence_maps.{png,pdf}\n")
cat("============================================================================\n\n")
