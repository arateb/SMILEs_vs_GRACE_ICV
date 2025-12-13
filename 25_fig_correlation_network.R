# ============================================================================
# FIGURE 25: Basin Correlation Patterns (Distance Decay & Networks)
# ============================================================================
#
# RESEARCH QUESTIONS:
#   1. How does correlation decay with distance?
#   2. Which regions show strongest local spatial coherence?
#   3. Are there long-range teleconnections (distant basins strongly correlated)?
#   4. Do GRACE patterns match model predictions?
#
# Panels:
#   (a) Scatter: Correlation vs distance decay (log scale, GRACE)
#   (b) Comparison: Correlation length scale L_corr (GRACE vs models)
#   (c) Comparison: Mean network correlation (GRACE vs models)
#   (d) Histogram: Distribution of all pairwise correlations
#
# Output:
#   outputs/figs/fig25_correlation_patterns.png (400 dpi)
#   outputs/figs/fig25_correlation_patterns.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 25: BASIN CORRELATION NETWORK\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load Phase 7 GRACE correlation matrix
cat("Loading GRACE correlation matrix...\n")
corr_grace <- readRDS("outputs/phase07_corr_grace.rds")
basin_summary <- readRDS("outputs/phase07_corr_basin_summary.rds")

# Extract correlation matrix
C_grace <- corr_grace$C  # 184x184 correlation matrix
basin_info <- basin_summary  # Basin metadata

cat("  Correlation matrix:", nrow(C_grace), "x", ncol(C_grace), "\n")
cat("  Basin info:", nrow(basin_info), "basins\n\n")

# Load basin shapefile
basins_shp <- st_read("/Volumes/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
basins_shp <- st_make_valid(basins_shp)
basins_shp <- st_simplify(basins_shp, dTolerance = 0.1, preserveTopology = TRUE)

# Load country borders
sf::sf_use_s2(FALSE)
world_borders <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
sf::sf_use_s2(TRUE)

# ============================================================================
# PANEL A: Correlation vs Distance Decay
# ============================================================================

cat("Creating Panel A: Correlation vs distance decay...\n")

# Load distance matrix
dist_matrix <- readRDS("outputs/phase07_distance_matrix.rds")

# Extract upper triangle correlations and distances
n_basins <- nrow(C_grace)
corr_dist_pairs <- data.frame(
  correlation = C_grace[upper.tri(C_grace)],
  distance = dist_matrix[upper.tri(dist_matrix)]
)

# Create distance bins for visualization
corr_dist_pairs <- corr_dist_pairs %>%
  filter(!is.na(correlation) & !is.na(distance)) %>%
  mutate(distance_bin = cut(distance, breaks = seq(0, 20000, by = 1000)))

# Calculate bin means for smoother visualization
bin_summary <- corr_dist_pairs %>%
  group_by(distance_bin) %>%
  summarize(
    distance_mid = mean(distance, na.rm = TRUE),
    corr_mean = mean(correlation, na.rm = TRUE),
    corr_sd = sd(correlation, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n >= 10)  # Only show bins with sufficient data

panel_a <- ggplot() +
  # Individual points (subsample for clarity)
  geom_point(data = corr_dist_pairs %>% sample_n(min(5000, nrow(corr_dist_pairs))),
             aes(x = distance, y = correlation),
             alpha = 0.05, size = 0.5, color = "#6BAED6") +
  # Binned means
  geom_point(data = bin_summary,
             aes(x = distance_mid, y = corr_mean),
             size = 2, color = "#CB181D") +
  geom_line(data = bin_summary,
            aes(x = distance_mid, y = corr_mean),
            color = "#CB181D", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.5) +
  scale_x_log10(
    name = "Distance (km)",
    breaks = c(100, 500, 1000, 5000, 10000, 20000),
    labels = c("100", "500", "1k", "5k", "10k", "20k")
  ) +
  scale_y_continuous(
    name = "Correlation",
    limits = c(-0.5, 0.8),
    breaks = seq(-0.4, 0.8, 0.2)
  ) +
  annotation_logticks(sides = "b", size = 0.25, color = "grey50") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 120, y = 0.75, label = "a",
           size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")

# ============================================================================
# PANEL B: Correlation Length Scale Comparison
# ============================================================================

cat("Creating Panel B: Correlation length scale comparison...\n")

# Load model window metadata
windows_meta <- readRDS("outputs/phase07_corr_windows_meta.rds")

# Calculate L_corr distributions by model
L_corr_summary <- windows_meta %>%
  group_by(model) %>%
  summarize(
    L_corr_p05 = quantile(L_corr, 0.05, na.rm = TRUE),
    L_corr_p50 = quantile(L_corr, 0.50, na.rm = TRUE),
    L_corr_p95 = quantile(L_corr, 0.95, na.rm = TRUE),
    n = sum(!is.na(L_corr))
  ) %>%
  ungroup()

# GRACE L_corr
L_corr_grace <- corr_grace$L_corr

# Create comparison plot
panel_b <- ggplot(L_corr_summary, aes(x = model, y = L_corr_p50)) +
  # Model ensemble ranges (p05-p95)
  geom_errorbar(aes(ymin = L_corr_p05, ymax = L_corr_p95),
                width = 0.3, linewidth = 0.6, color = "#6BAED6") +
  geom_point(size = 4, color = "#2171B5", shape = 21, fill = "white", stroke = 1) +
  # GRACE value
  geom_hline(yintercept = L_corr_grace, linetype = "solid",
             color = "#CB181D", linewidth = 0.8) +
  annotate("text", x = 2.3, y = L_corr_grace + 1000,
           label = paste0("GRACE: ", round(L_corr_grace), " km"),
           hjust = 1, size = 3, color = "#CB181D") +
  scale_y_continuous(
    name = "Correlation length scale L_corr (km)",
    limits = c(0, max(c(L_corr_summary$L_corr_p95, L_corr_grace)) * 1.1),
    expand = c(0, 0)
  ) +
  scale_x_discrete(name = "") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 0.6, y = max(L_corr_summary$L_corr_p95) * 1.05,
           label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")

# ============================================================================
# PANEL C: Mean Off-Diagonal Correlation Comparison
# ============================================================================

cat("Creating Panel C: Mean off-diagonal correlation comparison...\n")

# Calculate mean off-diagonal distributions by model
mean_offdiag_summary <- windows_meta %>%
  group_by(model) %>%
  summarize(
    offdiag_p05 = quantile(mean_offdiag_corr, 0.05, na.rm = TRUE),
    offdiag_p50 = quantile(mean_offdiag_corr, 0.50, na.rm = TRUE),
    offdiag_p95 = quantile(mean_offdiag_corr, 0.95, na.rm = TRUE),
    n = sum(!is.na(mean_offdiag_corr))
  ) %>%
  ungroup()

# GRACE mean off-diagonal
mean_offdiag_grace <- corr_grace$mean_offdiag

# Create comparison plot
panel_c <- ggplot(mean_offdiag_summary, aes(x = model, y = offdiag_p50)) +
  # Model ensemble ranges (p05-p95)
  geom_errorbar(aes(ymin = offdiag_p05, ymax = offdiag_p95),
                width = 0.3, linewidth = 0.6, color = "#6BAED6") +
  geom_point(size = 4, color = "#2171B5", shape = 21, fill = "white", stroke = 1) +
  # GRACE value
  geom_hline(yintercept = mean_offdiag_grace, linetype = "solid",
             color = "#CB181D", linewidth = 0.8) +
  annotate("text", x = 2.3, y = mean_offdiag_grace + 0.002,
           label = paste0("GRACE: ", round(mean_offdiag_grace, 3)),
           hjust = 1, size = 3, color = "#CB181D") +
  scale_y_continuous(
    name = "Mean off-diagonal correlation",
    limits = c(min(mean_offdiag_summary$offdiag_p05) * 0.9,
               max(c(mean_offdiag_summary$offdiag_p95, mean_offdiag_grace)) * 1.1),
    expand = c(0, 0)
  ) +
  scale_x_discrete(name = "") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 0.6, y = max(mean_offdiag_summary$offdiag_p95) * 1.05,
           label = "c", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel C complete\n")

# ============================================================================
# PANEL D: Distribution of All Pairwise Correlations
# ============================================================================

cat("Creating Panel D: Distribution of pairwise correlations...\n")

# Extract upper triangle (exclude diagonal)
corr_upper <- C_grace[upper.tri(C_grace)]

# Create histogram
panel_d <- ggplot(data.frame(correlation = corr_upper), aes(x = correlation)) +
  geom_histogram(bins = 50, fill = "#6BAED6", color = "black", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.5) +
  geom_vline(xintercept = median(corr_upper, na.rm = TRUE),
             linetype = "solid", color = "#CB181D", linewidth = 0.8) +
  scale_x_continuous(
    name = "Correlation coefficient",
    limits = c(-0.6, 0.8),
    breaks = seq(-0.6, 0.8, 0.2)
  ) +
  scale_y_continuous(
    name = "Number of basin pairs",
    expand = c(0, 0)
  ) +
  annotate("text", x = median(corr_upper, na.rm = TRUE) + 0.05, y = Inf,
           label = paste0("Median = ", round(median(corr_upper, na.rm = TRUE), 3)),
           hjust = 0, vjust = 1.5, size = 3, color = "#CB181D") +
  theme_nature(base_size = 8) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = -0.55, y = Inf, label = "d",
           size = 5, fontface = "bold", hjust = 0, vjust = 1.5)

cat("  ✓ Panel D complete\n\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

fig25 <- (panel_a | panel_d) /
  (panel_b | panel_c) +
  plot_layout(heights = c(1, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig25, "fig25_correlation_network", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 25 CORRELATION NETWORK COMPLETE\n")
cat("============================================================================\n")
cat("  Figures: outputs/figs/fig25_correlation_network.{png,pdf}\n")
cat("============================================================================\n\n")

# ============================================================================
# Print Summary Statistics
# ============================================================================

cat("Summary of pairwise correlations:\n")
cat("  Mean:   ", round(mean(corr_upper, na.rm = TRUE), 3), "\n")
cat("  Median: ", round(median(corr_upper, na.rm = TRUE), 3), "\n")
cat("  SD:     ", round(sd(corr_upper, na.rm = TRUE), 3), "\n")
cat("  Range:  [", round(min(corr_upper, na.rm = TRUE), 3), ", ",
    round(max(corr_upper, na.rm = TRUE), 3), "]\n\n")

# Find strongest positive correlations
corr_mat_named <- C_grace
rownames(corr_mat_named) <- basin_info$basin_name
colnames(corr_mat_named) <- basin_info$basin_name

# Extract top 10 correlations (excluding diagonal)
corr_mat_upper <- corr_mat_named
corr_mat_upper[lower.tri(corr_mat_upper, diag = TRUE)] <- NA
corr_vec <- as.vector(corr_mat_upper)
basin_pairs <- expand.grid(basin1 = basin_info$basin_name,
                           basin2 = basin_info$basin_name)
top_idx <- order(corr_vec, decreasing = TRUE, na.last = NA)[1:10]

cat("Top 10 strongest positive correlations:\n")
for (i in 1:10) {
  idx <- top_idx[i]
  cat(sprintf("  %2d. %s <-> %s: %.3f\n",
              i,
              basin_pairs$basin1[idx],
              basin_pairs$basin2[idx],
              corr_vec[idx]))
}
cat("\n")
