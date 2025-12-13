#!/usr/bin/env Rscript
# ============================================================
# 10_map_compatibility.R
# Map and summarize GRACE–model multivariate compatibility
# ============================================================

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cowplot)

cat("\n")
cat("============================================================================\n")
cat("COMPATIBILITY MAPPING\n")
cat("============================================================================\n\n")

#--------------------------------------------------------------
# 1. Load style and data
#--------------------------------------------------------------
cat("Step 1: Loading data...\n")

source("R/plot_style.R")   # defines theme_nature(), base_world_map(),
                           # save_nature_figure(), etc.

comp   <- readRDS("outputs/phase08_compatibility_basin.rds")
basins <- readRDS("outputs/basin_attributes.rds")  # has ID, name, C_lon, C_lat

# comp uses sequential basin numbers (1-184), basins is already in same order
# Join by row number
basins <- basins %>%
  mutate(basin_seq = row_number())

cat("  ✓ Loaded compatibility data:", nrow(comp), "basins\n")
cat("  ✓ Loaded basin metadata:", nrow(basins), "basins\n\n")

#--------------------------------------------------------------
# 2. Join compatibility metrics to basin metadata
#--------------------------------------------------------------
cat("Step 2: Joining compatibility to basin metadata...\n")

basins_full <- basins %>%
  left_join(comp, by = c("basin_seq" = "basin")) %>%
  # Use basins.name as primary basin_name, use basin_id from comp
  rename(basin_name_basin = name) %>%
  select(-basin_name) %>%  # remove duplicate from comp
  rename(basin_name = basin_name_basin)

# Convert to sf (if not already)
basins_sf <- st_as_sf(
  basins_full,
  coords = c("C_lon", "C_lat"),
  crs = 4326,
  remove = FALSE
)

# Define class factor (Compatible / Incompatible / NA)
# Use C_b criterion: compatible if C_b in [0.05, 0.95]
basins_sf <- basins_sf %>%
  mutate(
    comp_class = case_when(
      is.na(C_b) ~ "NA",
      C_b >= 0.05 & C_b <= 0.95 ~ "Compatible",
      C_b < 0.05 | C_b > 0.95 ~ "Incompatible"
    ),
    comp_class = factor(comp_class,
                        levels = c("Compatible", "Incompatible", "NA"))
  )

# Convenience subsets
compatible_basins   <- basins_sf %>% filter(comp_class == "Compatible")
incompatible_basins <- basins_sf %>% filter(comp_class == "Incompatible")
na_basins <- basins_sf %>% filter(comp_class == "NA")

cat("  ✓ Classification complete\n\n")

#--------------------------------------------------------------
# 3. Basic summaries
#--------------------------------------------------------------
cat("Step 3: Computing summary statistics...\n")

summary_counts <- basins_sf %>%
  st_drop_geometry() %>%
  count(comp_class, name = "n_basins") %>%
  mutate(frac = n_basins / sum(n_basins))

cat("\nCompatibility summary:\n")
print(summary_counts)
cat("\n")

# Optional: summary of C_b distribution
summary_Cb <- basins_sf %>%
  st_drop_geometry() %>%
  summarize(
    n_valid   = sum(!is.na(C_b)),
    median_Cb = median(C_b, na.rm = TRUE),
    mean_Cb   = mean(C_b, na.rm = TRUE),
    q25_Cb    = quantile(C_b, 0.25, na.rm = TRUE),
    q75_Cb    = quantile(C_b, 0.75, na.rm = TRUE),
    min_Cb    = min(C_b, na.rm = TRUE),
    max_Cb    = max(C_b, na.rm = TRUE)
  )

cat("C_b distribution:\n")
print(summary_Cb)
cat("\n")

# Write lists for supplement
compatible_list <- compatible_basins %>%
  st_drop_geometry() %>%
  select(basin_id, basin_name, C_lon, C_lat, C_b, d_mahal,
         A_LF_grace, H_max_grace, D_max_grace, tau_grace, P_LF_grace)

incompatible_list <- incompatible_basins %>%
  st_drop_geometry() %>%
  select(basin_id, basin_name, C_lon, C_lat, C_b, d_mahal,
         A_LF_grace, H_max_grace, D_max_grace, tau_grace, P_LF_grace)

write.csv(compatible_list,
          "outputs/compatible_basins.csv", row.names = FALSE)
write.csv(incompatible_list,
          "outputs/incompatible_basins.csv", row.names = FALSE)

cat("  ✓ Saved: outputs/compatible_basins.csv (", nrow(compatible_list), " basins)\n", sep = "")
cat("  ✓ Saved: outputs/incompatible_basins.csv (", nrow(incompatible_list), " basins)\n\n", sep = "")

#--------------------------------------------------------------
# 4. Map: single global map colouring by compatibility class
#--------------------------------------------------------------
cat("Step 4: Creating global compatibility map...\n")

p_map_both <- base_world_map() +
  geom_sf(
    data = basins_sf,
    aes(color = comp_class),
    size  = 1.5,
    alpha = 0.85
  ) +
  scale_color_manual(
    name   = "Compatibility",
    values = c(
      "Compatible"   = "#009E73",  # green
      "Incompatible" = "#D55E00",  # vermillion/red
      "NA"           = "grey70"
    )
  ) +
  theme_nature_map() +
  theme(
    legend.position = c(0.12, 0.35),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7)
  ) +
  labs(
    title = "GRACE–model multivariate compatibility by basin"
  )

save_figure(
  p_map_both,
  "Fig_compatibility_map_both",
  width_mm = 180,
  height_mm = 100
)

cat("  ✓ Saved: outputs/figs/Fig_compatibility_map_both.png\n\n")

#--------------------------------------------------------------
# 5. Two-panel map: compatible vs incompatible
#--------------------------------------------------------------
cat("Step 5: Creating split compatibility maps...\n")

p_comp <- base_world_map() +
  geom_sf(
    data  = compatible_basins,
    color = "#009E73",
    size  = 1.8,
    alpha = 0.9
  ) +
  theme_nature_map() +
  theme(
    plot.title = element_text(size = 9, face = "bold")
  ) +
  labs(title = paste0("Compatible basins (n = ", nrow(compatible_list), ")"))

p_incomp <- base_world_map() +
  geom_sf(
    data  = incompatible_basins,
    color = "#D55E00",
    size  = 1.8,
    alpha = 0.9
  ) +
  theme_nature_map() +
  theme(
    plot.title = element_text(size = 9, face = "bold")
  ) +
  labs(title = paste0("Incompatible basins (n = ", nrow(incompatible_list), ")"))

fig_split <- plot_grid(
  p_comp, p_incomp,
  labels     = c("a", "b"),
  label_size = 11,
  label_fontface = "bold",
  nrow       = 1
)

save_figure(
  fig_split,
  "Fig_compatibility_map_split",
  width_mm = 180,
  height_mm = 90
)

cat("  ✓ Saved: outputs/figs/Fig_compatibility_map_split.png\n\n")

#--------------------------------------------------------------
# 6. Histogram of C_b for quick diagnosis
#--------------------------------------------------------------
cat("Step 6: Creating C_b histogram...\n")

p_hist <- basins_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(C_b)) %>%
  ggplot(aes(x = C_b)) +
  geom_histogram(
    bins  = 30,
    fill  = "grey60",
    color = "black",
    linewidth = 0.3
  ) +
  geom_vline(xintercept = c(0.05, 0.95),
             linetype = "dashed", linewidth = 0.6, color = "#D55E00") +
  geom_vline(xintercept = 0.5,
             linetype = "dotted", linewidth = 0.6, color = "#009E73") +
  annotate("text", x = 0.5, y = Inf, vjust = 1.5,
           label = "Typical\n(C_b = 0.5)",
           size = 3, color = "#009E73") +
  annotate("text", x = 0.05, y = Inf, vjust = 1.5, hjust = 1.1,
           label = "Extreme\ntails",
           size = 3, color = "#D55E00") +
  annotate("text", x = 0.95, y = Inf, vjust = 1.5, hjust = -0.1,
           label = "Extreme\ntails",
           size = 3, color = "#D55E00") +
  scale_x_continuous(
    name = "Compatibility index C_b",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    name = "Number of basins"
  ) +
  theme_nature() +
  labs(
    title = "Distribution of GRACE–model compatibility index across basins"
  )

save_figure(
  p_hist,
  "Fig_compatibility_hist",
  width_mm = 90,
  height_mm = 80
)

cat("  ✓ Saved: outputs/figs/Fig_compatibility_hist.png\n\n")

#--------------------------------------------------------------
# 7. Summary report
#--------------------------------------------------------------
cat("============================================================================\n")
cat("COMPATIBILITY MAPPING COMPLETE\n")
cat("============================================================================\n\n")

cat("Output files:\n")
cat("  - outputs/figs/Fig_compatibility_map_both.png\n")
cat("  - outputs/figs/Fig_compatibility_map_split.png\n")
cat("  - outputs/figs/Fig_compatibility_hist.png\n")
cat("  - outputs/compatible_basins.csv\n")
cat("  - outputs/incompatible_basins.csv\n\n")

cat("Key findings:\n")
cat("  Compatible basins:   ", nrow(compatible_list), " (",
    round(100 * nrow(compatible_list) / nrow(basins_sf), 1), "%)\n", sep = "")
cat("  Incompatible basins: ", nrow(incompatible_list), " (",
    round(100 * nrow(incompatible_list) / nrow(basins_sf), 1), "%)\n", sep = "")
cat("  NA (insufficient data): ", nrow(na_basins), " (",
    round(100 * nrow(na_basins) / nrow(basins_sf), 1), "%)\n\n", sep = "")

cat("Interpretation:\n")
cat("  - Compatible (green): C_b in [0.05, 0.95], GRACE typical of model variability\n")
cat("  - Incompatible (red): C_b < 0.05 or > 0.95, GRACE in extreme tails\n")
cat("  - Median C_b = ", round(summary_Cb$median_Cb, 3), "\n", sep = "")
cat("  - Mean C_b = ", round(summary_Cb$mean_Cb, 3), "\n\n", sep = "")

cat("============================================================================\n\n")

# End of script
# ============================================================
