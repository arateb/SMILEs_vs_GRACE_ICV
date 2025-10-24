library(tidyverse)
# Load data
G <- readRDS("analysis/GGFo_vs_MMILEs_Comparison_UpdatedAug25.rds")
amplsd_aug <- G$updated_AMp_SD_Min_MAC
ampl_aug <- G$newAmplitude_results_aug
# ==============================================================================
# Q1: WHERE DOES GRACE LIE?
# ==============================================================================
q1_position <- amplsd_aug %>%
  mutate(
    amp_position = case_when(
      amp_grace < q5_amp ~ "Below all (< q5)",
      amp_grace > q95_amp ~ "Above all (> q95)",
      TRUE ~ "Within (q5-q95)"
    ),
    sd_position = case_when(
      sd_grace < q5_sd ~ "Below all (< q5)",
      sd_grace > q95_sd ~ "Above all (> q95)",
      TRUE ~ "Within (q5-q95)"
    ),
    min_position = case_when(
      min_grace < q5_min ~ "More extreme (< q5)",
      min_grace > q95_min ~ "Less extreme (> q95)",
      TRUE ~ "Within (q5-q95)"
    ),
    max_position = case_when(
      max_grace > q95_max ~ "More extreme (> q95)",
      max_grace < q5_max ~ "Less extreme (< q5)",
      TRUE ~ "Within (q5-q95)"
    )
  )

q1_summary <- q1_position %>%
  group_by(model_name) %>%
  summarise(
    n_basins = n(),
    # AMPLITUDE
    amp_below_all_n = sum(amp_position == "Below all (< q5)"),
    amp_above_all_n = sum(amp_position == "Above all (> q95)"),
    amp_within_n = sum(amp_position == "Within (q5-q95)"),
    amp_below_pct = round(amp_below_all_n / n() * 100, 1),
    amp_above_pct = round(amp_above_all_n / n() * 100, 1),
    amp_within_pct = round(amp_within_n / n() * 100, 1),
    # VARIANCE
    sd_below_all_n = sum(sd_position == "Below all (< q5)"),
    sd_above_all_n = sum(sd_position == "Above all (> q95)"),
    sd_within_n = sum(sd_position == "Within (q5-q95)"),
    sd_below_pct = round(sd_below_all_n / n() * 100, 1),
    sd_above_pct = round(sd_above_all_n / n() * 100, 1),
    sd_within_pct = round(sd_within_n / n() * 100, 1),
    # DROUGHT
    min_more_extreme_n = sum(min_position == "More extreme (< q5)"),
    min_less_extreme_n = sum(min_position == "Less extreme (> q95)"),
    min_within_n = sum(min_position == "Within (q5-q95)"),
    min_more_extreme_pct = round(min_more_extreme_n / n() * 100, 1),
    min_less_extreme_pct = round(min_less_extreme_n / n() * 100, 1),
    min_within_pct = round(min_within_n / n() * 100, 1),
    # PLUVIAL
    max_more_extreme_n = sum(max_position == "More extreme (> q95)"),
    max_less_extreme_n = sum(max_position == "Less extreme (< q5)"),
    max_within_n = sum(max_position == "Within (q5-q95)"),
    max_more_extreme_pct = round(max_more_extreme_n / n() * 100, 1),
    max_less_extreme_pct = round(max_less_extreme_n / n() * 100, 1),
    max_within_pct = round(max_within_n / n() * 100, 1)
  )

# ==============================================================================
# Q2: BIAS STRUCTURE (RATIOS)
# ==============================================================================

q2_ratios <- amplsd_aug %>%
  mutate(
    amp_ratio = amp_grace / median_amp,
    sd_ratio = sd_grace / median_sd,
    amp_class = case_when(
      amp_ratio < 0.8 ~ "Overdispersed",
      amp_ratio > 1.2 ~ "Underdispersed",
      TRUE ~ "Adequate"
    ),
    sd_class = case_when(
      sd_ratio < 0.8 ~ "Overdispersed",
      sd_ratio > 1.2 ~ "Underdispersed",
      TRUE ~ "Adequate"
    )
  )

q2_summary <- q2_ratios %>%
  group_by(model_name) %>%
  summarise(
    amp_underdispersed_pct = round(mean(amp_class == "Underdispersed") * 100, 1),
    amp_overdispersed_pct = round(mean(amp_class == "Overdispersed") * 100, 1),
    amp_adequate_pct = round(mean(amp_class == "Adequate") * 100, 1),
    amp_median_ratio = round(median(amp_ratio), 2),
    sd_underdispersed_pct = round(mean(sd_class == "Underdispersed") * 100, 1),
    sd_overdispersed_pct = round(mean(sd_class == "Overdispersed") * 100, 1),
    sd_adequate_pct = round(mean(sd_class == "Adequate") * 100, 1),
    sd_median_ratio = round(median(sd_ratio), 2)
  )

# ==============================================================================
# Q3: JOINT ADEQUACY
# ==============================================================================

q3_summary <- q2_ratios %>%
  mutate(
    both_adequate = (amp_class == "Adequate") & (sd_class == "Adequate")
  ) %>%
  group_by(model_name) %>%
  summarise(
    both_adequate_pct = round(mean(both_adequate) * 100, 1),
    amp_only_pct = round(mean(amp_class == "Adequate" & sd_class != "Adequate") * 100, 1),
    sd_only_pct = round(mean(sd_class == "Adequate" & amp_class != "Adequate") * 100, 1),
    neither_pct = round(mean(amp_class != "Adequate" & sd_class != "Adequate") * 100, 1),
    ratio_correlation = round(cor(amp_ratio, sd_ratio), 3)
  )

# ==============================================================================
# Q4: MEMBER-LEVEL SUCCESS
# ==============================================================================

q4_member_hits <- ampl_aug %>%
  left_join(
    amplsd_aug %>% dplyr::select(model_name, bd_ID, amp_grace, sd_grace),
    by = c("model" = "model_name", "bd_id" = "bd_ID")
  ) %>%
  mutate(
    amp_hits = amplitude >= amp_grace * 0.8 & amplitude <= amp_grace * 1.2,
    sd_hits = sd >= sd_grace * 0.8 & sd <= sd_grace * 1.2
  ) %>%
  group_by(model, bd_id) %>%
  summarise(
    n_members = n(),
    amp_hit_count = sum(amp_hits, na.rm = TRUE),
    sd_hit_count = sum(sd_hits, na.rm = TRUE),
    .groups = "drop"
  )

q4_summary <- q4_member_hits %>%
  group_by(model) %>%
  summarise(
    n_basins = n(),
    basins_with_amp_hits = sum(amp_hit_count > 0),
    basins_with_sd_hits = sum(sd_hit_count > 0),
    pct_basins_amp_hits = round(basins_with_amp_hits / n() * 100, 1),
    pct_basins_sd_hits = round(basins_with_sd_hits / n() * 100, 1),
    mean_amp_hit_rate = round(mean(amp_hit_count[amp_hit_count > 0] / n_members[amp_hit_count > 0]), 3),
    mean_sd_hit_rate = round(mean(sd_hit_count[sd_hit_count > 0] / n_members[sd_hit_count > 0]), 3)
  )
# ==============================================================================
# Q5: CALIBRATION - SIMPLIFIED (NO ROWWISE)
# ==============================================================================

# Simple percentile calculation
q5_percentiles <- amplsd_aug %>%
  mutate(
    # Simple binning approach
    amp_pit_bin = case_when(
      amp_grace < q10_amp ~ "0-10",
      amp_grace < q20_amp ~ "10-20",
      amp_grace < q30_amp ~ "20-30",
      amp_grace < q40_amp ~ "30-40",
      amp_grace < q50_amp ~ "40-50",
      amp_grace < q60_amp ~ "50-60",
      amp_grace < q70_amp ~ "60-70",
      amp_grace < q80_amp ~ "70-80",
      amp_grace < q90_amp ~ "80-90",
      TRUE ~ "90-100"
    ),
    sd_pit_bin = case_when(
      sd_grace < q10_sd ~ "0-10",
      sd_grace < q20_sd ~ "10-20",
      sd_grace < q30_sd ~ "20-30",
      sd_grace < q40_sd ~ "30-40",
      sd_grace < q50_sd ~ "40-50",
      sd_grace < q60_sd ~ "50-60",
      sd_grace < q70_sd ~ "60-70",
      sd_grace < q80_sd ~ "70-80",
      sd_grace < q90_sd ~ "80-90",
      TRUE ~ "90-100"
    )
  )

q5_uniformity <- q5_percentiles %>%
  group_by(model_name) %>%
  summarise(
    amp_chisq_pval = chisq.test(table(amp_pit_bin))$p.value,
    sd_chisq_pval = chisq.test(table(sd_pit_bin))$p.value,
    amp_in_tails_pct = round(mean(amp_grace < q10_amp | amp_grace > q90_amp) * 100, 1),
    sd_in_tails_pct = round(mean(sd_grace < q10_sd | sd_grace > q90_sd) * 100, 1)
  )

# ==============================================================================
# Q6: EXTREME COVERAGE
# ==============================================================================

q6_extremes <- q1_position %>%
  group_by(model_name) %>%
  summarise(
    n_basins = n(),
    drought_zero_coverage_pct = round(mean(min_position != "Within (q5-q95)") * 100, 1),
    drought_more_severe_pct = round(mean(min_position == "More extreme (< q5)") * 100, 1),
    pluvial_zero_coverage_pct = round(mean(max_position != "Within (q5-q95)") * 100, 1),
    pluvial_more_severe_pct = round(mean(max_position == "More extreme (> q95)") * 100, 1)
  )

# ==============================================================================
# Q7: EXTREME SEVERITY
# ==============================================================================

q7_severity <- amplsd_aug %>%
  mutate(
    min_dist = ifelse(min_grace < q5_min, (q5_min - min_grace) / IQR_min, 0),
    max_dist = ifelse(max_grace > q95_max, (max_grace - q95_max) / IQR_max, 0)
  ) %>%
  filter(min_dist > 0 | max_dist > 0) %>%
  group_by(model_name) %>%
  summarise(
    n_exceedances = n(),
    median_drought_dist = round(median(min_dist[min_dist > 0], na.rm = TRUE), 2),
    median_pluvial_dist = round(median(max_dist[max_dist > 0], na.rm = TRUE), 2)
  )

# ==============================================================================
# Q9: STRUCTURAL DIAGNOSIS
# ==============================================================================

q9_diagnosis <- amplsd_aug %>%
  mutate(
    amp_cv = (q75_amp - q25_amp) / median_amp,
    amp_outside = amp_grace < q5_amp | amp_grace > q95_amp
  ) %>%
  group_by(model_name) %>%
  summarise(
    ensemble_size = first(n_members),
    mean_amp_cv = round(mean(amp_cv), 3),
    pct_outside = round(mean(amp_outside) * 100, 1),
    diagnosis = ifelse(mean_amp_cv < 0.15 & mean(amp_outside) > 0.3, 
                      "STRUCTURAL PROBLEM", 
                      "Unclear"))

# ==============================================================================
# Q10: CROSS-MODEL CONSISTENCY
# ==============================================================================

q10_cross <- q1_position %>%
  dplyr::select(bd_ID, model_name, amp_position, sd_position) %>%
  pivot_wider(names_from = model_name, values_from = c(amp_position, sd_position))

q10_summary <- q10_cross %>%
  summarise(
    n_basins = n(),
    amp_both_fail_pct = round(mean((`amp_position_CESM2` != "Within (q5-q95)") & 
                                   (`amp_position_IPSL` != "Within (q5-q95)"), na.rm = TRUE) * 100, 1),
    sd_both_fail_pct = round(mean((`sd_position_CESM2` != "Within (q5-q95)") & 
                                  (`sd_position_IPSL` != "Within (q5-q95)"), na.rm = TRUE) * 100, 1)
  )

# ==============================================================================
# PRINT EVERYTHING
# ==============================================================================

cat("\n========== Q1: WHERE DOES GRACE LIE? ==========\n")
print(q1_summary)

cat("\n========== Q2: BIAS STRUCTURE ==========\n")
print(q2_summary)

cat("\n========== Q3: JOINT ADEQUACY ==========\n")
print(q3_summary)

cat("\n========== Q4: MEMBER SUCCESS ==========\n")
print(q4_summary)

cat("\n========== Q5: CALIBRATION ==========\n")
print(q5_uniformity)

cat("\n========== Q6: EXTREMES ==========\n")
print(q6_extremes)

cat("\n========== Q7: SEVERITY ==========\n")
print(q7_severity)

cat("\n========== Q9: DIAGNOSIS ==========\n")
print(q9_diagnosis)

cat("\n========== Q10: CROSS-MODEL ==========\n")
print(q10_summary)


library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(patchwork)
library(scales)
library(ggrepel)

# Load results
results <- readRDS("analysis/COMPLETE_RESULTS_ALL_QUESTIONS.rds")
amplsd_aug <- readRDS("analysis/GGFo_vs_MMILEs_Comparison_UpdatedAug25.rds")$updated_AMp_SD_Min_MAC

# Load basin shapefile (adjust path)
 shp <- st_read("/Volumes/data/Vector/Global/Gbasins/FinalGbasins_April25.shp", quiet = TRUE)
shp <- st_make_valid(shp)                 # GEOS repair
cen <- st_centroid(shp)                   # mathematical centroid (not just a la
# 4. Re-project centroid to geographic coords (EPSG 4326) ----------------------
cen_ll <- st_transform(cen, 4326)         # now in degrees    # adds X, Y # 5. Extract numeric columns and bind to attributes ----------------------------
coords <- st_coordinates(cen_ll)          # matrix with X = lon, Y = lat
out    <- cbind(st_drop_geometry(shp), 
                lon = coords[, "X"],
                lat = coords[, "Y"]) 

basins_sf=shp
# ==============================================================================
# FIGURE 1: Global Patterns of Amplitude and Variance Failures
# ==============================================================================
# =============================================================================
# ALL-IN-ONE: NATURE-QUALITY FIGURES FOR TWS ANALYSIS
# =============================================================================
# Author: Dr. Ashraf Rateb
# Purpose: Single file with all functions to generate publication figures
# Usage: source("nature_figures_complete.R") then run make_all_figures()
# =============================================================================
# =============================================================================
# ALL-IN-ONE: NATURE-QUALITY FIGURES FOR TWS ANALYSIS
# =============================================================================
# Author: Dr. Ashraf Rateb
# Purpose: Single file with all functions to generate publication figures
# Usage: source("nature_figures_complete.R") then run make_all_figures()
# =============================================================================

# Load packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(patchwork)
  library(scales)
  library(viridis)
  library(RColorBrewer)
})

# =============================================================================
# THEMES AND COLORS
# =============================================================================

theme_nature_map <- function() {
  theme_minimal(base_size = 9) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold"),
      legend.position = "right",
      legend.key.size = unit(4, "mm"),
      plot.margin = margin(2, 2, 2, 2, "mm")
    )
}

theme_nature <- function() {
  theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}

# Earth-tone colors
earth_colors <- list(
  ratio = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090",
            "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"),
  amplitude = c("#FFF7BC", "#FEE391", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02"),
  water = c("#F7FCFD", "#E0ECF4", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7"),
  severity = c("#FEE5D9", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D"),
  models = c(CESM2 = "#2C7BB6", IPSL = "#D7191C", GRACE = "#1A9850")
)

# =============================================================================
# FIGURE 1: GLOBAL TWS AMPLITUDE AND VARIANCE PATTERNS
# =============================================================================

make_figure1 <- function(amplsd_aug, basins_sf) {
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
(Content truncated due to size limit. Use page ranges or line ranges to read remaining content)