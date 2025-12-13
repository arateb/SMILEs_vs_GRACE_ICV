# ============================================================================
# FIGURE 4: Event Morphology (Extremes)
# ============================================================================
#
# Panels:
#   (a) Pluvial events: height vs duration (density + GRACE overlay)
#   (b) Drought events: depth vs duration (density + GRACE overlay)
#   (c) Recovery time distributions (boxplots)
#
# Output:
#   outputs/figs/fig4_event_morphology.png (500 dpi)
#   outputs/figs/fig4_event_morphology.pdf (vector)
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("FIGURE 4: EVENT MORPHOLOGY\n")
cat("============================================================================\n\n")

# Source plotting utilities
source("R/plot_style.R")

# Load data
cat("Loading data...\n")
event_summary <- readRDS("outputs/phase06_event_summary.rds")
events_grace <- readRDS("outputs/phase06_events_grace.rds")
events_models <- readRDS("outputs/phase06_events_models.rds")

cat("  Event summary:", nrow(event_summary), "basins\n")
cat("  GRACE events:", nrow(events_grace), "\n")
cat("  Model events:", nrow(events_models), "\n\n")

# ============================================================================
# PANEL A: Pluvial Events (Height vs Duration)
# ============================================================================

cat("Creating Panel A: Pluvial height vs duration...\n")

# Model pluvials
pluvials_model <- events_models %>%
  filter(type == "pluvial" & !is.na(pluvial_height) & !is.na(duration_months)) %>%
  filter(duration_months <= 50 & pluvial_height <= 500)  # Reasonable limits

# GRACE pluvials
pluvials_grace <- events_grace %>%
  filter(type == "pluvial" & !is.na(pluvial_height) & !is.na(duration_months))

panel_a <- ggplot() +
  # Model density (2D hexbin)
  geom_hex(data = pluvials_model,
           aes(x = duration_months, y = pluvial_height),
           bins = 40, alpha = 0.7) +
  scale_fill_viridis_nature(
    name = "Model\nevents",
    option = "C",
    trans = "log10",
    breaks = c(10, 100, 1000, 10000)
  ) +
  # GRACE points overlay
  geom_point(data = pluvials_grace,
             aes(x = duration_months, y = pluvial_height),
             color = "#D55E00", fill = "#D55E00",
             size = 2, alpha = 0.8, shape = 21, stroke = 0.5) +
  scale_x_continuous(
    name = "Duration (months)",
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10)
  ) +
  scale_y_continuous(
    name = "Pluvial height (mm)",
    limits = c(0, 500),
    breaks = seq(0, 500, by = 100)
  ) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6)
  ) +
  # Add GRACE legend manually
  annotate("point", x = 45, y = 470, color = "#D55E00", fill = "#D55E00",
           size = 2.5, shape = 21, stroke = 0.5) +
  annotate("text", x = 43, y = 470, label = "GRACE", hjust = 1, size = 2.5, fontface = "bold") +
  # Add panel label
  annotate("text", x = 1, y = 495, label = "a", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel A complete\n")
cat("    Model pluvials:", nrow(pluvials_model), "\n")
cat("    GRACE pluvials:", nrow(pluvials_grace), "\n\n")

# ============================================================================
# PANEL B: Drought Events (Depth vs Duration)
# ============================================================================

cat("Creating Panel B: Drought depth vs duration...\n")

# Model droughts
droughts_model <- events_models %>%
  filter(type == "drought" & !is.na(drought_depth) & !is.na(duration_months)) %>%
  filter(duration_months <= 50 & drought_depth >= -500)  # Reasonable limits

# GRACE droughts
droughts_grace <- events_grace %>%
  filter(type == "drought" & !is.na(drought_depth) & !is.na(duration_months))

panel_b <- ggplot() +
  # Model density (2D hexbin)
  geom_hex(data = droughts_model,
           aes(x = duration_months, y = abs(drought_depth)),  # Use absolute value for easier viz
           bins = 40, alpha = 0.7) +
  scale_fill_viridis_nature(
    name = "Model\nevents",
    option = "C",
    trans = "log10",
    breaks = c(10, 100, 1000, 10000)
  ) +
  # GRACE points overlay
  geom_point(data = droughts_grace,
             aes(x = duration_months, y = abs(drought_depth)),
             color = "#D55E00", fill = "#D55E00",
             size = 2, alpha = 0.8, shape = 21, stroke = 0.5) +
  scale_x_continuous(
    name = "Duration (months)",
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10)
  ) +
  scale_y_continuous(
    name = "Drought depth (mm, absolute)",
    limits = c(0, 500),
    breaks = seq(0, 500, by = 100)
  ) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.key.size = unit(3, "mm"),
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6)
  ) +
  # Add GRACE legend manually
  annotate("point", x = 45, y = 470, color = "#D55E00", fill = "#D55E00",
           size = 2.5, shape = 21, stroke = 0.5) +
  annotate("text", x = 43, y = 470, label = "GRACE", hjust = 1, size = 2.5, fontface = "bold") +
  # Add panel label
  annotate("text", x = 1, y = 495, label = "b", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel B complete\n")
cat("    Model droughts:", nrow(droughts_model), "\n")
cat("    GRACE droughts:", nrow(droughts_grace), "\n\n")

# ============================================================================
# PANEL C: Recovery Time Distributions
# ============================================================================

cat("Creating Panel C: Recovery time distributions...\n")

# Prepare recovery data
# Only include severe events (depth/height > threshold)
SEVERITY_THRESHOLD <- 100  # mm

recovery_data <- bind_rows(
  events_grace %>%
    filter(
      (type == "pluvial" & pluvial_height > SEVERITY_THRESHOLD) |
        (type == "drought" & abs(drought_depth) > SEVERITY_THRESHOLD)
    ) %>%
    filter(!is.na(recovery_months)) %>%
    mutate(source = "GRACE", model = "GRACE"),

  events_models %>%
    filter(
      (type == "pluvial" & pluvial_height > SEVERITY_THRESHOLD) |
        (type == "drought" & abs(drought_depth) > SEVERITY_THRESHOLD)
    ) %>%
    filter(!is.na(recovery_months)) %>%
    mutate(source = "Model")
) %>%
  filter(recovery_months > 0 & recovery_months < 100)  # Remove outliers

# Combine model types for simplicity
recovery_summary <- recovery_data %>%
  mutate(
    group = ifelse(source == "GRACE", "GRACE", model)
  )

panel_c <- ggplot(recovery_summary, aes(x = group, y = recovery_months, fill = group)) +
  geom_violin(alpha = 0.6, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.2, alpha = 0.9, outlier.size = 0.5, outlier.alpha = 0.4) +
  scale_fill_manual(
    values = c(
      "GRACE" = "#D55E00",
      "CESM2" = "#0072B2",
      "IPSL" = "#009E73"
    )
  ) +
  scale_y_continuous(
    name = "Recovery time (months)",
    breaks = seq(0, 100, by = 10)
  ) +
  labs(x = NULL) +
  theme_nature(base_size = 8) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  # Add median values as text
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("%.1f", ..y..)),
    vjust = -0.8,
    size = 2.5,
    fontface = "bold"
  ) +
  # Add sample sizes
  annotate("text", x = 1:3, y = -5,
           label = paste0("n=", table(recovery_summary$group)[c("GRACE", "CESM2", "IPSL")]),
           size = 2.2, color = "grey40") +
  # Add panel label
  annotate("text", x = 0.55, y = max(recovery_summary$recovery_months) * 0.98,
           label = "c", size = 5, fontface = "bold", hjust = 0)

cat("  ✓ Panel C complete\n")
cat("    Recovery time summary:\n")
print(recovery_summary %>%
        group_by(group) %>%
        summarise(
          n = n(),
          median_recovery = median(recovery_months, na.rm = TRUE),
          mean_recovery = mean(recovery_months, na.rm = TRUE)
        ))
cat("\n")

# ============================================================================
# ASSEMBLE FIGURE
# ============================================================================

cat("Assembling panels...\n")

# Layout: A and B on top (side by side), C below (narrower, centered)
fig4 <- (panel_a | panel_b) /
  (plot_spacer() | panel_c | plot_spacer()) +
  plot_layout(heights = c(1, 0.9), widths = c(1, 2, 1))

cat("  ✓ Figure assembled\n\n")

# ============================================================================
# SAVE FIGURE
# ============================================================================

save_figure(fig4, "fig4_event_morphology", width_mm = 180, height_mm = 140)

cat("\n")
cat("============================================================================\n")
cat("FIGURE 4 COMPLETE\n")
cat("============================================================================\n\n")
