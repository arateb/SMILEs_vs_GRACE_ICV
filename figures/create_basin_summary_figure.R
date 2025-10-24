# ==============================================================================
# Function: create_basin_summary_figure
# ==============================================================================
# Original name: make_figure5# 
# Description:
#   This function is part of a comprehensive terrestrial water storage (TWS)
#   analysis framework comparing GRACE satellite observations with climate
#   model ensemble outputs (CESM2 and IPSL).
#
# =============================================================================
# FIGURE 5: PIT CALIBRATION HISTOGRAMS
# =============================================================================
# =============================================================================
# FIGURE 5: PIT CALIBRATION - BEAUTIFUL VERSION
# =============================================================================
#
# ==============================================================================

create_basin_summary_figure <- function(amplsd_aug) {
  
  cat("Creating Figure 5: PIT Calibration...\n")
  
  library(dplyr)
  library(patchwork)
  library(ggplot2)
  
  # Gorgeous color palette
  model_colors <- c("CESM2" = "#6baed6", "IPSL" = "#fc8d59")
  model_colors_dark <- c("CESM2" = "#2171b5", "IPSL" = "#d7301f")
  
  # Function to compute PIT values
  compute_pit <- function(obs, q_cols) {
    n <- length(obs)
    pit_vals <- numeric(n)
    for (i in 1:n) {
      quantiles <- as.numeric(q_cols[i, ])
      pit_vals[i] <- mean(quantiles <= obs[i], na.rm = TRUE)
    }
    return(pit_vals)
  }
  
  # Get quantile columns
  amp_q_cols <- amplsd_aug %>%
    dplyr::select(matches("^q[0-9]+\\.?[0-9]*_amp$")) %>%
    dplyr::select(-matches("IQR|MAD|median|N"))
  
  sd_q_cols <- amplsd_aug %>%
    dplyr::select(matches("^q[0-9]+\\.?[0-9]*_sd$")) %>%
    dplyr::select(-matches("IQR|MAD|median|N"))
  
  # Compute PITs
  pit_data <- amplsd_aug %>%
    mutate(
      pit_amp = compute_pit(amp_grace, amp_q_cols),
      pit_sd = compute_pit(sd_grace, sd_q_cols)
    ) %>%
    dplyr::select(bd_ID, model_name, pit_amp, pit_sd)
  
  # Compute comprehensive statistics
  pit_stats <- pit_data %>%
    group_by(model_name) %>%
    summarise(
      ks_amp_stat = ks.test(pit_amp, "punif")$statistic,
      ks_amp_p = ks.test(pit_amp, "punif")$p.value,
      ks_sd_stat = ks.test(pit_sd, "punif")$statistic,
      ks_sd_p = ks.test(pit_sd, "punif")$p.value,
      mean_amp = mean(pit_amp, na.rm = TRUE),
      mean_sd = mean(pit_sd, na.rm = TRUE),
      # Calibration metrics
      bias_amp = mean(pit_amp - 0.5, na.rm = TRUE),
      bias_sd = mean(pit_sd - 0.5, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Enhanced histogram function
  make_pit_histogram <- function(data, pit_var, model, metric, stats_row) {
    n_bins <- 20
    expected <- nrow(data) / n_bins
    
    # Create smooth density
    dens <- density(data[[pit_var]], adjust = 1.3, from = 0, to = 1, n = 512)
    dens_df <- data.frame(x = dens$x, y = dens$y * nrow(data) / n_bins)
    
    # Extract statistics
    ks_p <- if(metric == "amplitude") stats_row$ks_amp_p else stats_row$ks_sd_p
    ks_stat <- if(metric == "amplitude") stats_row$ks_amp_stat else stats_row$ks_sd_stat
    bias <- if(metric == "amplitude") stats_row$bias_amp else stats_row$bias_sd
    
    # Determine calibration quality
    cal_quality <- case_when(
      ks_p > 0.1 ~ "Well calibrated",
      ks_p > 0.05 ~ "Acceptable",
      TRUE ~ "Poor calibration"
    )
    
    cal_color <- case_when(
      ks_p > 0.1 ~ "#1a9850",
      ks_p > 0.05 ~ "#fdae61",
      TRUE ~ "#d73027"
    )
    
    ggplot(data, aes(x = .data[[pit_var]])) +
      # Gradient background for perfect uniformity zone
      annotate("rect", xmin = 0, xmax = 1, 
               ymin = expected * 0.85, ymax = expected * 1.15,
               fill = "#e5f5e0", alpha = 0.4) +
      # Wider acceptable zone
      annotate("rect", xmin = 0, xmax = 1, 
               ymin = expected * 0.75, ymax = expected * 1.25,
               fill = "#fee6ce", alpha = 0.25) +
      # Histogram bars with gradient
      geom_histogram(bins = n_bins, 
                     fill = model_colors[model],
                     color = "white", 
                     alpha = 0.85, 
                     size = 0.5) +
      # Smooth density curve overlay
      geom_line(data = dens_df, aes(x = x, y = y), 
                color = model_colors_dark[model], 
                size = 1.2, 
                alpha = 0.95) +
      # Perfect uniform reference
      geom_hline(yintercept = expected, 
                 linetype = "solid", 
                 color = "grey30", 
                 size = 0.6, 
                 alpha = 0.8) +
      # Calibration quality badge
      annotate("label", 
               x = 0.02, y = Inf, 
               vjust = 1.3, hjust = 0,
               label = cal_quality,
               size = 3.5, 
               fontface = "bold",
               color = "white", 
               fill = cal_color,
               label.padding = unit(0.3, "lines"),
               label.size = 0) +
      # Statistical info box
      annotate("label", 
               x = 0.98, y = Inf, 
               vjust = 1.3, hjust = 1,
               label = sprintf("KS = %.3f\np = %.3f\nBias = %+.3f", 
                             ks_stat, ks_p, bias),
               size = 2.8, 
               color = "grey10", 
               fill = alpha("white", 0.92),
               label.size = 0.25,
               lineheight = 0.9) +
      # Annotations for interpretation
      annotate("text", x = 0.5, y = expected * 0.65, 
               label = "Expected frequency", 
               size = 2.5, color = "grey40", fontface = "italic") +
      scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2),
        labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
        expand = c(0.005, 0)
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        title = sprintf("%s: %s PIT", model, metric),
        x = "Probability Integral Transform",
        y = "Frequency"
      ) +
      theme_nature() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major.y = element_line(color = "grey94", size = 0.3),
        panel.grid.major.x = element_line(color = "grey96", size = 0.25),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "grey50", size = 0.4),
        axis.ticks.length = unit(3, "pt"),
        axis.text = element_text(size = 9, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20"),
        panel.border = element_rect(color = "grey65", fill = NA, size = 0.5),
        plot.title = element_text(size = 11, face = "bold", color = "grey10"),
        plot.background = element_rect(fill = "white", color = NA)
      )
  }
  
  # Create all histograms
  cesm_data <- filter(pit_data, model_name == "CESM2")
  ipsl_data <- filter(pit_data, model_name == "IPSL")
  
  cesm_stats <- filter(pit_stats, model_name == "CESM2")
  ipsl_stats <- filter(pit_stats, model_name == "IPSL")
  
  p5a <- make_pit_histogram(cesm_data, "pit_amp", "CESM2", "Amplitude", cesm_stats)
  p5b <- make_pit_histogram(cesm_data, "pit_sd", "CESM2", "Variance", cesm_stats)
  p5c <- make_pit_histogram(ipsl_data, "pit_amp", "IPSL", "Amplitude", ipsl_stats)
  p5d <- make_pit_histogram(ipsl_data, "pit_sd", "IPSL", "Variance", ipsl_stats)
  
  # Panel E: Stunning Q-Q plot for amplitude
  qq_amp <- pit_data %>%
    arrange(model_name, pit_amp) %>%
    group_by(model_name) %>%
    mutate(
      theoretical = (row_number() - 0.5) / n(),
      n_obs = n(),
      # 95% confidence bands (Kolmogorov-Smirnov)
      lower_band = pmax(0, theoretical - 1.36 / sqrt(n_obs)),
      upper_band = pmin(1, theoretical + 1.36 / sqrt(n_obs))
    ) %>%
    ungroup()
  
  # Calculate deviation from diagonal
  qq_amp <- qq_amp %>%
    mutate(deviation = abs(pit_amp - theoretical))
  
  p5e <- ggplot(qq_amp, aes(x = theoretical, y = pit_amp)) +
    # Gradient background
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "#f7f7f7", alpha = 0.5) +
    # Confidence bands with gradient
    geom_ribbon(aes(ymin = lower_band, ymax = upper_band, fill = model_name),
                alpha = 0.18, color = NA) +
    # Perfect calibration line (bold)
    geom_abline(slope = 1, intercept = 0, 
                linetype = "solid", 
                color = "grey20", 
                size = 0.8, 
                alpha = 0.9) +
    # Helper reference lines
    geom_abline(slope = 1, intercept = 0.1, 
                linetype = "dotted", color = "grey60", size = 0.3) +
    geom_abline(slope = 1, intercept = -0.1, 
                linetype = "dotted", color = "grey60", size = 0.3) +
    # Points with size based on deviation
    geom_point(aes(fill = model_name, color = model_name, size = deviation), 
               alpha = 0.65, 
               shape = 21, 
               stroke = 0.25) +
    scale_size_continuous(range = c(1, 3), guide = "none") +
    scale_color_manual(values = model_colors_dark, name = "Model") +
    scale_fill_manual(values = model_colors, name = "Model") +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1),
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1),
      expand = c(0.01, 0)
    ) +
    # Annotation explaining interpretation
    annotate("text", x = 0.05, y = 0.95, 
             label = "Points on diagonal =\nPerfect calibration", 
             hjust = 0, vjust = 1, size = 2.8, 
             color = "grey30", fontface = "italic", lineheight = 0.9) +
    labs(
      title = "e. Amplitude: Q-Q calibration plot",
      x = "Theoretical uniform quantiles",
      y = "Observed PIT quantiles"
    ) +
    theme_nature() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey94", size = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "grey50", size = 0.4),
      axis.ticks.length = unit(3, "pt"),
      axis.text = element_text(size = 9, color = "grey20"),
      axis.title = element_text(size = 10, color = "grey20"),
      panel.border = element_rect(color = "grey65", fill = NA, size = 0.5),
      legend.position = c(0.98, 0.02),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = alpha("white", 0.95), 
                                      color = "grey60", size = 0.4),
      legend.key.size = unit(0.9, "lines"),
      legend.spacing.y = unit(0.2, "lines"),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 11, face = "bold", color = "grey10"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Panel F: Q-Q plot for variance
  qq_sd <- pit_data %>%
    arrange(model_name, pit_sd) %>%
    group_by(model_name) %>%
    mutate(
      theoretical = (row_number() - 0.5) / n(),
      n_obs = n(),
      lower_band = pmax(0, theoretical - 1.36 / sqrt(n_obs)),
      upper_band = pmin(1, theoretical + 1.36 / sqrt(n_obs)),
      deviation = abs(pit_sd - theoretical)
    ) %>%
    ungroup()
  
  p5f <- ggplot(qq_sd, aes(x = theoretical, y = pit_sd)) +
    # Gradient background
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "#f7f7f7", alpha = 0.5) +
    # Confidence bands
    geom_ribbon(aes(ymin = lower_band, ymax = upper_band, fill = model_name),
                alpha = 0.18, color = NA) +
    # Perfect calibration line
    geom_abline(slope = 1, intercept = 0, 
                linetype = "solid", 
                color = "grey20", 
                size = 0.8, 
                alpha = 0.9) +
    # Helper lines
    geom_abline(slope = 1, intercept = 0.1, 
                linetype = "dotted", color = "grey60", size = 0.3) +
    geom_abline(slope = 1, intercept = -0.1, 
                linetype = "dotted", color = "grey60", size = 0.3) +
    # Points
    geom_point(aes(fill = model_name, color = model_name, size = deviation), 
               alpha = 0.65, 
               shape = 21, 
               stroke = 0.25) +
    scale_size_continuous(range = c(1, 3), guide = "none") +
    scale_color_manual(values = model_colors_dark, name = "Model") +
    scale_fill_manual(values = model_colors, name = "Model") +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1),
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1),
      expand = c(0.01, 0)
    ) +
    labs(
      title = "f. Variance: Q-Q calibration plot",
      x = "Theoretical uniform quantiles",
      y = "Observed PIT quantiles"
    ) +
    theme_nature() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey94", size = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "grey50", size = 0.4),
      axis.ticks.length = unit(3, "pt"),
      axis.text = element_text(size = 9, color = "grey20"),
      axis.title = element_text(size = 10, color = "grey20"),
      panel.border = element_rect(color = "grey65", fill = NA, size = 0.5),
      legend.position = "none",
      plot.title = element_text(size = 11, face = "bold", color = "grey10"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  fig5 <- (p5a + p5b) / (p5c + p5d) / (p5e + p5f)
  
  cat("  ✓ Figure 5 complete\n")
  return(fig5)
}
