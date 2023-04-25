tidy_meta <- function(meta_object) {
  pred_vals <- predict(meta_object)

  tibble(
    est = pred_vals$pred,
    se = pred_vals$se,
    ci.lb = pred_vals$ci.lb,
    ci.ub = pred_vals$ci.ub,
    pi.lb = pred_vals$pi.lb,
    pi.ub = pred_vals$pi.ub,
  )
}

axis_limits <- function(plot, x_lim, y_lim, x_ticks, y_ticks) {
  if (!is_null(x_ticks) & !is_null(y_ticks)) {
    plot <- plot +
      scale_x_continuous(breaks = x_ticks, expand = expansion(mult = 0)) +
      scale_y_reverse(breaks = y_ticks, expand = expansion(mult = 0)) +
      coord_cartesian(ylim = c(y_lim[2], y_lim[1]), xlim = c(x_lim[1], x_lim[2]))
  } else {
    plot <- plot +
      scale_x_continuous(expand = expansion(mult = 0)) +
      scale_y_reverse(expand = expansion(mult = 0)) +
      coord_cartesian(ylim = c(y_lim[2], y_lim[1]), xlim = c(x_lim[1], x_lim[2]))
  }
  plot
}

axis_limits_exp <- function(plot, x_lim, y_lim, x_ticks, y_ticks) {
  if (!is_null(x_ticks) & !is_null(y_ticks)) {
    plot <- plot +
      scale_x_log10(breaks = exp(x_ticks), expand = expansion(mult = 0)) +
      scale_y_reverse(breaks = y_ticks, expand = expansion(mult = 0)) +
      coord_cartesian(ylim = c(y_lim[2], y_lim[1]), xlim = c(exp(x_lim[1]), exp(x_lim[2])))
  } else {
    plot <- plot +
      scale_x_log10(expand = expansion(mult = 0)) +
      scale_y_reverse(expand = expansion(mult = 0)) +
      coord_cartesian(ylim = c(y_lim[2], y_lim[1]), xlim = c(exp(x_lim[1]), exp(x_lim[2])))
  }
  plot
}

axis_limits_n <- function(plot, x_lim, y_lim, x_ticks, y_ticks) {
  if (!is_null(x_ticks) & !is_null(y_ticks)) {
    plot <- plot +
      scale_x_continuous(breaks = x_ticks, expand = expansion(mult = 0)) +
      scale_y_continuous(breaks = y_ticks, expand = expansion(mult = 0)) +
      coord_cartesian(ylim = c(y_lim[1], y_lim[2]), xlim = c(x_lim[1], x_lim[2]))
  } else {
    plot <- plot +
      scale_x_continuous(expand = expansion(mult = 0)) +
      scale_y_continuous(expand = expansion(mult = 0)) +
      coord_cartesian(ylim = c(y_lim[1], y_lim[2]), xlim = c(x_lim[1], x_lim[2]))
  }
  plot
}

extfunnel2_n <- function(yi, sei, method = "REML", test = "z",
                         contour_points = 50, swe = 0,
                         x_lim, y_lim,
                         sd, n,
                         x_lab = "Effect size", y_lab = "Sample size per group",
                         x_ticks = NULL, y_ticks = NULL,
                         legend_pos = "right") {
  # Run meta-analysis with current studies
  res <- tidy_meta(rma.uni(yi = yi, sei = sei, method = method, test = test))


  ### Simulation

  # Create n = contour_points values from the lower to upper limit of the effect estimates (yi_sim) and sample sizes (n_sim)
  yi_sim <- seq(x_lim[1], x_lim[2], length.out = contour_points)
  n_sim <- seq(y_lim[1], y_lim[2], length.out = contour_points)

  # Minimal sample size per arm = 10
  n_sim[n_sim <= 10] <- 10

  # Create all possible combinations of effect estimates and sample sizes to map out the whole plot area
  df_sim <- expand_grid(yi_sim = yi_sim, n_sim = n_sim) %>%
    mutate(id = row_number()) %>%
    # Calculate standard errors, equal group sizes and fixed standard deviation are assumed
    mutate(n1i = n_sim, n1i = n_sim, m1i = 0, sd1i = sd, n2i = n_sim, m2i = 0 - yi_sim, sd2i = sd) %>%
    escalc(n1i = n1i, n2i = n2i, m1i = m1i, m2i = m2i, sd1i = sd1i, sd2i = sd2i, data = ., measure = "MD") %>%
    tibble() %>%
    mutate(sei_sim = sqrt(vi)) %>%
    select(id, yi_sim, n_sim, sei_sim)

  # Use parallel processing for faster computation
  plan(multisession)

  # Combine every simulated effect estimate and standard error combination with the original effect estimates and standard errors
  df_comb <-
    future_pmap(df_sim, function(id, yi_sim, n_sim, sei_sim) {
      tibble(
        id = id,
        yi_sim = c(yi, yi_sim),
        n_sim = n_sim,
        sei_sim = c(sei, sei_sim)
      )
    }) %>%
    list_rbind()

  res_sim <- df_comb %>%
    group_by(id) %>%
    group_modify(
      .data = .,
      # Rerun meta-analysis for every combination of old studies + simulated new studies
      .f = ~ suppressWarnings(tidy_meta(
        rma.uni(yi = .$yi_sim, sei = .$sei_sim, method = "DL", test = "z", control = list(tau2.max = 10000))
      ))
    ) %>%
    ungroup() %>%
    # Compare confidence intervals of pooled effect estimates with the smallest worthwhile effect (swe)
    mutate(sig = case_when(
      ci.lb < swe & ci.ub < swe ~ "Clearly worthwhile",
      ci.lb < swe & ci.ub > swe ~ "Unclear if worthwhile",
      ci.lb > swe & ci.ub > swe ~ "Clearly not worthwhile"
    )) %>%
    select(ci.lb, ci.ub, sig) %>%
    mutate(sig = as.factor(sig)) %>%
    bind_cols(yi_sim = df_sim$yi_sim, n_sim = df_sim$n_sim, sei_sim = df_sim$sei_sim, .)

  ### Create the extended funnel plot

  # Calculate y axis length
  y_axis_l <- y_lim[2] - y_lim[1]

  plot <- ggplot(data = res_sim) +
    # Plot one rectangle for each simulated meta-analytic result (color depends on sig, see case_when above)
    geom_tile(aes(x = yi_sim, y = n_sim, fill = sig)) +
    geom_hline(yintercept = y_ticks, color = "white") +
    # Add vertical line corresponding to the smallest worthwhile effect (swe)
    geom_vline(aes(xintercept = swe, color = "Smallest worthwhile effect"), linetype = "dashed") +
    # Add vertical line corresponding to the pooled estimate of the currently existing studies
    geom_vline(aes(xintercept = res$est, color = "Current pooled estimate"), linetype = "solid") +
    # Add points corresponding to point estimates of currently existing studies
    geom_point(
      data = data.frame(x = yi, y = n),
      aes(x = x, y = y, shape = "Current point estimates"),
      size = 2.5, stroke = 0.7,
      fill = "#42727D",
      color = "#070F14"
    ) +
    labs(x = x_lab, y = y_lab) +
    scale_color_manual(values = c("Smallest worthwhile effect" = "black", "Current pooled estimate" = "black")) +
    scale_shape_manual(values = c("Current point estimates" = 21)) +
    scale_fill_manual(values = c("Clearly worthwhile" = "#ded032", "Unclear if worthwhile" = "#f7766d", "Clearly not worthwhile" = "#03bfc4")) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    guides(color = guide_legend(
      override.aes = list(
        linetype = c("solid", "dashed")
      )
    ))

  plot <- axis_limits_n(plot, x_lim, y_lim, x_ticks, y_ticks)

  plot +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_line(color = "white"),
      axis.text = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0), size = 10, color = "black"),
      axis.ticks.length = unit(-0.12, "cm"),
      axis.title = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(hjust = 1, margin = margin(t = 10)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = 12)),
      legend.position = legend_pos,
      legend.spacing.y = unit(-0.04, "cm"),
      plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm")
    )
}
