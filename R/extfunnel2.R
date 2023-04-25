#' Generate Extended Funnel Plot
#'
#' Inspired by the archived \href{https://cran.r-project.org/web/packages/extfunnel/index.html}{extfunnel package} developed by \href{https://doi.org/10.1016/j.jclinepi.2011.10.009}{Langan et al}. The function generates an extended funnel plot with shaded contours that show the impact of a new study with a certain effect estimate and standard error (or sample size) on the conclusions of an updated meta-analysis. Uses ggplot2 instead of base R, allows specification of the meta-analytic model as well as simulation using sample size per group.
#'
#' @param yi A numeric vector with effect estimates for each study. Odds/ risk ratios should be log-transformed.
#' @param sei A numeric vector with standard errors for each study. Must be specified. If 'sd' and 'n' are not specified the y-axis will correspond to the standard error.
#' @param sd A single numeric value corresponding to the assumed standard deviation per group of a future study. If 'sd' and 'n' are specified the y-axis will correspond to the sample size per group. Only works for mean differences.
#' @param n A numeric vector of the average sample size per group for each study. Only needed when 'sd' is specified.
#' @param swe A single numeric value corresponding to the smallest worthwhile effect. Odds/ risk ratios should be log-transformed.
#' @param method A character string indicating which method should be used to estimate tau. Default is "REML". See \link[metafor]{rma} of the metafor package for more options.
#' @param test A character string indicating which method should be use to calculate confidence intervals. Default is "z". \link[metafor]{rma} of the metafor package for more options.
#' @param contour_points A numeric value indicating the number of contour points to be used for simulation. Default is 50. For high quality contours 500 is recommend (this might take while!).
#' @param x_lim A numeric vector with two values indicating the lower and upper limits of the simulated effect estimates (corresponds to x-axis limits). Odds/ risk ratios should be log-transformed.
#' @param y_lim A numeric vector with two values indicating the lower and upper limits of the simulated standard errors (corresponds to y-axis limits). If 'sd' and 'n' are provided 'y_lim' corresponds to lower and upper limits of the sample size per group.
#' @param x_lab A character string indicating the x-axis label for the plot. Default is "Effect size".
#' @param y_lab A character string indicating the y-axis label for the plot. Default is "Standard error".
#' @param x_ticks A numeric vector indicating the tick marks for the x-axis. Default is NULL. Odds/ risk ratios should be log-transformed.
#' @param y_ticks A numeric vector indicating the tick marks for the y-axis. Default is NULL.
#' @param legend_pos A character string indicating the position of the legend in the plot. Default is "none". Can be "top", "right", "bottom", or "left".
#' @param exp A logical value indicating whether the x-axis should be on a logarithmic scale (in case odds/ risk ratios are provided). Default is FALSE.
#'
#' @import tidyverse metafor furrr
#'
#' @examples
#' # Load package once installed
#' library(extfunnel2)
#'
#' # Reproducing the extended funnel plot from Ferreira et al. 2012
#' # Random effects meta-analysis (effect of exercise on chronic low back pain)
#' data <- data.frame(yi = c(-10.0, -16.3, -27.0, -14.0, -15.1, -2.1, -19.0, -8.7),
#'                    sei = c(3.4, 5.8, 3.4, 5.3, 2.9, 1.1, 7.2, 4.2),
#'                    ni = c(174.0, 35.0, 25.0, 30.5, 13.5, 34.0, 15.0, 86.0))
#'
#' # Generate extended funnel plot
#' # Standard error on y-axis
#' extfunnel2(data$yi, data$sei,
#'            swe = -20,
#'            method = "DL",
#'            contour_points = 50,
#'            x_lim = c(-40, 10), y_lim = c(0, 15),
#'            x_ticks = seq(from = -40, to = 10, by = 10),
#'            y_ticks = seq(from = 0, to = 15, by = 3),
#'            x_lab = "Effect on pain (100 point scale)"
#'            )
#'
#'  # Sample size per group on y-axis
#'  # Assumptions: Equal group sizes and fixed SD
#'  extfunnel2(data$yi, data$sei,
#'             sd = 15, n = data$ni,
#'             swe = -20,
#'             method = "DL",
#'             contour_points = 50,
#'             x_lim = c(-40, 10), y_lim = c(10, 1000),
#'             x_ticks = seq(from = -40, to = 10, by = 10),
#'             y_ticks = seq(from = 0, to = 1000, by = 200),
#'             x_lab = "Effect on pain (100 point scale)"
#'             )
#'
#'@section References:
#'Langan D, Higgins JPT, Gregory W, et al. Graphical augmentations to the funnel plot assess the impact of additional evidence on a meta-analysis. J Clin Epidemiol 2012;65:511–9. \cr
#'\url{https://doi.org/10.1016/j.jclinepi.2011.10.009} \cr
#'
#'Ferreira ML, Herbert RD, Crowther MJ, et al. When is a further clinical trial justified? BMJ 2012;345:e5913. \cr
#'\url{https://doi.org/10.1136/bmj.e5913}
#'
#' @return An extended funnel plot (ggplot2 object) with shaded contours that show the impact of a new study with a certain effect estimate and standard error (or sample size) on the conclusions of an updated meta-analysis.
#' @export
extfunnel2 <- function(yi, sei = sei, sd = NULL, n = NULL, swe,
                       method = "REML", test = "z",
                       contour_points = 50,
                       x_lim, y_lim,
                       x_lab = "Effect size", y_lab = "Standard error",
                       x_ticks = NULL, y_ticks = NULL,
                       legend_pos = "none", exp = FALSE) {

  # if (!is.null(sei) & !is.null(sd) & !is.null(n)) {
  #   message("Note: Simulation with sample size works only for mean differences.")
  # }

  if (!is.null(sd) & !is.null(n)) {
    extfunnel2_n(yi = yi, sei = sei, method = method, test = test, contour_points = contour_points, swe = swe,
                 x_lim = x_lim, y_lim = y_lim, sd = sd, n = n, x_lab = x_lab, y_lab = y_lab, x_ticks =
                   x_ticks, y_ticks = y_ticks, legend_pos = legend_pos)
  } else{
  # Run meta-analysis with current studies
  res <- tidy_meta(rma.uni(yi = yi, sei = sei, method = method, test = test))


  ### Simulation

  # Create n = contour_points values from the lower to upper limit of the effect estimates (yi_sim) and standard errors (sei_sim)
  yi_sim <- seq(x_lim[1], x_lim[2], length.out = contour_points)
  sei_sim <- seq(y_lim[1], y_lim[2], length.out = contour_points)

  # Standard errors cannot be 0
  sei_sim[sei_sim <= 0] <- 0.0000001

  # Create all possible combinations of effect estimates and standard errors to map out the whole plot area
  df_sim <- expand_grid(yi_sim = yi_sim, sei_sim = sei_sim) %>%
    mutate(id = row_number()) %>%
    select(id, everything())

  # Use parallel processing for faster computation
  plan(multisession)

  # Combine every simulated effect estimate and standard error combination with the original effect estimates and standard errors
  df_comb <-
    future_pmap(df_sim, function(id, yi_sim, sei_sim) {
      tibble(
        id = id,
        yi_sim = c(yi, yi_sim),
        sei_sim = c(sei, sei_sim)
      )
    }) %>%
    list_rbind()

  plan(sequential)

  res_sim <- df_comb %>%
    group_by(id) %>%
    group_modify(
      .data = .,
      # Rerun meta-analysis for every combination of old studies + simulated new study
      .f = ~ suppressWarnings(tidy_meta(
        rma.uni(yi = .$yi_sim, sei = .$sei_sim, method = method, test = test, control = list(tau2.max = 10000))
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
    bind_cols(yi_sim = df_sim$yi_sim, sei_sim = df_sim$sei_sim, .)


  ### Create the extended funnel plot

  # Calculate y axis length
  y_axis_l <- y_lim[2] - y_lim[1]

  if (exp == FALSE) {
    plot <- ggplot(data = res_sim) +
      # Plot one rectangle for each simulated meta-analytic result (color depends on sig, see case_when above)
      geom_tile(aes(x = yi_sim, y = sei_sim, fill = sig)) +
      geom_hline(yintercept = y_ticks, color = "white") +
      # Add vertical line corresponding to the smallest worthwhile effect (swe)
      geom_vline(aes(xintercept = swe, color = "Smallest worthwhile effect"), linetype = "dashed") +
      # Add vertical line corresponding to the pooled estimate of the currently existing studies
      geom_vline(aes(xintercept = res$est, color = "Current pooled estimate"), linetype = "solid") +
      # Add points corresponding to point estimates of currently existing studies
      geom_point(
        data = data.frame(x = yi, y = sei),
        aes(x = x, y = y, shape = "Current point estimates"),
        size = 2.5, stroke = 0.7,
        fill = "#42727D",
        color = "#070F14"
      ) +
      labs(x = x_lab, y = y_lab) +
      scale_color_manual(values = c("Smallest worthwhile effect" = "black", "Current pooled estimate" = "black")) +
      scale_shape_manual(values = c("Current point estimates" = 21)) +
      scale_fill_manual(values = c("Clearly worthwhile" = "#bf32de", "Unclear if worthwhile" = "#f7766d", "Clearly not worthwhile" = "#03bfc4")) +
      theme_classic() +
      theme(legend.title = element_blank()) +
      guides(color = guide_legend(
        override.aes = list(
          linetype = c("solid", "dashed")
        )
      ))

    plot <- axis_limits(plot, x_lim, y_lim, x_ticks, y_ticks)
    # Back transform log scale
  } else {
    plot <- ggplot(data = res_sim) +
      # Plot one rectangle for each simulated meta-analytic result (color depends on sig, see case_when above)
      geom_tile(aes(x = exp(yi_sim), y = sei_sim, fill = sig)) +
      geom_hline(yintercept = y_ticks, color = "white") +
      # Add vertical line corresponding to the smallest worthwhile effect (swe)
      geom_vline(aes(xintercept = exp(swe), color = "Smallest worthwhile effect"), linetype = "dashed") +
      # Add vertical line corresponding to the pooled estimate of the currently existing studies
      geom_vline(aes(xintercept = exp(res$est), color = "Current pooled estimate"), linetype = "solid") +
      # Add points corresponding to point estimates of currently existing studies
      geom_point(
        data = data.frame(x = exp(yi), y = sei),
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

    plot <- axis_limits_exp(plot, x_lim, y_lim, x_ticks, y_ticks)
  }

  plot +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_line(color = "white"),
      axis.text = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0), size = 10, color = "black"),
      axis.ticks.length = unit(-0.12, "cm"),
      axis.title = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(hjust = 1, margin = margin(t = 10)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = 12)),
      legend.position = legend_pos
    )
  }
}
