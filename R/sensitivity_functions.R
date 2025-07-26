#' Robustness Value
#'
#' Returns the estimated robustness value, for a specified proportion change
#' @param b_star Threshold that corresponds to a substantively meaningful change to the research conclusion. For example, a value of 0 denotes that the bias from omitting a variable was sufficiently large to change the estimate to zero.
#' @param estimate Weighted estimate
#' @param sigma2 Estimated variance of the outcome (i.e., stats::var(Y) for obervational setting; stats::var(tau) for generalization setting)
#' @param weights Vector of estimated weights
#' @return Robustness value for a specified proportion change
#' @export
robustness_value <- function(estimate, b_star = 0, sigma2, weights) {
  a <- (estimate - b_star)^2 / (sigma2 * stats::var(weights))
  RV_est <- (sqrt(a^2 + 4 * a) - a) / 2
  names(RV_est) <- NULL
  return(RV_est)
}


#' Formal benchmarking for sensitivity parameters
#'
#' Returns parameter estimates for an omitted variable with specified relative confounding strength to an observed covariate (or set of covariates)
#' @param weights Vector of estimated weights
#' @param weights_benchmark Vector of estimated weights, omitting the covariate (or set of covariates) being used to benchmark
#' @param k_sigma Relative ability of omitted confounder to explain variation in the true weights. If \code{k_sigma > 1}, then we expect the omitted confounder to explain more variation in the true weights than the benchmarked covariate(s). If \code{k_sigma < 1}, then we expect the omitted confounder to explain less of the variation in the true weights than the benchmarked covariate(s). Default is set to 1.
#' @param k_rho Relative correlation of omitted confounder with the outcome. If \code{k_rho > 1}, then we expect the omitted confounder to be more correlated with the outcome than the benchmarked covariate(s). If \code{k_rho < 1}, then we expect the omitted confounder to be less correlated with the outcome than the benchmarked covariate(s). Default is set to 1.
#' @param Y Vector of outcomes
#' @param Z Vector of treatment assignment (Only necessary if \code{estimand = "PATE"} in order to estimate covariances)
#' @param sigma2 Estimated variance of the outcome (i.e., stats::var(Y) for obervational setting; stats::var(tau) for generalization setting)
#' @param estimand String specifying the estimand of interest. Valid inputs are "PATE", "Augmented", "ATT", or "Survey".
#' @return \code{data.frame} containing estimated parameter values for a confounder with specified relative confounder strength to an observed covariate (or set of covariates), as well as the estimated bias from such an omitted confounder.
#' @export
benchmark_parameters <- function(weights, weights_benchmark, k_sigma = 1, k_rho = 1,
                                 Y, Z, sigma2, estimand = "ATT") {
  # Estimate informal calibrated components
  eps_benchmark <- weights_benchmark - weights
  R2 <- stats::var(eps_benchmark) / stats::var(weights)
  if (estimand == "PATE") {
    cov_w_calib <- stats::cov(Y[Z == 1], weights_benchmark[Z == 1]) -
      stats::cov(Y[Z == 0], weights_benchmark[Z == 0])
    cov_w <- stats::cov(Y[Z == 1], weights[Z == 1]) - stats::cov(Y[Z == 0], weights[Z == 0])
    cov_benchmark <- cov_w_calib - cov_w
    rho <- cov_benchmark / (sqrt(sigma2 * stats::var(eps_benchmark)))
  } else {
    if (!estimand %in% c("Augmented", "ATT", "Survey")) {
      return("Invalid Estimand. Estimand must be ATT, PATE, or Survey.")
    }
    # Estimand is ATT or Survey:
    rho <- stats::cor(Y, eps_benchmark)
  }

  # Calculate parameters:
  R2_benchmark <- k_sigma * R2 / (1 + k_sigma * R2)
  rho_benchmark <- k_rho * rho

  if (abs(rho_benchmark) > 1) {
    return("Infeasible correlation value obtained.")
  }
  # Expected bias:
  bias <- rho_benchmark * sqrt(stats::var(weights) * R2_benchmark / (1 - R2_benchmark)) * sqrt(sigma2)

  return(data.frame(R2_benchmark, rho_benchmark, bias))
}


#' Bias Contour Plots
#'
#' Generates bias contour plots to aid with sensitivity analysis
#' @param varW Variance of the estimated weights
#' @param sigma2 Estimated variance of the outcome (i.e., stats::var(Y) for obervational setting; stats::var(tau) for generalization setting)#'
#' @param killer_confounder Threshold for bias considered large enough to be a killer confounder. For example, if researchers are concerned about the bias large enough to reduce an estimated treatment effect to zero or change directional sign, set \code{killer_confounder} equal to the point estimate.
#' @param df_benchmark Data frame containing formal benchmarking results. The data.frame  must contain the columns \code{variable} (for the covariate name), \code{R2_benchmark}, and \code{rho_benchmark}.
#' @param benchmark Flag for whether or not to display benchmarking results (\code{benchmark = TRUE} if we want to add benchmarking results to plot, \code{benchamrk=FALSE} otherwise). If set to \code{TRUE}, \code{df_benchmark} must contain valid benchmarking results.
#' @param shade Flag for whether or not a specific benchmarking covariate (or set of benchmarked covariates) should be shaded a different color (\code{shade = TRUE} indicates that we want to highlight specific variables)
#' @param shade_var If \code{shade = TRUE}, this contains either a vector containing the variables we want to highlight
#' @param shade_fill Color to fill the highlighted variables. Default is set to \code{"#35a4bf"}.
#' @param shade_alpha Alpha value for the fill color. Default is set to \code{0.25}.
#' @param contour_width Width of the contour lines. Default is set to \code{1}.
#' @param binwidth If set to a numeric value, the function will generate a contour plot with the specified binwidth. Default is set to \code{NULL}.
#' @param label_size Size of the labels. Default is set to \code{0.25}.
#' @param point_size Size of the points. Default is set to \code{1}.
#' @param nudge Nudge value for the labels. Default is set to \code{0.05}.
#' @param axis_text_size Size of the axis text. Default is set to \code{12}.
#' @param axis_title_size Size of the axis title. Default is set to \code{14}.
#' @param axis_line_width Width of the axis lines. Default is set to \code{1}.
#' @param print If set to \code{TRUE}, the function will return a list with two elements: \code{plot} which contains the generated bias contour plot, and \code{data}, which provides the data.frame for generating the contour plot. If set to \code{FALSE}, the function will simply generate the bias contour plot. Default is set to \code{FALSE}.
#' @return A ggplot2 object, containing the bias contour plot
#' @export
contour_plot <- function(varW, sigma2, killer_confounder, df_benchmark,
                         benchmark = TRUE, shade = FALSE, shade_var = NULL,
                         shade_fill = "#35a4bf", shade_alpha = 0.25,
                         contour_width = 1, binwidth = NULL,
                         label_size = 0.25, point_size = 1, nudge = 0.05,
                         axis_text_size = 12, axis_title_size = 14,
                         axis_line_width = 1, print = FALSE) {
  contour_stroke <- 0.2
  r2_vals <- seq(0, 0.95, by = 0.05)
  rho_vals <- seq(-1, 1, by = 0.05)
  data.fit <- expand.grid(rho_vals, r2_vals)
  names(data.fit) <- c("rho", "r2")
  bias <- data.fit$rho * sqrt(varW * (data.fit$r2 / (1 - data.fit$r2))) *
    sqrt(sigma2)
  df_plot <- data.frame(
    R2 = data.fit$r2,
    rho = data.fit$rho,
    bias = bias
  )
  if (benchmark) {
    df_benchmark_plot <- df_benchmark |>
      dplyr::mutate(
        rho_benchmark = ifelse(abs(rho_benchmark > 1), NA, rho_benchmark)
      )
  }
  plt <- df_plot |>
    ggplot2::ggplot(ggplot2::aes(x = R2, y = rho, z = bias)) +
    ggplot2::labs(
      x = expression(R[epsilon]^2),
      y = expression(rho[epsilon * "," * tau])
    )
  if (is.null(binwidth) || is.na(binwidth)) {
    plt <- plt +
      ggplot2::geom_contour(col = "black", size = contour_width * 0.6) +
      metR::geom_text_contour(
        ggplot2::aes(z = bias), stroke = contour_stroke
      )
  } else {
    plt <- plt +
      ggplot2::geom_contour(
        col = "black", binwidth = binwidth, size = contour_width * 0.6
      ) +
      metR::geom_text_contour(
        ggplot2::aes(z = bias),
        stroke = contour_stroke, binwidth = binwidth
      )
  }
  if (!is.na(killer_confounder)) {
    plt <- plt +
      metR::geom_contour_fill(
        breaks = c(killer_confounder, 1000 * killer_confounder),
        fill = shade_fill, alpha = shade_alpha
      ) +
      ggplot2::geom_contour(
        breaks = c(killer_confounder), col = shade_fill, size = contour_width
      )
  }
  if (benchmark) {
    if (shade) {
      plt <- plt +
        ggplot2::geom_point(
          ggplot2::aes(
            x = R2_benchmark,
            y = rho_benchmark,
            color = (variable %in% shade_var)
          ),
          data = df_benchmark,
          size = point_size
        ) +
        ggrepel::geom_label_repel(
          ggplot2::aes(
            x = R2_benchmark, y = rho_benchmark, label = variable,
            color = (variable %in% shade_var)
          ),
          size = label_size,
          nudge_y = nudge,
          fill = "white",
          data = df_benchmark
        ) +
        ggplot2::scale_colour_manual(
          values = c("slategray", "black")
        ) +
        ggplot2::theme(legend.position = "none")
    } else {
      plt <- plt +
        ggplot2::geom_point(
          ggplot2::aes(x = R2_benchmark, y = rho_benchmark),
          data = df_benchmark,
          size = point_size
        ) +
        ggrepel::geom_label_repel(
          ggplot2::aes(x = R2_benchmark, y = rho_benchmark, label = variable),
          data = df_benchmark,
          size = label_size,
          nudge_y = nudge
        )
    }
  }
  
  plt <- plt +
    ggplot2::guides(color = "none") +
    vthemes::theme_vmodern(
      bg_color = "white",
      grid_color = "white",
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      axis_line_width = axis_line_width
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::scale_y_continuous(expand = c(0.01, 0.01))
  if (print) {
    return(list(plot = plt, data = df_plot))
  } else {
    return(plt)
  }
}


#' Extreme Scenario Plots
#'
#' Generates extreme scenario plots, with varying thresholds for the correlation between the true weights and the outcomes
#' @param rho_w Correlation between the estimated weights and the outcomes
#' @param sigma2 Estimated variance of the outcome (i.e., stats::var(Y) for obervational setting; stats::var(tau) for generalization setting)#'
#' @param estimate Weighted estimate
#' @param correlations A vector containing possible correlation values between the true weights and the outcomes
#' @return A ggplot2 object, genearting an extreme scenario analysis
#' @export
extreme_scenario_plot <- function(rho_w, sigma2, estimate, correlations = c(0.25, 0.5, 0.9, 1)) {
  df_plot <- calculate_extreme_scenario(rho_w, sigma2, correlations)
  p1 <- df_plot[which(estimate - df_plot$bias >= 0), ] |>
    ggplot2::ggplot(ggplot2::aes(
      x = R2_vals, y = estimate - bias, label = label,
      group = type, linetype = as.factor(abs(type))
    )) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, alpha = 0.5) +
    ggplot2::geom_text(nudge_x = 0.035, size = 3) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_line(
      data = df_plot[which(estimate - df_plot$bias < 0), ],
      ggplot2::aes(
        x = R2_vals, y = estimate - bias,
        group = type, linetype = as.factor(abs(type))
      ), color = "red"
    ) +
    ggplot2::geom_text(
      data = df_plot[which(estimate - df_plot$bias < 0), ], 
      ggplot2::aes(label = label),
      color = "black", nudge_x = 0.035, size = 3
    ) +
    ggplot2::xlab(expression(R[epsilon]^2)) +
    ggplot2::ylab("Adjusted Point Estimate (Estimate - Est. Bias)") +
    ggplot2::ggtitle("Weighted Estimator") +
    ggplot2::ylim(-25, 25)
  return(p1)
}
