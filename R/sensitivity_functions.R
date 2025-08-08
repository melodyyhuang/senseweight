#' Robustness Value
#'
#' Returns the estimated robustness value, for a specified proportion change
#' 
#' @param b_star Threshold that corresponds to a substantively meaningful change to the research conclusion. For example, a value of 0 denotes that the bias from omitting a variable was sufficiently large to change the estimate to zero.
#' @param estimate Weighted estimate
#' @param sigma2 Estimated variance of the outcome (i.e., stats::var(Y) for obervational setting; stats::var(tau) for generalization setting)
#' @param weights Vector of estimated weights
#' 
#' @return Robustness value for a specified proportion change
#' @export
#' 
#' @examples
#' data(jtpa_women)
#' site_name <- "NE"
#' df_site <- jtpa_women[which(jtpa_women$site == site_name), ]
#' df_else <- jtpa_women[which(jtpa_women$site != site_name), ]
#' 
#' # Estimate unweighted estimator:
#' model_dim <- estimatr::lm_robust(Y ~ T, data = df_site)
#' PATE <- coef(lm(Y ~ T, data = df_else))[2]
#' DiM <- coef(model_dim)[2]
# 
#' # Generate weights using observed covariates:
#' df_all <- jtpa_women
#' df_all$S <- ifelse(jtpa_women$site == "NE", 1, 0)
#' model_ps <- WeightIt::weightit(
#'   (1 - S) ~ . - site - T - Y, 
#'   data = df_all, method = "ebal", estimand = "ATT"
#' )
#' weights <- model_ps$weights[df_all$S == 1]
# 
#' # Estimate IPW model:
#' model_ipw <- estimatr::lm_robust(Y ~ T, data = df_site, weights = weights)
#' ipw <- coef(model_ipw)[2]
# 
#' # Estimate bound for var(tau):
#' vartau <- var(df_site$Y[df_site$T == 1]) - var(df_site$Y[df_site$T == 0])
#' RV <- robustness_value(estimate = ipw, b_star = 0, sigma2 = vartau, weights = weights)
#' 
#' print(RV)
robustness_value <- function(estimate, b_star = 0, sigma2, weights) {
  a <- (estimate - b_star)^2 / (sigma2 * stats::var(weights))
  RV_est <- (sqrt(a^2 + 4 * a) - a) / 2
  names(RV_est) <- NULL
  return(RV_est)
}


#' Helper function for benchmarking for sensitivity parameters
#'
#' Helper function that returns parameter estimates for an omitted variable with specified relative confounding strength to an observed covariate (or set of covariates)
#' 
#' @param weights Vector of estimated weights
#' @param weights_benchmark Vector of estimated weights, omitting the covariate (or set of covariates) being used to benchmark
#' @param k_sigma Relative ability of omitted confounder to explain variation in the true weights. If \code{k_sigma > 1}, then we expect the omitted confounder to explain more variation in the true weights than the benchmarked covariate(s). If \code{k_sigma < 1}, then we expect the omitted confounder to explain less of the variation in the true weights than the benchmarked covariate(s). Default is set to 1.
#' @param k_rho Relative correlation of omitted confounder with the outcome. If \code{k_rho > 1}, then we expect the omitted confounder to be more correlated with the outcome than the benchmarked covariate(s). If \code{k_rho < 1}, then we expect the omitted confounder to be less correlated with the outcome than the benchmarked covariate(s). Default is set to 1.
#' @param Y Vector of outcomes
#' @param Z Vector of treatment assignment (Only necessary if \code{estimand = "PATE"} in order to estimate covariances)
#' @param sigma2 Estimated variance of the outcome (i.e., stats::var(Y) for obervational setting; stats::var(tau) for generalization setting)
#' @param estimand String specifying the estimand of interest. Valid inputs are "PATE", "Augmented", "ATT", or "Survey".
#' 
#' @return \code{data.frame} containing estimated parameter values for a confounder with specified relative confounder strength to an observed covariate (or set of covariates), as well as the estimated bias from such an omitted confounder.
#' @keywords internal
#' 
#' @examples
#' data(jtpa_women)
#' site_name <- "NE"
#' df_site <- jtpa_women[which(jtpa_women$site == site_name), ]
#' df_else <- jtpa_women[which(jtpa_women$site != site_name), ]
#' 
#' # Estimate unweighted estimator:
#' model_dim <- estimatr::lm_robust(Y ~ T, data = df_site)
#' PATE <- coef(lm(Y ~ T, data = df_else))[2]
#' DiM <- coef(model_dim)[2]
# 
#' # Generate weights using observed covariates:
#' df_all <- jtpa_women
#' df_all$S <- ifelse(jtpa_women$site == "NE", 1, 0)
#' model_ps <- WeightIt::weightit(
#'   (1 - S) ~ . - site - T - Y, 
#'   data = df_all, method = "ebal", estimand = "ATT"
#' )
#' weights <- model_ps$weights[df_all$S == 1]
# 
#' # Estimate IPW model:
#' model_ipw <- estimatr::lm_robust(Y ~ T, data = df_site, weights = weights)
#' ipw <- coef(model_ipw)[2]
# 
#' # Estimate bound for var(tau):
#' vartau <- var(df_site$Y[df_site$T == 1]) - var(df_site$Y[df_site$T == 0])
#' RV <- robustness_value(estimate = ipw, b_star = 0, sigma2 = vartau, weights = weights)
#' 
#' # Select weighting variables:
#' weighting_vars <- names(df_all)[which(!names(df_all) %in% c("site", "S", "Y", "T"))]
#' 
#' # Run benchmarking:
#' df_benchmark <- run_benchmarking(
#'   weighting_vars = weighting_vars,
#'   data = df_all[, -1],
#'   treatment = "T", outcome = "Y", selection = "S",
#'   estimate = ipw,
#'   RV = RV, sigma2 = vartau,
#'   estimand = "PATE"
#' )
#' 
#' print(df_benchmark)
#' 
benchmark_parameters <- function(weights, weights_benchmark, k_sigma = 1, k_rho = 1,
                                 Y, Z, sigma2, estimand = "ATT") {
  # Estimate informal calibrated components
  eps_benchmark <- weights_benchmark - weights
  R2 <- stats::var(eps_benchmark) / stats::var(weights)
  rho <- stats::cor(Y, eps_benchmark)
  
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
#' 
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
#' 
#' @return A ggplot2 object, containing the bias contour plot
#' @export
#' 
#' @examples
#' # TODO
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
        rho_benchmark = ifelse(abs(.data$rho_benchmark > 1), NA, .data$rho_benchmark)
      )
  }
  plt <- df_plot |>
    ggplot2::ggplot(ggplot2::aes(x = .data$R2, y = .data$rho, z = .data$bias)) +
    ggplot2::labs(
      x = expression(R[epsilon]^2),
      y = expression(rho[epsilon * "," * tau])
    )
  if (is.null(binwidth) || is.na(binwidth)) {
    plt <- plt +
      ggplot2::geom_contour(col = "black", size = contour_width * 0.6) +
      metR::geom_text_contour(
        ggplot2::aes(z = .data$bias), stroke = contour_stroke
      )
  } else {
    plt <- plt +
      ggplot2::geom_contour(
        col = "black", binwidth = binwidth, size = contour_width * 0.6
      ) +
      metR::geom_text_contour(
        ggplot2::aes(z = .data$bias),
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
            x = .data$R2_benchmark,
            y = .data$rho_benchmark,
            color = (.data$variable %in% shade_var)
          ),
          data = df_benchmark,
          size = point_size
        ) +
        ggrepel::geom_label_repel(
          ggplot2::aes(
            x = .data$R2_benchmark, y = .data$rho_benchmark, label = .data$variable,
            color = (.data$variable %in% shade_var)
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
          ggplot2::aes(x = .data$R2_benchmark, y = .data$rho_benchmark),
          data = df_benchmark,
          size = point_size
        ) +
        ggrepel::geom_label_repel(
          ggplot2::aes(x = .data$R2_benchmark, y = .data$rho_benchmark, label = .data$variable),
          data = df_benchmark,
          size = label_size,
          nudge_y = nudge
        )
    }
  }
  
  plt <- plt +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(color = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = axis_text_size),
      axis.title = ggplot2::element_text(size = axis_title_size, face = "bold"),
      axis.line = ggplot2::element_line(linewidth = axis_line_width, color = "black")
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
