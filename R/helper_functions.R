#' Estimate Bias
#'
#' Returns the bias based on the different parameters in the bias decomposition
#' 
#' @param rho Correlation between the error in the weights and outcome
#' @param R2 R2 measure for how much variation in the true weights is explained by the error term, must be bound on the range [0,1)
#' @param weights Vector of estimated weights
#' @param sigma2 Estimated variance of the outcome (i.e., stats::var(Y) for obervational setting; stats::var(tau) for generalization setting)
#' 
#' @return Estimated bias from omitting a confounder from weights
#' @export
#' 
#' @examples
#' # TODO
#' 
estimate_bias <- function(rho, R2, weights, sigma2) {
  if (R2 >= 1 || R2 < 0) {
    return("R2 must be bound on interval [0,1)")
  }
  var_eps <- stats::var(weights) * R2 / (1 - R2)
  return(rho * sqrt(var_eps * sigma2))
}

#' Generate Correlation
#'
#' Returns the correlation between the error and the weights, using a user-inputted \code{R2} value, the correlation between the estimated weights and outcomes, and a scaling factor
#' 
#' @param rho_w Estimated correlation between the estimated weights and the outcomes
#' @param k Scaling factor, denoting how many times larger the correlation between the true weights and the outcomes are, relative to the correlation between the estimated weights and the outcomes
#' @param R2 R^2 measure for how much variation in the true weights is explained by the error term, must be bound on the range \code{[0,1)}
#' 
#' @return Correlation between the error and the weights, based on a user-input \code{R2} value and \code{stats::cor(w,Y)}
#' @export
#' 
#' @examples
#' # TODO
generate_rho <- function(rho_w, k, R2) {
  if (R2 >= 1 || R2 < 0) {
    return("R2 must be bound on interval [0,1)")
  }
  return(rho_w * sqrt((1 - R2) / R2) - (rho_w * k) / sqrt(R2))
}

#' Calculate extreme scenario
#'
#' Helper function for running an extreme scenario analysis
#' 
#' @param rho_w Estimated correlation between the estimated weights and the outcomes+
#' @param weights Vector of estimated weights
#' @param sigma2 Variance of the outcomes
#' @param correlations A vector containing possible correlation values between the true weights and the outcomes
#' 
#' @return A data frame that can be used to generate the extreme scenario plots
#' @export
#' 
#' @examples
#' # TODO
calculate_extreme_scenario <- function(rho_w, weights, sigma2, correlations = c(0.25, 0.5, 0.9, 1)) {
  R2_vals <- seq(0.01, 0.99, by = 0.01)
  cor_values <- list(NULL)
  i <- 1
  for (rho in c(-correlations, correlations)) {
    cor_values[[i]] <- sapply(R2_vals, generate_rho, rho_w = rho_w, k = rho / rho_w)
    i <- i + 1
  }
  values <- c(-correlations, correlations)
  names(cor_values) <- paste(values)
  df_plot <- lapply(
    1:length(cor_values),
    function(x) {
      data.frame(
        type = values[x], R2_vals,
        bias = estimate_bias(unlist(cor_values[x]), R2_vals, weights, sigma2),
        rho = unlist(cor_values[x])
      )
    }
  ) |> dplyr::bind_rows()

  df_plot <- df_plot[which(abs(df_plot$rho) < sqrt(1 - rho_w^2)), ]

  find_max <- df_plot |>
    dplyr::group_by(.data$type) |>
    dplyr::summarize(
      val = max(.data$R2_vals)
    )

  df_plot$label <- NA
  for (i in 1:nrow(find_max)) {
    df_plot$label[df_plot$type == find_max$type[i] & df_plot$R2 == find_max$val[i]] <-
      paste(find_max$type[i])
  }
  df_plot$type_abs <- paste(abs(as.numeric(paste(df_plot$type))))
  df_plot$type_abs <- as.factor(df_plot$type_abs)
  df_plot$type_abs <- factor(df_plot$type_abs, levels = rev(levels(df_plot$type_abs)))
  
  levels(df_plot$type_abs) <- paste(abs(df_plot$type)[order(-abs(df_plot$type))])

  return(df_plot)
}
