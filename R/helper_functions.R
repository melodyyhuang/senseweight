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
#' set.seed(331)
#' Y = rnorm(1000)
#' weights = rlogis(1000)
#' weights = weights/mean(weights)
#' estimate_bias(rho = 0.5, R2 = 0.5, weights = weights, sigma2 = var(Y))
estimate_bias <- function(rho, R2, weights, sigma2) {
  var_eps <- stats::var(weights) * R2 / (1 - R2)
  return(rho * sqrt(var_eps * sigma2))
}

#' Helper function for creating targets from auxiliary information and formula
#' 
#' Returns weighting targets for survey objects. 
#' 
#' @param target_design A survey object 
#' @param target_formula A formula object that contains the variables to weight on
#' 
#' @return Weighting target for survey objects
#' @export
#' 
#' @examples
#' data(ces)
#' ces_awt = survey::svydesign(ids = ~ 1,
#'                      weights = ~ vvweight_post,
#'                      data = ces)
#' 
#' #Set up raking formula:
#' formula_rake = ~ age_buckets + educ + gender + race + educ * pid + bornagain
#' 
#' #Generate targets:
#' targets_rake = create_targets(ces_awt, formula_rake)
#' 
create_targets <- function(target_design, target_formula) {
  target_mf <- stats::model.frame(target_formula, stats::model.frame(target_design))
  target_mm <- stats::model.matrix(target_formula, target_mf)
  wts <- stats::weights(target_design)
  if (all(wts == 1)) {
    return(colMeans(target_mm))
  } else {
    return(colSums(target_mm * wts) / sum(wts))
  }
}

#' Helper function to benchmark an instance
#'
#' Returns the benchmarking results for a single covariate (or a single group of covariates)
#' 
#' @param omit Variable to omit
#' @param weights Vector of estimated weights
#' @param data data.frame containing outcomes and covariate information
#' @param sigma2 Variance of the outcome variable 
#' @param weighting_method Weighting method. Supports weighting methods from the package \code{WeightIt}.
#' @param weight_max Maximum weight to trim at. Default set to \code{Inf}
#' @param estimand Specifies estimand; possible parameters include "ATT" or "PATE",
#' 
#' @return Benchmarking results for a single covariate
#' @keywords internal
benchmark <- function(omit, weights, data, sigma2,
                      estimand = "ATT",
                      weighting_method = "ebal",
                      weight_max = Inf) {
  data_benchmark <- data |> dplyr::select(-omit)
  model_weights <- WeightIt::weightit(missing ~ . - selection - treatment - outcome,
                                      data = data_benchmark,
                                      method = weighting_method, estimand = "ATT"
  )
  weights_benchmark <- model_weights$weights
  weights_benchmark <- stats::weights(
    survey::trimWeights(survey::svydesign(~1, data = data_benchmark, weights = weights_benchmark),
                        upper = weight_max
    )
  )
  return(data.frame(
    variable = omit,
    benchmark_parameters(weights[data$missing == 0] / mean(weights[data$missing == 0]),
                         weights_benchmark[data$missing == 0] / mean(weights_benchmark[data$missing == 0]),
                         data$outcome[data$missing == 0],
                         data$treatment[data$missing == 0],
                         sigma2,
                         k_sigma = 1, k_rho = 1,
                         estimand = estimand
    )
  ))
}
