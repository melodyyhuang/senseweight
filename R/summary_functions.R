#' Benchmark an instance
#'
#' Returns the benchmarking results for a single covariate (or a single group of covariates)
#' @param omit Variable to omit
#' @param weights Vector of estimated weights
#' @param data data.frame containing outcomes and covariate infromation
#' @param weighting_method Weighting method. Supports weighting methods from the package \code{WeightIt}.
#' @param weight_max Maximum weight to trim at. Default set to \code{Inf}
#' @param estimand Specifies estimand; possible parameters include "ATT", "PATE", or "Survey"
#' @return Robustness value for a specified proportion change
#' @export
benchmark <- function(omit, weights, data, sigma2,
                      weighting_method = "ebal",
                      weight_max = Inf, estimand = "ATT") {
  data_benchmark <- data %>% dplyr::select(-omit)
  model_weights <- WeightIt::weightit(missing ~ . - selection - treatment - outcome,
    data = data_benchmark,
    method = weighting_method, estimand = "ATT"
  )
  weights_benchmark <- model_weights$weights
  weights_benchmark <- weights(
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

#' Run Formal Benchmarking
#'
#' Wrapper function to run formal benchmarking on a set of pre-specified covariates. Returns a data.frame containing the benchmarked parameter values, the estimated bias, MRCS, and minimum \code{k_sigma} and \code{k_rho} values for a killer confounder.
#' @param weighting_vars Vector of variables to use in the weights estimation
#' @param benchmark_vars Vector of variables to benchmark parameters for. If \code{benchmark_vars = 'all'}, benchmarking will be run across all variables included in the weights. If not set to \code{all}, benchmarking will be conducted across the covariates included in the vector.
#' @param data A data.frame containing the observed covariates included in the weights; must include variables specified in weighting_vars
#' @param treatment Denotes which variable is the treatment variable
#' @param outcome Denotes which variable is the outcome variable
#' @param selection Denotes which variable is the selection variable
#' @param weighting_method Weighting method. Supports weighting methods from the package \code{WeightIt}.
#' @param weight_max Maximum weight to trim at. Default set to \code{Inf}.
#' @param estimate Weighted estimate
#' @param k_sigma Relative ability of omitted confounder to explain variation in the true weights. If \code{k_sigma > 1}, then we expect the omitted confounder to explain more variation in the true weights than the benchmarked covariate(s). If \code{k_sigma < 1}, then we expect the omitted confounder to explain less of the variation in the true weights than the benchmarked covariate(s). Default is set to 1.
#' @param k_rho Relative correlation of omitted confounder with the outcome. If \code{k_rho > 1}, then we expect the omitted confounder to be more correlated with the outcome than the benchmarked covariate(s). If \code{k_rho < 1}, then we expect the omitted confounder to be less correlated with the outcome than the benchmarked covariate(s). Default is set to 1.
#' @param RV Robustness Value
#' @param sigma2 If \code{estimand = "PATE"}, \code{sigma2} must specify the bound on treatment effect heterogeneity. For the other two estimands, the function will automatically calculate the sample variance across the control units, or the survey sample.
#' @param estimand Specifies estimand; possible parameters include "ATT", "PATE", or "Survey"
#' @return data.frame containing the benchmarked parameter values, the estimated bias, MRCS, and minimum \code{k_sigma} and \code{k_rho} values for a killer confounder for the set of pre-specified covariates.
#' @export
run_benchmarking <- function(weighting_vars, benchmark_vars = "all",
                             data, treatment = Z, outcome = Y, selection = S,
                             weighting_method = "ebal", weight_max = Inf,
                             estimate, RV, sigma2, estimand = "ATT") {
  names(data)[which(names(data) == paste(outcome))] <- "outcome"
  names(data)[which(names(data) == paste(treatment))] <- "treatment"
  if (estimand == "ATT") {
    sigma2 <- var(data$outcome[data$treatment == 0])
    data$missing <- data$treatment
    data$selection <- NA
  }
  if (estimand %in% c("PATE", "Survey")) {
    names(data)[which(names(data) == paste(selection))] <- "selection"
    if (estimand == "PATE" & is.null(sigma2)) {
      return("Error: Must specify bound for treatment effect heterogeneity.")
    }
    data$missing <- 1 - data$selection
  }
  keep_covariates <- append(c("outcome", "treatment", "selection", "missing"), weighting_vars)
  data <- data %>% dplyr::select(keep_covariates)
  model_weights <- WeightIt::weightit(missing ~ . - selection - treatment - outcome,
    data = data,
    method = weighting_method, estimand = "ATT"
  )
  weights <- model_weights$weights
  if (benchmark_vars == "all") {
    benchmark_vars <- weighting_vars
  }
  df_benchmark <- data.frame(
    lapply(benchmark_vars, benchmark,
      weights = weights, data = data, sigma2 = sigma2,
      weighting_method = weighting_method, weight_max = weight_max, estimand = estimand
    ) %>%
      dplyr::bind_rows()
  )
  df_benchmark$MRCS <- estimate / df_benchmark$bias
  df_benchmark$k_sigma_min <- RV / df_benchmark$R2_benchmark
  df_benchmark$k_rho_min <- sqrt(RV) / df_benchmark$rho_benchmark
  df_benchmark <- df_benchmark %>%
    dplyr::mutate_if(is.numeric, round, 2)
  return(df_benchmark)
}

#' Sensitivity Summary
#'
#' Returns a data.frame or Kable table with summary measures of sensitivity
#' @param weights Vector of estimated weights
#' @param Y Outcome of interest
#' @param Z Treatment assignment
#' @param q Proportion to evaluate roustness value at. Default at \code{q=1}
#' @param estimate (Optional) Weighted point estimate. If not specified, function will automatically generate the weighted estimator, given the inputs in \code{Y} and \code{weights}
#' @param SE (Optional) Standard error associated with the weighted point estimate
#' @param unweighted (Optional) Unweighted point estimate.
#' @param sigma2 (Optional) Variance of outcomes or individual-level treatment effect in PATE case. In the case of a PATE estimator, if not specified, function will automatically estimate an upper bound for the variance of the individual-level treatment effect.
#' @param estimand Specifies estimand; possible parameters include "ATT", "PATE", or "Survey"
#' @param pretty If set to \code{TRUE}, will return a Kable table. If set to \code{FALSE}, will return a data.frame.
#' @return Sensitivity summary
#' @export
summarize_sensitivity <- function(weights, Y, Z, b_star = 0,
                                  estimate = NULL, SE = NULL, unweighted = NULL,
                                  sigma2 = NULL, estimand = "ATT", pretty = FALSE,
                                  dependent_var = NULL, model = NULL, outcome_function = NULL,
                                  svy_srs = NULL, svy_wt = NULL) {
  if (estimand == "Survey") {
    if (!is.null(model)) {
      return(summarize_sensitivity_survey(svy_srs, svy_wt, weights, var(Y), b_star))
    } else {
      sigma2 <- var(Y)
      RV <- robustness_value(estimate = estimate - b_star, sigma2 = sigma2, weights = weights)
      df_summary <- data.frame(
        Unweighted = round(unweighted, 2),
        Estimate = round(estimate, 2),
        SE = round(SE, 2),
        RV = round(RV, 2)
      )
      return(data.frame(df_summary, sigma_Y = sigma2, cor_w = round(cor(weights, Y))))
    }
  }
  if (is.null(unweighted)) {
    model_DiM <- estimatr::lm_robust(Y ~ Z)
    DiM <- coef(model_DiM)[2]
    names(DiM) <- NULL
  }
  if (is.null(estimate)) {
    model_ipw <- estimatr::lm_robust(Y ~ Z, weights = weights)
    estimate <- coef(model_ipw)[2]
    names(estimate) <- NULL
  }
  if (is.null(sigma2)) {
    if (estimand == "PATE") {
      sigma2 <- var(Y[Z == 1]) + var(Y[Z == 0])
    } else {
      # Estimand is ATT (or Survey Mean)
      sigma2 <- var(Y[Z == 0])
    }
  }
  # Estimate correlation:
  if (estimand == "PATE") {
    cov_w <- cov(Y[Z == 1], weights[Z == 1]) -
      cov(Y[Z == 0], weights[Z == 0])
    cor_w <- cov_w / sqrt(var(weights) * sigma2)
  } else {
    # Estimand is ATT (or Survey Mean)
    cor_w <- cor(weights[Z == 0], Y[Z == 0])
  }
  # Calculate Robustness Value:
  RV <- robustness_value(estimate, b_star, sigma2, weights[Z == 0])
  df_summary <- data.frame(
    Unweighted = round(DiM, 2), Unweighted_SE = round(model_DiM$std.error[2], 2),
    Estimate = round(estimate, 2), SE = round(model_ipw$std.error[2], 2),
    RV = round(RV, 2)
  )

  if (estimand == "PATE") {
    output <- paste("$\\\\widehat \\\\sigma_{\\\\tau, max}=$", round(sqrt(sigma2), 2),
      "$\\\\widehat{cor}(w, \\\\tau) =$", round(cor_w, 2),
      sep = ", "
    )
  } else {
    output <- paste("$\\\\widehat \\\\sigma_{Y}=$", round(sqrt(sigma2), 2),
      "$\\\\widehat{cor}(w, Y) =$", round(cor_w, 2),
      sep = ", "
    )
  }
  if (pretty == TRUE) {
    return(
      kbl(df_summary, align = "c") %>%
        kable_styling(full_width = F) %>%
        kableExtra::add_footnote(label = output, escape = FALSE)
    )
  } else {
    if (estimand == "PATE") {
      return(data.frame(df_summary, sigma_tau_bound = round(sqrt(sigma2), 2), cor_w = round(cor_w, 2)))
    } else {
      return(data.frame(df_summary, sigma_Y = round(sqrt(sigma2), 2), cor_w = round(cor_w, 2)))
    }
  }
}
