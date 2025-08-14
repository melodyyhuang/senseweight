#' Run Formal Benchmarking 
#'
#' Wrapper function to run formal benchmarking on a set of pre-specified covariates. Returns a data.frame containing the benchmarked parameter values, the estimated bias, MRCS, and minimum \code{k_sigma} and \code{k_rho} values for a killer confounder.
#' 
#' @param estimate Weighted estimate
#' @param RV Robustness Value
#' @param weighting_vars Vector of variables to use in the weights estimation for ATT or PATE
#' @param benchmark_vars Vector of variables to benchmark parameters for. If \code{benchmark_vars = 'all'}, benchmarking will be run across all variables included in the weights. If not set to \code{all}, benchmarking will be conducted across the covariates included in the vector.
#' @param data A data.frame containing the observed covariates included in the weights; must include variables specified in weighting_vars
#' @param treatment Denotes which variable is the treatment variable
#' @param outcome Denotes which variable is the outcome variable
#' @param selection Denotes which variable is the selection variable
#' @param formula Raking formula for survey estimand 
#' @param weights A vector, containing the estimated survey weights
#' @param sample_svy Survey object, containing the survey sample being re-weighted
#' @param pop_svy Survey object, containing the population the survey sample is being re-weighted to
#' @param Y outcome of interest (used for survey object)
#' @param population_targets Population targets for the raking formula in surveys (optional, if not provided, will be generated from pop_svy)
#' @param weighting_method Weighting method. Supports weighting methods from the package \code{WeightIt}.
#' @param weight_max Maximum weight to trim at. Default set to \code{Inf}.
#' @param sigma2 If \code{estimand = "PATE"}, \code{sigma2} must specify the bound on treatment effect heterogeneity. For the other two estimands, the function will automatically calculate the sample variance across the control units, or the survey sample.
#' @param estimand Specifies estimand; possible parameters include "ATT", "PATE", or "Survey"
#' 
#' @return data.frame containing the benchmarked parameter values, the estimated bias, MRCS, and minimum \code{k_sigma} and \code{k_rho} values for a killer confounder for the set of pre-specified covariates.
#' @export
#' 
#' @examples
#' # For the external validity setting: 
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

run_benchmarking <- function(estimate, RV, 
                             formula = NULL, weights = NULL, pop_svy = NULL,
                             sample_svy = NULL, Y = NULL,
                             weighting_vars = NULL, benchmark_vars = "all",
                             data  = NULL, treatment = NULL, outcome = NULL, 
                             selection  = NULL,
                             population_targets = NULL,
                             weighting_method = "ebal", weight_max = Inf,
                             sigma2 = NULL, estimand = "ATT") {
  if(estimand == "Survey"){
    if(benchmark_vars == "all"){
      benchmark_covariates = all.vars(formula)
      
    }
    df_benchmark = lapply(benchmark_covariates, 
           benchmark_survey,
           formula = formula,
           weights = weights,
           pop_svy = pop_svy,
           Y = Y,
           sample_svy = sample_svy,
           population_targets = population_targets) |> dplyr::bind_rows()
    
  }else{
    names(data)[which(names(data) == paste(outcome))] <- "outcome"
    names(data)[which(names(data) == paste(treatment))] <- "treatment"
    if (estimand == "ATT") {
      sigma2 <- stats::var(data$outcome[data$treatment == 0])
      data$missing <- data$treatment
      data$selection <- NA
    }
    if (estimand == "PATE") {
      names(data)[which(names(data) == paste(selection))] <- "selection"
      if (estimand == "PATE" & is.null(sigma2)) {
        return("Error: Must specify bound for treatment effect heterogeneity.")
      }
      data$missing <- 1 - data$selection
    }
    keep_covariates <- append(c("outcome", "treatment", "selection", "missing"), weighting_vars)
    data <- data |> dplyr::select(keep_covariates)
    model_weights <- WeightIt::weightit(missing ~ . - selection - treatment - outcome,
                                        data = data,
                                        method = weighting_method, estimand = "ATT"
    )
    weights <- model_weights$weights
    weights = weights/mean(weights)
    if (benchmark_vars == "all") {
      benchmark_covariates <- weighting_vars
    }
    df_benchmark <- data.frame(
      lapply(benchmark_covariates, benchmark,
             weights = weights, data = data, sigma2 = sigma2,
             weighting_method = weighting_method, weight_max = weight_max, 
             estimand = estimand
      ) |>
        dplyr::bind_rows()
    )
  }
  
  df_benchmark$MRCS <- estimate / df_benchmark$bias
  df_benchmark$k_sigma_min <- RV / df_benchmark$R2_benchmark
  df_benchmark$k_rho_min <- sqrt(RV) / df_benchmark$rho_benchmark
  df_benchmark <- df_benchmark |>
    dplyr::mutate_if(is.numeric, round, 2)
  return(df_benchmark)
}

#' Sensitivity Summary
#'
#' Returns a data.frame or Kable table with summary measures of sensitivity
#' 
#' @param weights Vector of estimated weights
#' @param Y Outcome of interest
#' @param b_star Killer confounder threshold. If not specified, will be automatically set to 0.
#' @param estimate (Optional) Weighted point estimate. If not specified, function will automatically generate the weighted estimator, given the inputs in \code{Y} and \code{weights}
#' @param SE (Optional) Standard error associated with the weighted point estimate
#' @param unweighted (Optional) Unweighted point estimate.
#' @param Z Treatment assignment (Not needed for settings when users are analyzing surveys)
#' @param sigma2 (Optional) Variance of outcomes or individual-level treatment effect in PATE case. In the case of a PATE estimator, if not specified, function will automatically estimate an upper bound for the variance of the individual-level treatment effect.
#' @param estimand Specifies estimand; possible parameters include "ATT", "PATE", or "Survey"
#' @param pretty If set to \code{TRUE}, will return a Kable table. If set to \code{FALSE}, will return a data.frame.
#' @param svy_srs Unweighted `svymean` object 
#' @param svy_wt Weighted `svymean` object
#' @param sig.fig Significant figures to round the output to (default set to 2)
#' 
#' @return Sensitivity summary
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
#' summarize_sensitivity(weights = weights, 
#' Y = df_site$Y, 
#' Z = df_site$T, 
#' sigma2 = vartau,
#'  estimand = "PATE")
summarize_sensitivity <- function(weights = NULL, Y = NULL, Z = NULL, b_star = 0,
                                  estimate = NULL, SE = NULL, unweighted = NULL,
                                  sigma2 = NULL, estimand = "ATT", pretty = FALSE,
                                  svy_srs = NULL, svy_wt = NULL, sig.fig = 2) {
  if (estimand == "Survey") {
    if (!is.null(svy_srs) & !is.null(svy_wt)) {
      return(summarize_sensitivity_survey(svy_srs, svy_wt, weights, stats::var(Y), b_star))
    } else {
      sigma2 <- stats::var(Y)
      RV <- robustness_value(estimate = estimate - b_star, sigma2 = sigma2, weights = weights)
      df_summary <- data.frame(
        Unweighted = round(unweighted, 2),
        Estimate = round(estimate, 2),
        SE = round(SE, 2),
        RV = round(RV, 2)
      )
      return(data.frame(df_summary, sigma_Y = sigma2, cor_w = stats::cor(weights, Y)))
    }
  }
  if (is.null(unweighted)) {
    model_DiM <- estimatr::lm_robust(Y ~ Z)
    DiM <- stats::coef(model_DiM)[2]
    names(DiM) <- NULL
  }
  if (is.null(estimate)) {
    model_ipw <- estimatr::lm_robust(Y ~ Z, weights = weights)
    estimate <- stats::coef(model_ipw)[2]
    names(estimate) <- NULL
  }
  if (is.null(sigma2)) {
    if (estimand == "PATE") {
      sigma2 <- stats::var(Y[Z == 1]) + stats::var(Y[Z == 0])
    } else {
      # Estimand is ATT (or Survey Mean)
      sigma2 <- stats::var(Y[Z == 0])
    }
  }
  # Estimate correlation:
  if (estimand == "PATE") {
    cov_w <- stats::cov(Y[Z == 1], weights[Z == 1]) -
      stats::cov(Y[Z == 0], weights[Z == 0])
    cor_w <- cov_w / sqrt(stats::var(weights) * sigma2)
  } else {
    # Estimand is ATT (or Survey Mean)
    cor_w <- stats::cor(weights[Z == 0], Y[Z == 0])
  }
  # Calculate Robustness Value:
  RV <- robustness_value(estimate, b_star, sigma2, weights[Z == 0])
  df_summary <- data.frame(
    Unweighted = round(DiM, sig.fig), Unweighted_SE = round(model_DiM$std.error[2], sig.fig),
    Estimate = round(estimate, sig.fig), SE = round(model_ipw$std.error[2], sig.fig),
    RV = round(RV, sig.fig)
  )

  if (estimand == "PATE") {
    output <- paste("$\\\\widehat \\\\sigma_{\\\\tau, max}=$", round(sqrt(sigma2), sig.fig),
      "$\\\\widehat{cor}(w, \\\\tau) =$", round(cor_w, sig.fig),
      sep = ", "
    )
  } else {
    output <- paste("$\\\\widehat \\\\sigma_{Y}=$", round(sqrt(sigma2), sig.fig),
      "$\\\\widehat{cor}(w, Y) =$", round(cor_w, sig.fig),
      sep = ", "
    )
  }
  if (pretty == TRUE) {
    return(
      kableExtra::kbl(df_summary, align = "c") |>
        kableExtra::kable_styling(full_width = F) |>
        kableExtra::add_footnote(label = output, escape = FALSE)
    )
  } else {
    if (estimand == "PATE") {
      return(data.frame(df_summary, sigma_tau_bound = round(sqrt(sigma2), sig.fig), cor_w = round(cor_w, sig.fig)))
    } else {
      return(data.frame(df_summary, sigma_Y = round(sqrt(sigma2), sig.fig), cor_w = round(cor_w, sig.fig)))
    }
  }
}
