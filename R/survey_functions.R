#' Sensitivity Summary (for survey weights)
#'
#' Returns a data.frame or Kable table with summary measures of sensitivity; helper function for main summary function, and allows users to directly input a survey object and a design object
#' @param svy_srs Survey object, containing the unweighted survey
#' @param svy_wt Survey object, containing the weighted survey
#' @param weights A vector, containing the estimated survey weights
#' @param varY variance of the outcome
#' @param b_star Killer confounder threshold, default set to be zero
#' @export
summarize_sensitivity_survey <- function(svy_srs, svy_wt, weights, varY, b_star = 0) {
  estimate_srs <- as.data.frame(svy_srs)[, 1]
  estimate_srs_se <- as.data.frame(svy_srs)[, 2]
  estimate_wt <- as.data.frame(svy_wt)[, 1]
  estimate_wt_se <- as.data.frame(svy_wt)[, 2]

  RV <- robustness_value(
    estimate = estimate_wt - b_star,
    sigma2 = varY,
    weights = weights
  )

  out <- data.frame(
    Unweighted = estimate_srs,
    Unweighted_SE = estimate_srs_se,
    Estimate = estimate_wt,
    SE = estimate_wt_se,
    RV = round(RV, 2)
  )
  return(out)
}


#' Benchmark (for survey weights)
#'
#' Returns benchmarking results for survey weighting
#' @param omit Variable to benchmark
#' @param formula Raking formula
#' @param weights A vector, containing the estimated survey weights
#' @param sample_svy Survey object, containing the survey sample being re-weighted
#' @param pop_svy Survey object, containing the population the survey sample is being re-weighted to
#' @param Y outcome of interest
#' @param weighting_method Weighting method (default to raking)
#' @return Benchmarking results for a variable (or subset of variables)
#' @export
benchmark_survey <- function(omit, formula, weights, pop_svy,
                             sample_svy, Y, weighting_method) {
  if (length(all.vars(formula)) == 1) {
    return(NULL)
  }
  formula_benchmark <- as.formula(paste0(
    "~",
    paste0(all.vars(formula)[-which(all.vars(formula) %in% omit)],
      collapse = " + "
    )
  ))

  # Set up population targets:
  population_targets <- create_targets(pop_svy, formula_benchmark)

  # Estimate weights;
  model_benchmark <- survey::calibrate(
    design = sample_svy,
    formula = formula_benchmark,
    population = population_targets,
    calfun = weighting_method
  )

  weights_benchmark <- stats::weights(model_benchmark) * nrow(model_benchmark)

  return(data.frame(
    variable = omit,
    benchmark_parameters(
      weights,
      weights_benchmark,
      Y = Y,
      sigma2 = var(Y),
      estimand = "Survey"
    )
  ))
}
