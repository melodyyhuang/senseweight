#' Sensitivity Summary (for survey weights)
#'
#' Returns a data.frame or Kable table with summary measures of sensitivity; helper function for main summary function, and allows users to directly input a survey object and a design object
#' 
#' @param svy_srs Survey object, containing the unweighted survey
#' @param svy_wt Survey object, containing the weighted survey
#' @param weights A vector, containing the estimated survey weights
#' @param varY variance of the outcome
#' @param b_star Killer confounder threshold, default set to be zero
#' 
#' @return Sensitivity data.frame or latex table with summary measures of sensitivity
#' @export
#' 
#' @examples
#' data(poll.data)
#' poll_srs <- survey::svydesign(ids = ~ 1, data = poll.data)
#' pop_targets = c(1, 0.212, 0.264, 0.236, 0.310, 
#'                 0.114, 0.360, 0.528, 0.114, 
#'                 0.021, 0.034, 0.805, 
#'                 0.266, 0.075, 0.312, 0.349)
#' names(pop_targets) = c("(Intercept)",
#'                        "age_buckets36to50",
#'                        "age_buckets51to64",
#'                        "age_bucketsOver65",
#'                        "educHigh School or Less",
#'                        "educPost-grad",
#'                        "educSome college",
#'                        "genderWomen", 
#'                        "raceBlack",
#'                        "raceHispanic",
#'                        "raceOther",
#'                        "raceWhite", 
#'                        "pidIndependent", "pidOther", 
#'                        "pidRepublican", "bornagainYes")
#' #Set up raking formula:
#' formula_rake <- ~ age_buckets + educ + gender + race + pid + bornagain
#' 
#' #PERFORM RAKING:
#' model_rake <- survey::calibrate(
#'   design = poll_srs,
#'   formula = formula_rake,
#'   population = pop_targets,
#'   calfun = "raking",
#'   force = TRUE
#' )
#' 
#' 
#' rake_results <- survey::svydesign( ~ 1, data = poll.data, weights = stats::weights(model_rake))
#' #Estimate from raking results:
#' weights = stats::weights(rake_results) * nrow(model_rake)
#' 
#' unweighted_estimate = survey::svymean(~ Y, poll_srs, na.rm = TRUE)
#' weighted_estimate = survey::svymean(~ Y, model_rake, na.rm = TRUE)
#' summarize_sensitivity(estimand = 'Survey',
#' Y = poll.data$Y,
#' weights = weights,
#' svy_srs = unweighted_estimate, 
#' svy_wt = weighted_estimate,
#' b_star = 0.5)

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
    RV = RV
  )
  return(out)
}


#' Benchmark (for survey weights)
#'
#' Returns benchmarking results for survey weighting
#' 
#' @param omit Variable to benchmark
#' @param formula Raking formula
#' @param weights A vector, containing the estimated survey weights
#' @param sample_svy Survey object, containing the survey sample being re-weighted
#' @param pop_svy Survey object, containing the population the survey sample is being re-weighted to
#' @param Y outcome of interest
#' @param population_targets Population targets for the raking formula (optional, if not provided, will be generated from pop_svy)
#' @param weighting_method Weighting method (default to raking)
#' 
#' @return Benchmarking results for a variable (or subset of variables)
#' @export
#' 
#' @examples 
#' data(poll.data)
#' poll_srs <- survey::svydesign(ids = ~ 1, data = poll.data)
#' pop_targets = c(1, 0.212, 0.264, 0.236, 0.310, 
#'                 0.114, 0.360, 0.528, 0.114, 
#'                 0.021, 0.034, 0.805, 
#'                 0.266, 0.075, 0.312, 0.349)
#' names(pop_targets) = c("(Intercept)",
#'                        "age_buckets36to50",
#'                        "age_buckets51to64",
#'                        "age_bucketsOver65",
#'                        "educHigh School or Less",
#'                        "educPost-grad",
#'                        "educSome college",
#'                        "genderWomen", 
#'                        "raceBlack",
#'                        "raceHispanic",
#'                        "raceOther",
#'                        "raceWhite", 
#'                        "pidIndependent", "pidOther", 
#'                        "pidRepublican", "bornagainYes")
#' #Set up raking formula:
#' formula_rake <- ~ age_buckets + educ + gender + race + pid + bornagain
#' 
#' #PERFORM RAKING:
#' model_rake <- survey::calibrate(
#'   design = poll_srs,
#'   formula = formula_rake,
#'   population = pop_targets,
#'   calfun = "raking",
#'   force = TRUE
#' )
#' 
#' 
#' rake_results <- survey::svydesign( ~ 1, data = poll.data, weights = stats::weights(model_rake))
#' #Estimate from raking results:
#' weights = stats::weights(rake_results) * nrow(model_rake)
#' 
#' unweighted_estimate = survey::svymean(~ Y, poll_srs, na.rm = TRUE)
#' weighted_estimate = survey::svymean(~ Y, model_rake, na.rm = TRUE)
#' benchmark_survey('educ', 
#'                  formula = formula_rake,
#'                  weights = weights,
#'                  population_targets = pop_targets,
#'                  sample_svy = poll_srs,
#'                  Y = poll.data$Y)
#'  
benchmark_survey <- function(omit, formula, weights, pop_svy = NULL,
                             sample_svy, Y, population_targets = NULL, 
                             weighting_method = 'raking') {
  if (length(all.vars(formula)) == 1) {
    return(NULL)
  }
  formula_benchmark <- stats::as.formula(paste0(
      "~",
      paste0(all.vars(formula)[-which(all.vars(formula) %in% omit)],
             collapse = " + "
      )
  ))
  if(is.null(population_targets )){
      
    # Set up population targets:
    population_targets <- create_targets(pop_svy, formula_benchmark)
    
  }else{
    omit_char = paste0("^", omit)
    omit_idx = which(names(population_targets) %in% grep(omit_char, names(population_targets), value = TRUE))
    population_targets = population_targets[-omit_idx]
  }
  
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
      sigma2 = stats::var(Y),
      estimand = "Survey"
    )
  ))
}
