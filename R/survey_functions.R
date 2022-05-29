#' Sensitivity Summary (for survey weights)
#'
#' Returns a data.frame or Kable table with summary measures of sensitivity; helper function for main summary function, and allows users to directly input a survey object and a design object
#' @param dependent_var String that provides the name of the outcome variable of interest
#' @param model Weighting model (i.e., a calibrate object)
#' @param outcome_function 
summarize_sensitivity_survey<-function(dependent_var, model, outcome_function, 
                                       svy_srs = NULL, Y = NULL, b_star = 0){
  outcome_variable = as.formula(paste("~", dependent_var))
  if(!is.null(outcome_function)){
    svy_results = svymean(outcome_variable, model, na.rm=TRUE)
    estimate = as.data.frame(svy_results)$nlcon[1]
    estimate_se = as.data.frame(svy_results)$SE[1]
  }else{
    svy_results = svycontrast(svymean(outcome_variable, model, na.rm=TRUE), outcome_function)
    estimate = as.data.frame(svy_results)$nlcon
    estimate_se = as.data.frame(svy_results)$SE  
  }
  #Calculate unweighted: 
  if(is.null(svy_srs)){
    unweighted=NA 
    print("Please load in the unweighted estimate, or the survey design object.")
  }
  unweighted = svycontrast(svymean(outcome_variable, svy_srs, na.rm=TRUE), outcome_function)[1] %>%
    as.numeric()
  if(is.null(Y)){
    Y = model_rake$variables[[dependent_var]]
    if(!is.numeric(Y)){
      return("Please load in numeric outcome variable.")
    }  
  }
  #ELSE: User has loaded in an outcome
  sigma2 = var(Y)
  weights = weights(model)*nrow(model)
  RV = robustness_value(q = 1, estimate - b_star, sigma2, weights)
  df_summary = data.frame(Unweighted = round(unweighted, 2), 
                          Estimate = round(estimate,2), SE = round(estimate_se,2), RV = round(RV,2))
  return(data.frame(df_summary, sigma_Y = round(sigma2,2), cor_w = round(cor(weights, Y), 2)))
}


#' Benchmark (for survey weights)
#'
#' Returns benchmarking results for survey weighting
#' @param drop_variable Variable to benchmark
#' @param formula Raking formula 
#' @param y Response variable
#' @param design Survey object, containing the survey sample being re-weighted 
#' @param target Survey object, containing the population the survey sample is being re-weighted to
#' @param weights_orig Weights used for analysis 
#' @param calfun_choice Weighting method (default to raking)
#' @return Benchmarking results for a variable (or subset of variables) 
#' @export
benchmark_survey<-function(drop_variable, formula, y, design, target, weights_orig, calfun_choice='raking'){
  formula_benchmark = as.formula(paste0("~",
                                        paste0(all.vars(formula)[-which(all.vars(formula) %in% drop_variable)],
                                               collapse = " + ")))
  benchmark_rake = calibrate(design = design,
                             formula = formula_benchmark,
                             population = create_targets(target, formula_benchmark),
                             calfun = calfun_choice)
  weights_calib = weights(benchmark_rake)*nrow(benchmark_rake)
  return(data.frame(variable = drop_variable, 
                    benchmark_parameters(weights_orig, weights_calib, Y=y, sigma2=var(y), estimand="Survey")))
}


