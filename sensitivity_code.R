rm(list = ls())
#-------------------------------------------------------------------------------------
library(tidyverse)
library(survey)
library(senseweight)
#-------------------------------------------------------------------------------------
#Wrapper function: 
#Flags: 
#experiment: data.frame containing the experimental sample data
#outcome: name of the outcome variable in the experiment data.frame
#treatment: name of the treatment variable in the experiment data.frame
#target_pop: data.frame containing the target population data 
#covariates: a vector of covariate names that we would like to include in the weights
#weighting_method: this passes in a flag to a WeightIt object. 
#default set to propensity score weights ("ps"). Can be set to "ebal" for entropy balancing, 
#or other weighting methods supported by the WeightIt package. 
generalize_experiment<-function(experiment, outcome = "Y", treatment = "Z", target_pop, 
    covariates = c("age", "gender"), weighting_method = 'ps'){
    df_sample = data.frame(Y = experiment[[outcome]], Z = experiment[[treatment]])
    df_sample = cbind(df_sample, experiment %>% dplyr::select(covariates))
    df_pop = cbind(data.frame(Y= NA, Z = NA), target_pop %>% dplyr::select(covariates))
    df_all = rbind(data.frame(df_sample, S = 1), data.frame(df_pop, S = 0))
    weighting_formula = as.formula(paste("1-S ~", paste(covariates, collapse="+"), sep=""))

    model_dim = estimatr::lm_robust(Y~Z, data = df_sample)
    DiM = coef(model_dim)[2]

    model_ps = WeightIt::weightit(weighting_formula,  method = weighting_method, data = df_all[,-(1:2)],
        estimand="ATT")
    weights = model_ps$weights[df_all$S == 1]
    #CENTER AT MEAN 1: 
    weights = weights/mean(weights)

    model_ipw = estimatr::lm_robust(Y ~ Z, data = df_sample, weights=weights)
    ipw = coef(model_ipw)[2]
    ipw_se = model_ipw$std.error[2]
    p_value = model_ipw$p.value[2]
    var_tau = var(df_sample$Y[df_sample$Z==1]) - var(df_sample$Y[df_sample$Z==0])
    if(var_tau < 0 || is.na(var_tau)){
        var_tau = var(df_sample$Y[df_sample$assigned==1]) + var(df_sample$Y[df_sample$assigned==0])
    }

    sensitivity_summary = summarize_sensitivity(weights = weights, Y = df_sample$Y, Z = df_sample$Z,
        sigma2= var_tau, estimand = "PATE")

    RV = robustness_value(estimate = ipw, sigma2=var_tau, weights=weights)

    df_benchmark = run_benchmarking(covariates, data = df_all,
                 treatment="Z", outcome = "Y", selection = "S",
                 estimate=ipw,
                 RV = RV, sigma2=var_tau,
                 estimand = "PATE",
                 weighting_method = weighting_method)

    bias_contour = contour_plot(var(weights), var_tau, ipw, df_benchmark, benchmark=TRUE, shade=FALSE)

    return(list(
        sensitivity_summary = sensitivity_summary,
        RV = RV,
        benchmark_results = df_benchmark,
        bias_contour = bias_contour))
}


data(jtpa_women)
experiment_sample = jtpa_women %>% filter (site == "CV")
target_population = jtpa_women %>% filter (site != "CV")


generalize_experiment(experiment_sample, target_population, outcome="Y", treatment = "T",
    covariates = c("prevearn", "age", "married", "hrwage", "black", "hispanic", "hsorged", "yrs_educ"))