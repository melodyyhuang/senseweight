#' Function for creating targets from auxiliary information and formula
#' @keywords internal
create_targets <- function(target_design, target_formula) {
  target_mf <- model.frame(target_formula, model.frame(target_design))
  target_mm <- model.matrix(target_formula, target_mf)
  wts <- stats::weights(target_design)
  if (all(wts == 1)) {
    return(colMeans(target_mm))
  } else {
    return(colSums(target_mm * wts) / sum(wts))
  }
}


#' @keywords internal
generate_survey_objects <- function(sample_data, population_data,
                                    outcome = 'Y',
                                    pop_weights = NULL,
                                    weighting_method = c('raking', 'manual'),
                                    covariates,
                                    type = c('mean', 'contrast'),
                                    contrast1 = NULL,
                                    contrast2 = NULL) {
  type <- match.arg(type)
  weighting_method <- match.arg(weighting_method)
  msgs_ls <- NULL
  
  if (weighting_method == "manual") {
    stop("Error: manual weighting method not yet implemented!")
  }
  
  # TODO: Check for NAs as in external/internal validity settings
  
  sample_data$.outcome <- sample_data[[outcome]]
  if (type == 'contrast') {
    # User must pick what contrasts we are comparing:
    if (is.null(contrast1) || is.null(contrast2)) {
      stop("Error: user must specify contrasts!")
    }
    contrast1_recode <- paste0(outcome, contrast1)
    contrast2_recode <- paste0(outcome, contrast2)
    contrast <- sprintf(
      "(%1$s-%2$s)/(%1$s+%2$s)", contrast1_recode, contrast2_recode
    )
    Y_recode <- as.numeric(sample_data$.outcome == contrast1)
  } else {
    Y_recode <- sample_data$.outcome
  }
  varY <- var(Y_recode)
  if (!is.null(pop_weights)) {
    population_data$.weights <- pop_weights
  } else {
    # No population weights needed:
    population_data$.weights <- 1
  }
  
  sample_svy <- survey::svydesign(
    ids = ~1, data = sample_data
  )
  pop_svy <- survey::svydesign(
    ids = ~1, weights = ~ .weights, data = population_data
  )
  
  # Set up weighting formula:
  formula_weight <- as.formula(paste("~", paste(covariates, collapse = "+")))
  
  # Generate targets:
  survey_targets <- create_targets(sample_svy, formula_weight)
  pop_targets <- create_targets(pop_svy, formula_weight)
  
  if (length(survey_targets) != length(pop_targets)) {
    stop("Error! Length of survey targets and population targets do not match.")
  }
  # PERFORM WEIGHTING:
  model_weight <- survey::calibrate(
    design = sample_svy,
    formula = formula_weight,
    population = pop_targets,
    calfun = weighting_method,
    force = TRUE
  )
  
  sample_wt_svy <- survey::svydesign(
    ~1, data = sample_data, weights = stats::weights(model_weight)
  )
  wts <- stats::weights(model_weight) * nrow(model_weight)
  wts <- wts / mean(wts)
  
  if (type == 'mean') {
    estimate_wt <- survey::svymean(~ .outcome, sample_wt_svy, na.rm = TRUE)
    estimate_srs <- survey::svymean(~ .outcome, sample_svy, na.rm = TRUE)
  } else {
    estimate_wt <- survey::svycontrast(
      survey::svymean(~ .outcome, sample_wt_svy, na.rm = TRUE), contrast
    )
    estimate_srs <- survey::svycontrast(
      survey::svymean(~ .outcome, sample_svy, na.rm = TRUE), contrast
    )
  }
  
  out <- list(
    svy_srs_est = estimate_srs,
    svy_wt_est = estimate_wt,
    pop_svy = pop_svy,
    sample_svy = sample_svy,
    sample_wt_svy = sample_wt_svy,
    weights = wts,
    formula = formula_weight,
    varY = varY,
    Y_recode = Y_recode,
    warning_messages = msgs_ls
  )
  return(out)
}

#' @keywords internal
check_overlap <- function(df_sample, df_pop, covariates) {
  covariates_overlap <- covariates
  problematic_covariates <- c()
  dropped <- c()
  for (covariate in covariates) {
    # Q: Is this the desired behavior for continuous variables?
    if (length(unique(df_sample[[covariate]])) <
        length(unique(df_pop[[covariate]]))) {
      problematic_covariates <- c(problematic_covariates, covariate)
      if (is.factor(df_sample[[covariate]]) ||
          (length(unique(df_pop[[covariate]])) == 2)) {
        dropped = c(dropped, covariate)
        # If categorical or binary variable, remove the variable
        covariates_overlap <- setdiff(covariates_overlap, covariate)
      }
    }
  }
  if (length(dropped) != 0) {
    msgs_ls <- list(
      sprintf(
        "There were overlap issues in the following covariates: %s. Weighting was performed only on the following covariates with overlap: %s.",
        paste(dropped, collapse = ", "),
        paste(covariates_overlap, collapse = ", ")
      )
    )
  } else {
    msgs_ls <- NULL
  }
  out <- list(
    covariates_overlap = covariates_overlap,
    warning_messages = msgs_ls
  )
  return(out)
}


#' @keywords internal
generalize_experiment <- function(experiment, target_pop,
                                  outcome = "Y", treatment = "Z",
                                  filters = TRUE,
                                  covariates,
                                  weighting_method = 'ebal', b_star) {
  
  msgs_ls <- NULL
  df_sample <- data.frame(
    .Y = experiment[[outcome]],
    .Z = experiment[[treatment]]
  ) |>
    cbind(
      experiment |>
        dplyr::select(tidyselect::all_of(covariates))
    )
  if (any(is.na(df_sample))) {
    n_orig <- nrow(df_sample)
    n_na <- sum(rowSums(is.na(df_sample)) > 0)
    df_sample <- na.omit(df_sample)
    msgs_ls <- c(
      msgs_ls,
      list(
        sprintf("Missing values were present in the experiment data. %s (out of %s) observations with missing values have been dropped from the analysis.", n_na, n_orig)
      )
    )
  }
  
  # Choose population:
  df_pop <- target_pop |>
    dplyr::filter(!!filters) |>
    dplyr::select(tidyselect::all_of(covariates))
  if (any(is.na(df_pop))) {
    n_orig <- nrow(df_pop)
    n_na <- sum(rowSums(is.na(df_pop)) > 0)
    df_pop <- na.omit(df_pop)
    msgs_ls <- c(
      msgs_ls,
      list(
        sprintf("Missing values were present in the target population data. %s (out of %s) observations with missing values have been dropped from the analysis.", n_na, n_orig)
      )
    )
  }
  df_pop <- cbind(data.frame(.Y = NA, .Z = NA), df_pop)
  
  covariates_overlap_out <- check_overlap(df_sample, df_pop, covariates)
  covariates_overlap <- covariates_overlap_out$covariates_overlap
  msgs_ls <- c(msgs_ls, covariates_overlap_out$warning_messages)
  
  df_sample <- df_sample |>
    dplyr::select(tidyselect::all_of(c(".Y", ".Z", covariates_overlap))) |>
    dplyr::mutate(.S = 1)
  df_pop <- df_pop |>
    dplyr::select(tidyselect::all_of(c(".Y", ".Z", covariates_overlap))) |>
    dplyr::mutate(.S = 0)
  df_all <- rbind(df_sample, df_pop)
  
  weighting_formula <- as.formula(
    paste("1 - .S ~", paste(covariates_overlap, collapse = "+"), sep = "")
  )
  
  model_ps <- WeightIt::weightit(
    weighting_formula,
    method = weighting_method,
    data = df_all |>
      dplyr::select(-.Y, -.Z),
    estimand = "ATT"
  )
  wts <- model_ps$weights[df_all$.S == 1]
  wts <- wts / mean(wts)  # center at mean 1
  
  model_ipw <- estimatr::lm_robust(.Y ~ .Z, data = df_sample, weights = wts)
  ipw <- coef(model_ipw)[2]
  ipw_se <- model_ipw$std.error[2]
  p_value <- model_ipw$p.value[2]
  var_tau <- var(df_sample$.Y[df_sample$.Z == 1]) -
    var(df_sample$.Y[df_sample$.Z==0])
  if (var_tau < 0 || is.na(var_tau)) {
    var_tau <- var(df_sample$.Y[df_sample$.Z == 1]) +
      var(df_sample$.Y[df_sample$.Z == 0])
  }
  
  sensitivity_summary <- summarize_sensitivity(
    weights = wts, Y = df_sample$.Y, Z = df_sample$.Z,
    sigma2 = var_tau, estimand = "PATE", b_star = b_star
  )
  
  RV <- robustness_value(
    estimate = ipw, b_star = b_star, sigma2 = var_tau, weights = wts
  )
  
  if (length(covariates_overlap) > 1) {
    df_benchmark <- run_benchmarking(
      covariates_overlap, data = df_all,
      treatment = ".Z", outcome = ".Y", selection = ".S",
      estimate = ipw, RV = RV, sigma2 = var_tau, estimand = "PATE",
      weighting_method = weighting_method
    )
  } else {
    df_benchmark <- NULL
  }
  
  out <- list(
    sensitivity_summary = sensitivity_summary,
    RV = RV,
    benchmark_results = df_benchmark,
    weights = wts,
    sigma2 = var_tau,
    warning_messages = msgs_ls
  )
  return(out)
}


#' @keywords internal
run_obs_analysis <- function(data, treatment, outcome, weighting_vars,
                             weighting_method, b_star) {
  
  msgs_ls <- NULL
  data_analysis <- data.frame(
    .Y = data[[outcome]],
    .Z = data[[treatment]]
  ) |>
    cbind(
      data |>
        dplyr::select(tidyselect::all_of(weighting_vars))
    )
  if (any(is.na(data_analysis))) {
    n_orig <- nrow(data_analysis)
    n_na <- sum(rowSums(is.na(data_analysis)) > 0)
    data_analysis <- na.omit(data_analysis)
    msgs_ls <- c(
      msgs_ls,
      list(
        sprintf("Missing values were present in the observational data. %s (out of %s) observations with missing values have been dropped from the analysis.", n_na, n_orig)
      )
    )
  }
  
  model_ps <- WeightIt::weightit(
    .Z ~ .,
    method = weighting_method,
    data = data_analysis |> dplyr::select(-.Y),
    estimand = "ATT"
  )
  wts <- model_ps$weights
  wts[data_analysis$.Z == 0] <- wts[data_analysis$.Z == 0] /
    mean(wts[data_analysis$.Z == 0])  # center at mean 1
  
  model_ipw <- estimatr::lm_robust(.Y ~ .Z, data = data_analysis, weights = wts)
  ipw <- coef(model_ipw)[2]
  ipw_se <- model_ipw$std.error[2]
  p_value <- model_ipw$p.value[2]
  
  varY <- var(data_analysis$.Y[data_analysis$.Z == 0])
  sensitivity_summary <- summarize_sensitivity(
    weights = wts,
    Y = data_analysis$.Y,
    Z = data_analysis$.Z,
    sigma2 = varY,
    estimand = "ATT"
  )
  
  RV <- robustness_value(
    estimate = ipw, sigma2 = varY, weights = wts, b_star = b_star
  )
  
  if (length(weighting_vars) > 1) {
    df_benchmark <- run_benchmarking(
      weighting_vars,
      data = data_analysis,
      treatment = ".Z", outcome = ".Y",
      estimate = ipw,
      RV = RV, sigma2 = varY,
      estimand = "ATT",
      weighting_method = weighting_method
    )
  } else {
    df_benchmark = NULL
  }
  
  out <- list(
    sensitivity_summary = sensitivity_summary,
    RV = RV,
    benchmark_results = df_benchmark,
    weights = wts[data_analysis$.Z == 0],
    sigma2 = varY,
    warning_messages = msgs_ls
  )
  return(out)
}