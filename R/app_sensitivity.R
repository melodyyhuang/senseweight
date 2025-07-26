#' @keywords internal
sensitivityFun <- function(input, output, session,
                           analysis_type,
                           mainDataInput, populationDataInput, treatment, Y,
                           weightingMethod = 'raking', weightingVars,
                           popWeights = NULL,
                           threshold = 0, type = "mean",
                           contrast1 = NULL, contrast2 = NULL) {
  
  shiny::reactive({
    shiny::req(analysis_type())
    
    if (analysis_type() == "survey") {
      shiny::req(
        mainDataInput(), populationDataInput(), Y(),
        weightingMethod(), weightingVars()
      )
    } else if (analysis_type() == "experiment") {
      shiny::req(
        mainDataInput(), populationDataInput(), Y(), treatment(),
        weightingMethod(), weightingVars()
      )
    } else if (analysis_type() == "observational") {
      shiny::req(
        mainDataInput(), Y(), treatment(),
        weightingMethod(), weightingVars()
      )
    }
    
    # initialize variables
    data <- mainDataInput()
    population_data <- populationDataInput()
    treatment_var <- treatment()
    response_var <- Y()
    weighting_method <- weightingMethod()
    weighting_vars <- weightingVars()
    pop_weights <- popWeights()
    b_star <- threshold()
    type <- type()
    contrast1 <- contrast1()
    contrast2 <- contrast2()
    
    # # error checking
    # if (!is.numeric(data[[response_var]])) {
    #   stop("Response variable must be numeric.")
    # }
    # if (!identical(treatment_var, "")) {
    #   if (!is.numeric(data[[treatment_var]]) | 
    #       any(!(data[[treatment_var]] %in% c(0, 1)))) {
    #     stop("Treatment variable must be numeric and binary (0/1).")
    #   }
    # }
    
    if (analysis_type() == "survey") {
      #GENERATE SURVEY OBJECTS:
      survey_objs <- generate_survey_objects(
        data, population_data,
        outcome = response_var,
        pop_weights = pop_weights,
        weighting_method = weighting_method,
        covariates = weighting_vars,
        type = type,
        contrast1 = contrast1,
        contrast2 = contrast2
      )
      
      #GENERATE SENSITIIVTY SUMMARY:
      df_sensitivity_summary <- summarize_sensitivity_survey(
        survey_objs$svy_srs_est,
        survey_objs$svy_wt_est,
        weights = survey_objs$weights,
        varY = survey_objs$varY,
        b_star = b_star
      )
      
      #BENCHMARK
      if (length(weighting_vars) > 1) {
        df_benchmark <- lapply(
          weighting_vars,
          function(benchmark_var) {
            benchmark_survey(
              omit = benchmark_var,
              formula = survey_objs$formula,
              weights = survey_objs$weights,
              pop_svy = survey_objs$pop_svy,
              sample_svy = survey_objs$sample_svy,
              Y = survey_objs$Y_recode,
              weighting_method = weighting_method
            )
          }
        ) |>
          dplyr::bind_rows() |>
          dplyr::mutate(
            MRCS = abs((df_sensitivity_summary$Estimate - b_star) / bias)
          ) |>
          dplyr::arrange(MRCS)
        
        shade_variables <- df_benchmark$variable[
          df_sensitivity_summary$Estimate - df_benchmark$bias > b_star
        ]
        shade <- TRUE
        benchmark <- TRUE
      } else {
        df_benchmark <- NULL
        benchmark <- FALSE
        shade_variables <- NULL
        shade <- FALSE
      }
      # Outputs all info to create benchmarking table, summary table, and plot via contour_plot()
      out <- list(
        varW = stats::var(survey_objs$weights),
        sigma2 = stats::var(data[[response_var]]),
        killer_confounder = df_sensitivity_summary$Estimate - b_star,
        df_benchmark = df_benchmark,
        benchmark = benchmark,
        shade = shade,
        shade_var = shade_variables,
        df_sensitivity_summary = df_sensitivity_summary,
        warning_messages = survey_objs$warning_messages
      )
    } else if (analysis_type() %in% c("experiment", "observational")) {
      #SET UP OBJECTS:
      if (analysis_type() == "experiment") {
        sensitivity_results <- generalize_experiment(
          experiment = data,
          target_pop = population_data,
          outcome = response_var,
          treatment = treatment_var,
          covariates = weighting_vars,
          weighting_method = weighting_method,
          b_star = b_star
        )
      } else if (analysis_type() == "observational") {
        sensitivity_results <- run_obs_analysis(
          data = data,
          treatment = treatment_var,
          outcome = response_var,
          weighting_vars = weighting_vars,
          weighting_method = weighting_method,
          b_star = b_star
        )
      }
      
      #Extract results
      df_sensitivity_summary <- sensitivity_results$sensitivity_summary
      
      #BENCHMARK
      if (!is.null(sensitivity_results$benchmark_results)) {
        df_benchmark <- sensitivity_results$benchmark_results |>
          dplyr::mutate(
            MRCS = abs((df_sensitivity_summary$Estimate - b_star) / bias)
          ) |>
          dplyr::arrange(MRCS)
        shade_variables <- df_benchmark$variable[
          df_sensitivity_summary$Estimate - df_benchmark$bias > b_star
        ]
        shade <- TRUE
        benchmark <- TRUE
      } else {
        df_benchmark <- NULL
        benchmark <- FALSE
        shade_variables <- NULL
        shade <- FALSE
      }
      
      # Outputs all info to create benchmarking table, summary table, and plot via contour_plot()
      out <- list(
        varW = stats::var(sensitivity_results$weights),
        sigma2 = sensitivity_results$sigma2,
        killer_confounder = df_sensitivity_summary$Estimate - b_star,
        df_benchmark = df_benchmark,
        benchmark = benchmark,
        shade = shade,
        shade_var = shade_variables,
        df_sensitivity_summary = df_sensitivity_summary,
        warning_messages = sensitivity_results$warning_messages
      )
    }
    
    # Outputs all info to create benchmarking table, summary table, and plot via contour_plot()
    return(out)
  })
}
