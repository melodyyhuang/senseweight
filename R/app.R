#' Run Shiny SenseWeight application
#' 
#' @param ... Additional arguments to pass to [shiny::shinyApp()]
#' 
#' @returns A shiny application
#' 
#' @export
run_app <- function(...) {
  
  experiment_sample <- jtpa_women |>
    dplyr::filter(site == "NE")
  experiment_pop <- jtpa_women |>
    dplyr::filter(site != "NE")
  
  lalonde_robust <- senseweight::nsw_dw |>
    dplyr::filter(treat == 1) |>
    dplyr::bind_rows(senseweight::psid_controls)
  
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  title <- "Shiny SenseWeight: Sensitivity Analysis for Weighting"
  
  picker_width <- "97.5%"
  
  # Tooltips -------------------------------------------------------------------
  analysis_type_tooltip <- shinyWrappers::format_tooltip("TODO: ADD TOOLTIP HERE")
  sample_data_input_tooltip <- shinyWrappers::format_tooltip("Dataset of survey sample.")
  population_survey_data_input_tooltip <- shinyWrappers::format_tooltip("Dataset of population of interest.")
  population_experiment_data_input_tooltip <- shinyWrappers::format_tooltip("Dataset of target population of interest.")
  experiment_data_input_tooltip <- shinyWrappers::format_tooltip("Dataset of experimental sample. ")
  observational_data_input_tooltip <- shinyWrappers::format_tooltip("Dataset of observational study.")
  treatment_input_tooltip <- shinyWrappers::format_tooltip("Variable name containing the treatment of interest. Must be a binary treatment variable encoded as 0/1's.")
  y_input_tooltip <- shinyWrappers::format_tooltip("Variable name of dependent variable. Dependent variable must be numeric.")
  population_weights_survey_tooltip <- shinyWrappers::format_tooltip("Variable name of sampling weights (optional). Sampling weights must be numeric.")
  population_weights_exp_tooltip  <- shinyWrappers::format_tooltip("Variable name of sampling weights for target population (optional). Sampling weights must be numeric.")
  weighting_method_tooltip <- shinyWrappers::format_tooltip("Weighting approach")
  weighting_vars_manual_tooltip <- shinyWrappers::format_tooltip("User-estimated weights.")
  weighting_vars_tooltip <- shinyWrappers::format_tooltip("Select the covariates to include in the weights.")
  threshold_tooltip <- shinyWrappers::format_tooltip("Killer confounders are confounders that are strong enough to be change our research conclusions. The threshold value defines what constitutes a change in the overall conclusion.")
  type_tooltip <- shinyWrappers::format_tooltip("TODO: ADD TOOLTIP HERE")
  var_interpret_tooltip <- shinyWrappers::format_tooltip("TODO: ADD TOOLTIP HERE")
  contrast1_tooltip <- shinyWrappers::format_tooltip("TODO: ADD TOOLTIP HERE")
  contrast2_tooltip <- shinyWrappers::format_tooltip("TODO: ADD TOOLTIP HERE")
  
  # Captions -------------------------------------------------------------------
  benchmarking_plot_caption <- htmltools::tags$span(
    "Bias contour plots are a visual summary of the overall sensitivity in our estimates. The y-axis varies how imbalanced the omitted confounder is, while the x-axis varies how related the residual imbalance is in an omitted confounder to the outcome of interest. The shaded region denotes the killer confounder region. This represents the set of sensitivity parameters that would result in a substantively meaningful change in our research conclusion. The size of the killer confounder region is a proxy for how much sensitivity there is in the estimate.",
    style = shinyWrappers::css_styler(color = "#777")
  )
  summary_table_caption <- "TODO: Add caption here"
  # benchmarking_table_caption <- "Benchmarking allows researchers to evaluate how plausible a potential killer confounder is. In particular, benchmarking allows researchers to estimate the sensitivity parameters asociated with an omitted confounder as strong as an observed covariate. The estimated bias tells us the corresponding bias that would result from omitting a confounder as strong as the benchmarked covariate. MRCS is the minimum relative confounding strength. If an omitted confounder has an MRCS > 1, then this implies that an omitted confounder would have to be stronger than the benchmarked covariate to be a killer confounder. In contrast, if the MRCS is less than 1, then this implies that an omitted confounder does not have to be as strong as the benchmarked covariate to be a killer confounder."
  benchmarking_table_caption <- "Benchmarking allows researchers to evaluate how plausible a potential killer confounder is. In particular, benchmarking allows researchers to estimate the sensitivity parameters asociated with an omitted confounder as strong as an observed covariate."
  
  # Other Text -----------------------------------------------------------------
  instructions_text <- "TODO: Add instructions here"
  about_text <- htmltools::HTML("This Shiny app evaluates the sensitivity of omitted variable bias from weighted estimators. We currently consider three common missing data settings that leverage weights to account for missing not at random: (1) survey non-response, (2) generalizing or transporting causal effects (i.e., external validity), and (3) observational causal inference (i.e., internal validity). <br><br>To use the Shiny app, start by selecting the analysis type you are trying to perform and then upload your data in the left sidebar.")
  
  # UI -------------------------------------------------------------------------
  ui <- shinydashboardPlus::dashboardPage(
    title = title,
    
    # Header -------------------------------------------------------------------
    shinyWrappers::prettyDashboardHeader(
      title = title,
      title_position = "center",
      title_style = list(
        `font-variant` = "small-caps", `font-size` = "26px",
        `font-family` = "'Quattrocento Sans'"
      )
    ),
    
    # Footer -------------------------------------------------------------------
    footer = shinydashboardPlus::dashboardFooter(
      right = htmltools::HTML("&copy; 2024 Melody Y. Huang and Tiffany M. Tang")
    ),
    
    # Sidebar ------------------------------------------------------------------
    shinydashboard::dashboardSidebar(
      # Analysis Type Section --------------------------------------------------
      input_header_style(
        shiny::icon("database"),
        " Analysis Type" #|>
        # add_my_tooltip_icon(
        #   id = "analysis_type",
        #   style = shinyWrappers::css_styler(`font-size` = "10pt")
        # )
      ),
      shinyWrappers::radio_group_buttons(
        "analysis_type",
        label = NULL,
        choices = c(
          "Survey" = "survey",
          "External Validity" = "experiment",
          "Internal Validity" = "observational"
        ),
        checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
      ),
      shinyWrappers::add_tooltip("analysis_type", analysis_type_tooltip),
      
      # Data Input Section -----------------------------------------------------
      shinyWrappers::hr_short() |>
        shinyWrappers::set_margins(top = "2em", bottom = "1.75em"),
      input_header_style(shiny::icon("database"), " Data Upload"),
      
      # File Upload ------------------------------------------------------------
      shiny::conditionalPanel(
        condition = "input.analysis_type == 'survey'",
        shinyWrappers::fileInputUI(
          "sample_data",
          label = "Survey Data",
          tooltip = sample_data_input_tooltip
        ),
        shinyWrappers::fileInputUI(
          "population_data_survey",
          label = "Target Population Data",
          tooltip = population_survey_data_input_tooltip
        )
      ),
      shiny::conditionalPanel(
        condition = "input.analysis_type == 'experiment'",
        shinyWrappers::fileInputUI(
          "experiment_data",
          label = "Experiment Data",
          tooltip = experiment_data_input_tooltip
        ),
        shinyWrappers::fileInputUI(
          "population_data_experiment",
          label = "Target Population Data",
          tooltip = population_experiment_data_input_tooltip
        )
      ),
      shiny::conditionalPanel(
        condition = "input.analysis_type == 'observational'",
        shinyWrappers::fileInputUI(
          "observational_data",
          label = "Observational Study Data",
          tooltip = observational_data_input_tooltip
        )
      ),
      
      # Treatment Input --------------------------------------------------------
      shiny::conditionalPanel(
        condition = "input.analysis_type == 'experiment' || input.analysis_type == 'observational'",
        shinyWrappers::picker_input(
          "treatment",
          label = "Treatment Variable" |>
            shinyWrappers::add_tooltip_icon(id = "treatment"),
          choices = NULL
        ),
        shinyWrappers::add_tooltip("treatment", treatment_input_tooltip)
      ),
      
      # Y Input --------------------------------------------------------------
      shinyWrappers::picker_input(
        "Y",
        label = "Outcome of Interest" |>
          shinyWrappers::add_tooltip_icon(id = "Y"),
        choices = NULL
      ),
      shinyWrappers::add_tooltip("Y", y_input_tooltip),
      
      # Population Weights ----------------------------------------------------
      shiny::conditionalPanel(
        condition = "input.analysis_type == 'survey'",
        shinyWrappers::picker_input(
          "pop_weights_survey",
          label = "Population Weights (optional)" |>
            shinyWrappers::add_tooltip_icon(id = "pop_weights_survey"),
          choices = NULL
        ),
        shinyWrappers::add_tooltip("pop_weights_survey", population_weights_survey_tooltip)
      ),
      shiny::conditionalPanel(
        condition = "input.analysis_type == 'experiment'",
        shinyWrappers::picker_input(
          "pop_weights_experiment",
          label = "Population Weights (optional)" |>
            shinyWrappers::add_tooltip_icon(id = "pop_weights_experiment"),
          choices = NULL
        ),
        shinyWrappers::add_tooltip("pop_weights_experiment", population_weights_exp_tooltip)
      ),
      
      # Threshold --------------------------------------------------------------
      shiny::numericInput(
        "threshold",
        label = "Killer Confounder Threshold" |>
          shinyWrappers::add_tooltip_icon(id = "threshold"),
        value = 0,
        min = 0,
        step = 0.1
      ),
      shinyWrappers::add_tooltip("threshold", threshold_tooltip),
      
      # Weighting Section ------------------------------------------------------
      shinyWrappers::hr_short() |>
        shinyWrappers::set_margins(top = "2em", bottom = "1.75em"),
      input_header_style(shiny::icon("scale-balanced"), " Weighting Options"),
      
      # Population Weighting ---------------------------------------------------
      weightingOptionsUI(
        id = "sample",
        weighting_methods = c("None"),
        weighting_method_tooltip = weighting_method_tooltip,
        weighting_vars_manual_tooltip = weighting_vars_manual_tooltip,
        weighting_vars_tooltip = weighting_vars_tooltip,
        picker_width = picker_width
      ),
      
      # # Mode/Contrasts ---------------------------------------------------------
      # shinyWrappers::radio_buttons(
      #   "type",
      #   label = "Type" |>
      #     shinyWrappers::add_tooltip_icon(id = "type"),
      #   choices = c("Mean" = "mean", "Contrast" = "contrast")
      # ),
      # shinyWrappers::add_tooltip("type", type_tooltip),
      #
      # shiny::textInput(
      #   "contrast1",
      #   label = "Contrast1" |>
      #     shinyWrappers::add_tooltip_icon(id = "contrast1")
      # ),
      # shinyWrappers::add_tooltip("contrast1", contrast1_tooltip),
      #
      # shiny::textInput(
      #   "contrast2",
      #   label = "Contrast2" |>
      #     shinyWrappers::add_tooltip_icon(id = "contrast2")
      # ),
      # shinyWrappers::add_tooltip("contrast2", contrast2_tooltip),
      
      # Action Buttons ---------------------------------------------------------
      shinyWrappers::hr_short() |>
        shinyWrappers::set_margins(top = "2em", bottom = "1.75em"),
      shinyWrappers::reset_button(),
      shinyWrappers::continue_button("run", label = "Run Analysis")
    ),
    
    # Body ---------------------------------------------------------------------
    shinydashboard::dashboardBody(
      shinyWidgets::useSweetAlert(),
      shinyjs::useShinyjs(),
      shinyWrappers::use_pretty_style(),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "@import url('https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,300..800;1,300..800&family=Quattrocento:wght@400;700&family=Ubuntu:ital,wght@0,300;0,400;0,500;0,700;1,300;1,400;1,500;1,700&display=swap');
            @import url('https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,300..800;1,300..800&family=Quattrocento+Sans:ital,wght@0,400;0,700;1,400;1,700&family=Quattrocento:wght@400;700&family=Ubuntu:ital,wght@0,300;0,400;0,500;0,700;1,300;1,400;1,500;1,700&display=swap');
            @import url('https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap');"
          )
        )
      ),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "body {font-family: 'Quattrocento'; font-size: 16px};"
          )
        )
      ),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {font-family: 'Fira Sans';};"
          )
        )
      ),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "#analysis_type>.btn-group>.btn-group>.btn {padding: 6px 8px;};"
          )
        )
      ),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "label.control-label {font-family: 'Quattrocento Sans';};"
          )
        )
      ),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            ".main-footer {background: transparent; color: #a0a0a0; padding: 8px 16px 24px 8px; font-size: 13px;};"
          )
        )
      ),
      
      # Row 1 ------------------------------------------------------------------
      bslib::layout_columns(
        # Instructions ---------------------------------------------------------
        shinyWrappers::prettyBox(
          shinyWrappers::textboxUI("about"),
          title = "About",
          color = "slategray"
        ),
        # Summary Table --------------------------------------------------------
        shinyWrappers::prettyBox(
          shinyWrappers::download_button("download_summary_table"),
          shinyWrappers::tableUI("summary"),
          dropdownMenu = shinyWrappers::options_dropdown(
            shinyWrappers::tableOptionsUI("summary", digits = 2)
          ),
          title = "Summary Table"
        )
      ),
      
      # Row 2 ------------------------------------------------------------------
      bslib::layout_columns(
        # Benchmarking Plot ----------------------------------------------------
        shinyWrappers::prettyBox(
          # shinyWrappers::selectVizUI("benchmarking"),
          shiny::uiOutput("warning_text"),
          shinyWrappers::plotUI("benchmarking"),
          shinyWrappers::download_button("download_benchmarking_plot"),
          dropdownMenu = shinyWrappers::options_dropdown(
            contourPlotOptionsUI("benchmarking"),
            right = FALSE,
            width = "400px"
          ),
          title = "Bias Contour Plot",
          footer = benchmarking_plot_caption
        ),
        
        # Benchmarking Table -------------------------------------------------
        shinyWrappers::prettyBox(
          shinyWrappers::download_button("download_benchmarking_table"),
          shinyWrappers::tableUI("benchmarking"),
          dropdownMenu = shinyWrappers::options_dropdown(
            shinyWrappers::tableOptionsUI("benchmarking", digits = 2)
          ),
          shinyWrappers::picker_input(
            "var_interpret",
            label = "How do I interpret the benchmarking results for: ",
            # label = "I want to interpret: " |>
            #   shinyWrappers::add_tooltip_icon(id = "var_interpret"),
            choices = NULL,
            inline = TRUE,
            width = "fit"
          ),
          # shinyWrappers::add_tooltip("var_interpret", var_interpret_tooltip),
          shinyWrappers::textboxUI("interpretation"),
          title = "Benchmarking Table"
        )
      )
    )
  )
  
  # Server ---------------------------------------------------------------------
  server <- function(input, output, session) {
    
    # Update inputs ------------------------------------------------------------
    ## Update data input -------------------------------------------------------
    sampleDataInput <- shinyWrappers::fileInputServer(
      "sample_data", default_data = wapo
    )
    experimentDataInput <- shinyWrappers::fileInputServer(
      "experiment_data", default_data = experiment_sample
    )
    observationalDataInput <- shinyWrappers::fileInputServer(
      "observational_data", default_data = lalonde_robust
    )
    populationSurveyDataInput <- shinyWrappers::fileInputServer(
      "population_data_survey",
      default_data = ces
    )
    populationExperimentDataInput <- shinyWrappers::fileInputServer(
      "population_data_experiment",
      default_data = experiment_pop
    )
    mainDataInput <- shiny::reactive({
      if (input$analysis_type == "survey") {
        data <- sampleDataInput()
      } else if (input$analysis_type == "experiment") {
        data <- experimentDataInput()
      } else if (input$analysis_type == "observational") {
        data <- observationalDataInput()
      }
      return(data)
    })
    populationDataInput <- shiny::reactive({
      if (input$analysis_type == "survey") {
        data <- populationSurveyDataInput()
      } else if (input$analysis_type == "experiment") {
        data <- populationExperimentDataInput()
      } else if (input$analysis_type == "observational") {
        data <- NULL
      }
    })
    isDefaultDataInput <- shiny::reactive({
      if (input$analysis_type == "survey") {
        filenames <- c("sample_data", "population_data_survey")
      } else if (input$analysis_type == "experiment") {
        filenames <- c("experiment_data", "population_data_experiment")
      } else if (input$analysis_type == "observational") {
        filenames <- c("observational_data")
      }
      isDefault <- TRUE
      for (fname in filenames) {
        isDefault <- isDefault && !shinyWrappers::isFileUploaded(fname)()
      }
      return(isDefault)
    })
    
    ## Update variable choices if data input changes ---------------------------
    shiny::observe({
      sample_vars <- colnames(mainDataInput())
      if (isDefaultDataInput()) {
        selected <- dplyr::case_when(
          input$analysis_type == "experiment" ~ "T",
          input$analysis_type == "observational" ~ "treat",
          TRUE ~ "NA"
        )
        shinyWidgets::updatePickerInput(
          session, "treatment", choices = sample_vars, selected = selected
        )
      } else {
        shinyWidgets::updatePickerInput(
          session, "treatment", choices = sample_vars, selected = NULL
        )
      }
    })
    shiny::observe({
      sample_vars <- colnames(mainDataInput())
      if (isDefaultDataInput()) {
        selected <- dplyr::case_when(
          input$analysis_type == "survey" ~ "candidate",
          input$analysis_type == "experiment" ~ "Y",
          input$analysis_type == "observational" ~ "re78",
          TRUE ~ "NA"
        )
        shinyWidgets::updatePickerInput(
          session, "Y", choices = sample_vars, selected = selected
        )
      } else {
        shinyWidgets::updatePickerInput(
          session, "Y", choices = sample_vars, selected = NULL
        )
      }
    })
    shiny::observe({
      population_vars <- colnames(populationDataInput())
      if (isDefaultDataInput() && (input$analysis_type == "survey")) {
        shinyWidgets::updatePickerInput(
          session, sprintf("pop_weights_%s", input$analysis_type),
          choices = population_vars, selected = "vvweight_post"
        )
      } else {
        shinyWidgets::updatePickerInput(
          session, sprintf("pop_weights_%s", input$analysis_type),
          choices = population_vars, selected = character(0)
        )
      }
    })
    shiny::observe({
      if (isDefaultDataInput()) {
        value <- dplyr::case_when(
          input$analysis_type == "survey" ~ 0.5,
          input$analysis_type == "experiment" ~ 0,
          input$analysis_type == "observational" ~ 0,
          TRUE ~ NA
        )
        shiny::updateNumericInput(
          session, "threshold", value = value, min = 0, step = 0.1
        )
      } else {
        shiny::updateNumericInput(
          session, "threshold", value = 0, min = 0, step = 0.1
        )
      }
    })
    updateWeightingOptions(
      "sample", data = mainDataInput, is_default_data = isDefaultDataInput,
      Y = Y, treatment = treatment, analysis_type = analysis_type
    )
    shiny::observe({
      shinyWidgets::updatePickerInput(
        session, "var_interpret", choices = weightingVars()
      )
      benchmark_df <- get_benchmarking_table() |>
        dplyr::arrange(MRCS)
      shinyWidgets::updatePickerInput(
        session, "var_interpret",
        choices = weightingVars(),
        selected = benchmark_df$Variable[[1]]
      )
    })
    
    ## Shared reactive variables -----------------------------------------------
    analysis_type <- shiny::reactive({input$analysis_type})
    treatment <- shiny::reactive({input$treatment})
    Y <- shiny::reactive({input$Y})
    weightingMethod <- shiny::reactive({input$`sample-weighting_method`})
    weightingVars <- shiny::reactive({
      if (weightingMethod() == "manual") {
        input$`sample-weighting_vars_manual`
      } else {
        input$`sample-weighting_vars`
      }
    })
    popWeights <- shiny::reactive({
      pop_weights <- NULL
      if (input$analysis_type == "survey") {
        pop_weights <- populationDataInput()[[input$pop_weights_survey]]
      } else if (input$analysis_type == "experiment") {
        pop_weights <- populationDataInput()[[input$pop_weights_experiment]]
      }
      # if (!is.null(pop_weights)) {
      #   if (!is.numeric(pop_weights)) {
      #     stop("Population weights variable must be numeric.")
      #   }
      # }
      return(pop_weights)
    })
    threshold <- shiny::reactive({input$threshold})
    type <- shiny::reactive({input$type})
    var_interpret <- shiny::reactive({input$var_interpret})
    contrast1 <- shiny::reactive({input$contrast1})
    contrast2 <- shiny::reactive({input$contrast2})
    
    ## Instructions ------------------------------------------------------------
    shinyWrappers::textboxServer("instructions", instructions_text)
    shinyWrappers::textboxServer("about", about_text)
    
    ## Run Method --------------------------------------------------------------
    doSensitivityModule <- shiny::callModule(
      sensitivityFun, "sensitivity",
      analysis_type = analysis_type,
      mainDataInput = mainDataInput,
      populationDataInput = populationDataInput,
      treatment = treatment,
      Y = Y,
      weightingMethod = weightingMethod,
      weightingVars = weightingVars,
      popWeights = popWeights,
      threshold = threshold,
      type = type,
      contrast1 = contrast1,
      contrast2 = contrast2
    )
    doSensitivity <- shiny::eventReactive(input$run, {
      doSensitivityModule()
    })
    
    ## Warning Messages --------------------------------------------------------
    warning_text <- reactive({
      results <- doSensitivity()
      out <- NULL
      if (!is.null(results$warning_messages)) {
        out <- paste(
          shiny::tags$span("Warning Messages") |>
            htmltools::tagAppendAttributes(
              style = shinyWrappers::css_styler(
                color = "#A61C02",
                `font-size` = "12pt",
                `font-weight` = "bold"
              )
            ),
          do.call(
            shiny::tags$ul,
            purrr::map(results$warning_messages, shiny::tags$li)
          ),
          htmltools::br()
        ) |>
          htmltools::HTML()
      }
      return(out)
    })
    output$warning_text <- shiny::renderUI({
      warning_text()
    })
    
    ## Benchmarking (contour) plot ---------------------------------------------
    get_benchmarking_plot <- reactive({
      # require(ggplot2)
      results <- doSensitivity()
      plt <- contour_plot(
        varW = results$varW,
        sigma2 = results$sigma2,
        killer_confounder = results$killer_confounder,
        df_benchmark = results$df_benchmark,
        benchmark = results$benchmark,
        shade = results$shade,
        shade_var = results$shade_var,
        shade_fill = input$`benchmarking-shade_fill`,
        shade_alpha = input$`benchmarking-shade_alpha`,
        # contour_stroke = input$`benchmarking-contour_stroke`,
        contour_width = input$`benchmarking-contour_width`,
        binwidth = input$`benchmarking-binwidth`,
        label_size = input$`benchmarking-label_size`,
        point_size = input$`benchmarking-point_size`,
        nudge = input$`benchmarking-nudge`,
        axis_text_size = input$`benchmarking-display_text_size`,
        axis_title_size = input$`benchmarking-display_title_size`,
        axis_line_width = input$`benchmarking-display_awidth`
      )
      if (input$analysis_type != "experiment") {
        plt <- plt +
          ggplot2::labs(y = expression(rho[epsilon * ", " * y]))
      }
      return(plt)
    })
    shinyWrappers::plotServer(
      id = "benchmarking",
      plot_fun = get_benchmarking_plot,
      plot_options = FALSE,
      modes = "ggplot"
      # modes = c("ggplot", "plotly")
    )
    output$download_benchmarking_plot <- shiny::downloadHandler(
      filename = "bias_contour_plot.pdf",
      content = function(file) {
        plt <- get_benchmarking_plot()
        ggplot2::ggsave(
          file,
          plt,
          device = "pdf",
          height = 7,
          width = 8
        )
      }
    )
    
    ## Summary table -----------------------------------------------------------
    get_summary_table <- reactive({
      results <- doSensitivity()
      digits <- input$`summary-display_digits`
      sigfig <- input$`summary-display_sigfigs`
      if (sigfig) {
        dig_format <- "g"
      } else {
        dig_format <- "f"
      }
      if (input$analysis_type == "survey") {
        out <- results$df_sensitivity_summary |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ formatC(.x, digits = digits, format = dig_format, flag = "#")
            ),
            `Unweighted Estimate` = sprintf(
              "%s (%s)", Unweighted, Unweighted_SE
            ),
            `Weighted Estimate` = sprintf(
              "%s (%s)", Estimate, SE
            )
          ) |>
          dplyr::select(
            `Unweighted Estimate`, `Weighted Estimate`, RV
          )
      } else {
        results$df_sensitivity_summary |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ formatC(.x, digits = digits, format = dig_format, flag = "#")
            ),
            `Unweighted Estimate` = sprintf(
              "%s (%s)", Unweighted, Unweighted_SE
            ),
            `Weighted Estimate` = sprintf(
              "%s (%s)", Estimate, SE
            )
          ) |>
          dplyr::select(
            `Unweighted Estimate`, `Weighted Estimate`, RV
          )
      }
    })
    summary_table_caption <- reactive({
      rv <- get_summary_table()$RV[[1]]
      
      digits <- input$`summary-display_digits`
      if (isTRUE(is.na(digits))) {
        digits <- NULL
      }
      sigfig <- input$`summary-display_sigfigs`
      if (is.null(sigfig)) {
        sigfig <- FALSE
      }
      if (sigfig) {
        dig_format <- "g"
      } else {
        dig_format <- "f"
      }
      
      if (analysis_type() == "experiment") {
        var_type <- "moderator"
      } else {
        var_type <- "confounder"
      }
      
      rounded_rv <- formatC(
        rv, digits = digits, format = dig_format, flag = "#"
      )
      formatted_rv <- htmltools::tags$span(
        rounded_rv,
        style = shinyWrappers::css_styler(
          `font-weight` = 900,
          color = "#35a4bf"
        )
      )
      formatted_rv_percent <- htmltools::tags$span(
        paste0(as.numeric(rounded_rv) * 100, "%"),
        style = shinyWrappers::css_styler(
          `font-weight` = 900,
          color = "#35a4bf"
        )
      )
      htmltools::tags$caption(
        htmltools::tags$span(
          "The estimated robustness value is "
        ),
        formatted_rv,
        htmltools::tags$span(
          sprintf(
            ". This implies that the omitted %s would have to explain ",
            var_type
          )
        ),
        formatted_rv_percent,
        htmltools::tags$span(
          "of the variation in the outcome "
        ),
        htmltools::tags$i(
          "and"
        ),
        " the selection process in order to change our research conclusion."
      )
    })
    shinyWrappers::tableServer(
      id = "summary",
      table_fun = get_summary_table,
      mode = "DT",
      caption = summary_table_caption,
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE)
    )
    output$download_summary_table <- shiny::downloadHandler(
      filename = "summary_table.csv",
      content = function(file) {
        tab <- get_summary_table()
        write.csv(tab, file, row.names = FALSE)
      }
    )
    
    ## Benchmarking table ------------------------------------------------------
    get_benchmarking_table <- reactive({
      results <- doSensitivity()
      tab <- results$df_benchmark
      req(tab)
      tab <- tab |>
        dplyr::select(
          Variable = variable,
          `Benchmarked R-squared` = R2_benchmark,
          `Benchmarked rho` = rho_benchmark,
          `Estimated Bias` = bias,
          `MRCS` = MRCS
        )
    })
    shinyWrappers::tableServer(
      id = "benchmarking",
      table_fun = get_benchmarking_table,
      table_options = TRUE,
      mode = "DT",
      caption = benchmarking_table_caption,
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
    output$download_benchmarking_table <- shiny::downloadHandler(
      filename = "benchmarking_table.csv",
      content = function(file) {
        tab <- get_benchmarking_table()
        write.csv(tab, file, row.names = FALSE)
      }
    )
    
    ## Interpretation of results -----------------------------------------------
    interpretation_text <- reactive({
      shiny::req(var_interpret())
      benchmark_df <- get_benchmarking_table() |>
        dplyr::arrange(MRCS) |>
        dplyr::filter(Variable == !!var_interpret())
      
      digits <- input$`benchmarking-display_digits`
      if (isTRUE(is.na(digits))) {
        digits <- NULL
      }
      sigfig <- input$`benchmarking-display_sigfigs`
      if (is.null(sigfig)) {
        sigfig <- FALSE
      }
      if (sigfig) {
        dig_format <- "g"
      } else {
        dig_format <- "f"
      }
      
      if (analysis_type() == "experiment") {
        var_type <- "moderator"
      } else {
        var_type <- "confounder"
      }
      
      sprintf(
        "From benchmarking, we see that omitting a %4$s with equivalent strength as %1$s, will result in a bias of %2$s. This implies that an omitted %4$s would have to be %3$s times as strong as %1$s to be a killer confounder.",
        benchmark_df[["Variable"]][[1]] |>
          htmltools::span() |>
          htmltools::tagAppendAttributes(
            style = shinyWrappers::css_styler(
              `font-weight` = 900,
              color = "#35a4bf"
            )
          ),
        formatC(
          benchmark_df[["Estimated Bias"]][[1]],
          digits = digits, format = dig_format, flag = "#"
        ) |>
          htmltools::span() |>
          htmltools::tagAppendAttributes(
            style = shinyWrappers::css_styler(
              `font-weight` = 900,
              color = "#35a4bf"
            )
          ),
        formatC(
          benchmark_df[["MRCS"]][[1]],
          digits = digits, format = dig_format, flag = "#"
        ) |>
          htmltools::span() |>
          htmltools::tagAppendAttributes(
            style = shinyWrappers::css_styler(
              `font-weight` = 900,
              color = "#35a4bf"
            )
          ),
        var_type
      )
    })
    shinyWrappers::reactiveTextboxServer(
      "interpretation", interpretation_text
    )
    
    # Run example analysis on startup -----------------------------------------
    default_run <- observeEvent(input$analysis_type, {
      shinyjs::delay(
        300,
        {
          if (input$analysis_type %in% c("experiment", "observational")) {
            shiny::req(
              isDefaultDataInput(), Y(), treatment(), threshold(),
              weightingMethod(), weightingVars()
            )
            shinyjs::click("run")
          } else {
            shiny::req(
              isDefaultDataInput(), Y(), popWeights(), threshold(),
              weightingMethod(), weightingVars()
            )
            shinyjs::click("run")
          }
        }
      )
    })
    
    # Reset all inputs if want to start over -----------------------------------
    shinyWrappers::reset_inputs(
      input, session,
      reset_input_ids = c(
        "sample_data",
        "population_data_survey",
        "experiment_data",
        "population_data_experiment",
        "observational_data",
        "treatment",
        "Y",
        "pop_weights_survey",
        "pop_weights_experiment",
        "threshold",
        "sample-weighting_method",
        "sample-weighting_vars_manual",
        "sample-weighting_vars"
      )
    )
  }
  
  shiny::shinyApp(ui, server, ...)
}
