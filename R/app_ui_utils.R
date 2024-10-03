#' UI for weighting options
#' @keywords internal
weightingOptionsUI <- function(id,
                               weighting_methods,
                               weighting_method_tooltip = NULL,
                               weighting_vars_manual_tooltip = NULL,
                               weighting_vars_tooltip = NULL,
                               picker_width = "100%") {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Select Weighting Method --------------------------------------------------
    shinyWrappers::radio_buttons(
      inputId = ns("weighting_method"),
      label = "Weighting Method", #|>
      # shinyWrappers::add_tooltip_icon(id = ns("weighting_method")),
      choices = weighting_methods
    ),
    # shinyWrappers::add_tooltip(
    #   ns("weighting_method"), weighting_method_tooltip
    # ),
    
    # Select Weighting Variables -----------------------------------------------
    shiny::conditionalPanel(
      condition = sprintf("input['%1$s'] == 'manual'", ns("weighting_method")),
      shinyWrappers::picker_multiple_input(
        inputId = ns("weighting_vars_manual"),
        label = "Column Name of Weights" |>
          shinyWrappers::add_tooltip_icon(id = ns("weighting_vars_manual")),
        choices = NULL,
        selected = character(0),
        width = picker_width
      ),
      shinyWrappers::add_tooltip(
        ns("weighting_vars_manual"), weighting_vars_manual_tooltip
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%1$s'] !== 'manual'", ns("weighting_method")),
      shinyWrappers::picker_multiple_input(
        inputId = ns("weighting_vars"),
        label = "Weighting Variables" |>
          shinyWrappers::add_tooltip_icon(id = ns("weighting_vars")),
        choices = NULL,
        selected = character(0),
        width = picker_width
      ),
      shinyWrappers::add_tooltip(
        ns("weighting_vars"), weighting_vars_tooltip
      )
    )
  )
}


#' Update weighting options when data changes
#' @keywords internal
updateWeightingOptions <- function(id, data, is_default_data,
                                   Y, treatment, analysis_type) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      vars <- colnames(data())
      y_var <- Y()
      treatment_var <- treatment()
      if (analysis_type() == "survey") {
        weighting_methods <- c(
          "Raking" = "raking",
          "Manual Input" = "manual"
        )
      } else if (analysis_type() %in% c("experiment", "observational")) {
        weighting_methods <- c(
          "Entropy Balancing" = "ebal",
          "Propensity Score (Logit)" = "ps",
          "Covariate Balancing Propensity Score" = "cbps"
        )
      }
      shinyWidgets::updatePrettyRadioButtons(
        session, "weighting_method", choices = weighting_methods
      )
      if (is_default_data()) {
        selected <- character(0)
        if (analysis_type() == "survey") {
          selected <- c(
            "pid", "educ", "age_buckets", "bornagain", "gender", "race"
          )
        } else if (analysis_type() == "experiment") {
          selected <- setdiff(vars, c(y_var, treatment_var, "site"))
        } else if (analysis_type() == "observational") {
          selected <- c(
            "age", "education", "black", "hispanic", "married", "nodegree"
          )
        }
        shinyWidgets::updatePickerInput(
          session, "weighting_vars",
          choices = setdiff(vars, c(y_var, treatment_var)),
          selected = selected
        )
      } else {
        shinyWidgets::updatePickerInput(
          session, "weighting_vars",
          choices = setdiff(vars, c(y_var, treatment_var)),
          selected = character(0)
        )
      }
      shinyWidgets::updatePickerInput(
        session, "weighting_vars_manual",
        choices = setdiff(vars, c(y_var, treatment_var)),
        selected = character(0)
      )
    })
  })
}


#' Custom plot options for contour plot
#' @keywords internal
contourPlotOptionsUI <- function(id,
                                 total_width = 12, height = 500,
                                 axis_text_size = 15, axis_title_size = 20,
                                 axis_line_width = 1, other_options = NULL) {
  ns <- shiny::NS(id)
  
  basic_opts <- shiny::tagList(
    shiny::numericInput(
      ns("label_size"), "Label Size", value = 6
    ),
    shiny::numericInput(
      ns("point_size"), "Point Size", value = 3,
    ),
    shiny::numericInput(
      ns("display_title_size"),
      "Axis Title Size",
      axis_title_size
    ),
    shiny::numericInput(
      ns("display_text_size"),
      "Axis Text Size",
      axis_text_size
    ),
    shiny::numericInput(
      ns("display_awidth"),
      "Axis Line Width",
      axis_line_width
    )
  )
  
  contour_opts <- shiny::tagList(
    shiny::textInput(
      ns("shade_fill"), "Shade Fill", value = "#35a4bf"
    ),
    shiny::numericInput(
      ns("shade_alpha"), "Shade Transparency", value = 0.25
    ),
    # shiny::numericInput(
    #   ns("contour_stroke"), "Contour Stroke", value = 0.2
    # ),
    shiny::numericInput(
      ns("contour_width"), "Contour Line Width", value = 1
    ),
    shiny::numericInput(
      ns("binwidth"), "Contour Binwidth", value = NULL
    ),
    shiny::numericInput(
      ns("nudge"), "Nudge Labels", value = 0.05
    ),
    shiny::numericInput(
      ns("display_height"),
      "Plot Height (px)", height
    )
  )
  
  opts <- shiny::tagList(
    shiny::column(6, basic_opts),
    shiny::column(6, contour_opts)
  )
  
  return(opts)
}
