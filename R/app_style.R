#' @keywords internal
input_header_style <- function(...) {
  shiny::span(...) |>
    shiny::h4() |>
    htmltools::tagAppendAttributes(
      style = shinyWrappers::css_styler(
        `margin-left` = "0.5em",
        `margin-bottom` = "0em",
        `font-weight` = "bold",
        `font-size` = "20px",
        `font-variant` = "small-caps"
      )
    )
}


#' @keywords internal
input_subheader_style <- function(...) {
  shiny::span(...) |>
    shiny::h4() |>
    htmltools::tagAppendAttributes(
      style = shinyWrappers::css_styler(
        `margin-left` = "0.85em",
        `margin-bottom` = "-0.05em",
        `font-weight` = "900",
        `text-decoration` = "underline",
        `font-style` = "italic",
        `font-size` = "16px"
      )
    )
}


#' @keywords internal
indent_style <- function(..., indent = "0.85em") {
  htmltools::div(...) |>
    htmltools::tagAppendAttributes(
      style = shinyWrappers::css_styler(
        `padding-left` = indent
      )
    )
}


#' @keywords internal
add_my_tooltip_icon <- function (..., id, icon = shiny::icon("info-circle"),
                                 style) {
  ns <- shiny::NS(id)
  htmltools::span(
    ...,
    htmltools::span(icon, id = ns("tooltip")) |>
      htmltools::tagAppendAttributes(style = style)
  )
}

