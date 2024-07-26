#' Remove Input from Shiny Session
#'
#' The removal of the named input in a shiny session.
#'
#' @param id Input value name
#' @param selector The HTML selector to remove the UI for. By default it is the
#' tag where the ID matches the input, but might need to be adjusted for
#' different inputs.
#' @param session The Shiny session to remove the input from
#'
#' @return
#' An invisible `TRUE` value confirming that the input has been removed.
#'
#' @details
#' If the input is a standard `{shiny}` input e.g. `numericInput`, then to
#' remove the label as well as the input, set the selector to be
#' `paste0(":has(> #", id, ")")`
#'
#'
#' @examplesIf interactive()
#' library(shiny)
#' library(shiny.destroy)
#'
#' ui <- fluidPage(
#'   numericInput("number", "Select number:", 5, 1, 10),
#'   p("Selected number:", textOutput("number_out", inline = TRUE)),
#'   actionButton("delete", "Remove input")
#' )
#'
#' server <- function(input, output, session) {
#'   output$number_out <- renderText(input$number %||% "input unavailable")
#'
#'   observeEvent(
#'     input$delete,
#'     removeInput("number", selector = ":has(> #number)")
#'    )
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
removeInput <- function(id, selector = paste0("#", id), session = getDefaultReactiveDomain()) {
  shiny::removeUI(selector, immediate = TRUE, session = session)

  destroyInput(id, session = session)
  invalidateInputs(session)

  invisible(TRUE)
}

destroyInput <- function(id, session = getDefaultReactiveDomain()) {
  session$manageInputs(stats::setNames(list(NULL), id))

  input <- .subset2(session$input, "impl")

  input$.values$remove(id)
  input$.nameOrder <- setdiff(input$.nameOrder, id)
}

invalidateInputs <- function(session = getDefaultReactiveDomain()) {
  input <- .subset2(session$input, "impl")

  input$.namesDeps$invalidate()
  input$.valuesDeps$invalidate()
}
