#' Remove Input from Shiny Session
#'
#' The removal of the named input in a shiny session.
#'
#' @param id Input value name
#' @param session The Shiny session to remove the input from
#'
#' @return
#' An invisible `TRUE` value confirming that the input has been removed.
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
#'   output$number_out <- renderText(input$number)
#'
#'   observeEvent(input$delete, removeInput("number"))
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
removeInput <- function(id, session = getDefaultReactiveDomain()) {
  shiny::removeUI(paste0("#", id), immediate = TRUE, session = session)

  destroyInput(id, session = session)
  invalidateInputs(session)

  invisible(TRUE)
}

destroyInput <- function(id, session = getDefaultReactiveDomain()) {
  input <- .subset2(session$input, "impl")

  input$.values$remove(id)
  input$.nameOrder <- setdiff(input$.nameOrder, id)
}

invalidateInputs <- function(session = getDefaultReactiveDomain()) {
  input <- .subset2(session$input, "impl")

  input$.namesDeps$invalidate()
  input$.valuesDeps$invalidate()
}
