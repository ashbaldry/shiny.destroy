#' Remove Output from Shiny Session
#'
#' The removal of the named output in a shiny session.
#'
#' @param id Output value name
#' @param selector The HTML selector to remove the UI for. By default it is the
#' tag where the ID matches the output, but might need to be adjusted for
#' different inputs.
#' @param session The Shiny session to remove the output from
#'
#' @return
#' An invisible `TRUE` value confirming that the output has been removed.
#'
#' @examplesIf interactive()
#' library(shiny)
#' library(shiny.destroy)
#'
#' ui <- fluidPage(
#'   numericInput("number", "Select number:", 5, 1, 10),
#'   p("Selected number:", textOutput("number_out", inline = TRUE)),
#'   actionButton("delete", "Remove output")
#' )
#'
#' server <- function(input, output, session) {
#'   output$number_out <- renderText(input$number)
#'
#'   observeEvent(
#'     input$delete,
#'     removeOutput("number_out")
#'    )
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
removeOutput <- function(id, selector = paste0("#", id), session = getDefaultReactiveDomain()) {
  shiny::removeUI(selector, immediate = TRUE, session = session)

  destroyOutput(id, session = session)
  session$requestFlush()

  invisible(TRUE)
}

#' Remove Output from Shiny Session
#'
#' The removal of the named output in a shiny session.
#'
#' @param id Output value name
#' @param session The Shiny session to remove the output from
#'
#' @noRd
destroyOutput <- function(id, session = getDefaultReactiveDomain()) {
  session$defineOutput(id, NULL, NULL)
  session$.__enclos_env__$private$.outputs[[id]] <- NULL
  session$.__enclos_env__$private$.outputOptions[[id]] <- NULL
}
