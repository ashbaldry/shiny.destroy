#' Destroy Shiny Module
#'
#' @description
#' Given the namespace of a shiny module, remove all references
#' to the inputs, outputs and observers that are called within
#' the module.
#'
#' @param id The module namespace ID
#' @param session The shiny session, by default it is `shiny::getDefaultReactiveDomain()`
#'
#' @examplesIf interactive()
#' library(shiny)
#'
#' basicModuleUI <- function(id) {
#'   ns <- NS(id)
#'   actionButton(ns("click"), "Click Button")
#' }
#'
#' basicModuleServer <- function(id) {
#'   moduleServer(id, function(input, output, session) {
#'     rv <- reactiveVal(0L)
#'     observeEvent(input$click, rv(rv() + 1L))
#'     rv
#'   })
#' }
#'
#' destroyableModuleUI <- makeModuleUIDestroyable(basicModuleUI)
#' destroyableModuleServer <- makeModuleServerDestroyable(basicModuleServer)
#'
#' ui <- fluidPage(
#'   destroyableModuleUI(id = "test"),
#'   actionButton("destroy", "Destroy module"),
#'   textOutput("reactive_value")
#' )
#'
#' server <- function(input, output, session) {
#'   top_rv <- reactiveVal()
#'   reactive_value <- destroyableModuleServer("test")
#'   observeEvent(reactive_value(), top_rv(reactive_value()))
#'
#'   output$reactive_value <- renderText(top_rv())
#'
#'   observeEvent(input$destroy, destroyModule("test"))
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
destroyModule <- function(id = NULL, session = getDefaultReactiveDomain()) {
  destroyModuleUI(id, session = session)
  destroyModuleServer(id, session = session)

  invisible(NULL)
}

#' Destroy the UI Component of a Shiny Module
#' @noRd
destroyModuleUI <- function(id, session = getDefaultReactiveDomain()) {
  ns_id <- session$ns(id)
  shiny::removeUI(selector = paste0("[shiny-destroy=\"", ns_id, "\"]"), immediate = TRUE)

  input <- session$input
  purrr::walk(names(input), \(x) {
    x_id <- if (is.null(id)) session$ns(x) else x
    if (startsWith(x_id, ns_id)) destroyInput(x_id, session)
  })

  input_obj <- .subset2(input, "impl")
  input_obj$.namesDeps$invalidate()
  input_obj$.valuesDeps$invalidate()

  invisible(NULL)
}

#' Destroy the Server Component of a Shiny Module
#' @noRd
destroyModuleServer <- function(id, session = getDefaultReactiveDomain()) {
  observers <- session$userData$.shiny.destroy
  ns_id <- session$ns(id)
  contains_id <- startsWith(names(observers), ns_id)

  output <- session$.__enclos_env__$private$.outputs
  purrr::walk(names(output), \(x) {
    x_id <- if (is.null(id)) session$ns(x) else x
    if (startsWith(x_id, ns_id)) destroyOutput(x_id, session)
  })
  session$requestFlush()

  observers <- unlist(observers[contains_id], recursive = FALSE)
  purrr::walk(observers, \(x) x$destroy())

  session$userData$.shiny.destroy <- session$userData$.shiny.destroy[!contains_id]

  invisible(NULL)
}

#' Remove Input from Shiny Session
#'
#' The removal of the named input in a shiny session.
#'
#' @param id Input value name
#' @param session The Shiny session to remove the input from
#'
#' @noRd
destroyInput <- function(id, session = getDefaultReactiveDomain()) {
  input <- .subset2(session$input, "impl")

  input$.values$remove(id)
  input$.nameOrder <- setdiff(input$.nameOrder, id)
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
