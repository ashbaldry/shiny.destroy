library(bslib)
library(shiny)
library(shiny.destroy)

boxModuleUI <- function(id) {
  ns <- NS(id)

  bslib::card(
    id = ns("card"),
    h3("Box for", sub("_", " ", id)),
    actionButton(ns("destroy"), "Remove card")
  )
}

boxModuleServer <- function(id, create_card) {
  moduleServer(id, function(input, output, session) {
    observeEvent(create_card(), Sys.sleep(1L))
    observeEvent(input$destroy, destroyModule())
  })
}

destroyableBoxModuleUI <- makeModuleUIDestroyable(boxModuleUI)
destroyableBoxModuleServer <- makeModuleServerDestroyable(boxModuleServer)

comparisonModuleUI <- function(id, title) {
  ns <- NS(id)

  tagList(
    h2(title),
    bslib::layout_columns(
      div(
        p("Number of boxes:", textOutput(ns("n_modules"), inline = TRUE)),
        actionButton(ns("create"), "Create new module")
      ),
      div(
        h3("Inputs"),
        verbatimTextOutput(ns("inputs"))
      ),
      col_widths = c(3, 9)
    ),
    div(id = ns("cards"))
  )
}

comparisonModuleServer <- function(id, destroyable = FALSE) {
  module_server_fn <- if (destroyable) destroyableBoxModuleServer else boxModuleServer
  module_ui_fn <- if (destroyable) destroyableBoxModuleUI else boxModuleUI

  moduleServer(id, function(input, output, session) {
    card_id <- reactiveVal(1L)
    ns <- session$ns

    observeEvent(input$create, {
      ns_id <- paste0("card_", card_id())
      cards_id <- ns("cards")
      insertUI(paste0("#", cards_id), "beforeEnd", module_ui_fn(id = ns(ns_id)))
      module_server_fn(id = ns_id, create_card = reactive(input$create))

      card_id(card_id() + 1L)
    })

    output$n_modules <- renderText({
      sum(grepl(ns("destroy"), names(input)))
    })

    output$inputs <- renderPrint({
      inputs <- vapply(names(input), \(x) paste0(x, ": ", input[[x]]), character(1L))
      cat(inputs, sep = "\n")
    })
  })
}

ui <- bslib::page_fluid(
  title = "shiny.destroy comparison",
  h1("shiny v shiny.destroy comparison"),
  p(
    "For each box module created, a second will elapse. As more boxes are created, the longer",
    "it will take for the app to become responsive again."
  ),
  p(
    "Click the new modules and see the time comparison it takes to create a new destroyable",
    "module after some boxes have been deleted compared to using standard shiny calls"
  ),
  bslib::layout_columns(
    comparisonModuleUI("shiny", "Shiny modules"),
    comparisonModuleUI("shiny_destroy", "Destroyable modules"),
    fill = FALSE
  )
)

server <- function(input, output, session) {
  comparisonModuleServer("shiny")
  comparisonModuleServer("shiny_destroy", destroyable = TRUE)
}

shinyApp(ui, server)
