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
    observeEvent(input$destroy, removeUI(paste0("#", session$ns("card"))))
  })
}

ui <- bslib::page_fluid(
  h1("Removable Modules"),
  p("Number of boxes:", textOutput("n_modules", inline = TRUE)),
  actionButton("create", "Create new module"),
  bslib::layout_columns(
    div(id = "cards"),
    div(
      h2("Inputs"),
      verbatimTextOutput("inputs")
    ),
    col_widths = c(8, 4)
  )
)

server <- function(input, output, session) {
  card_id <- reactiveVal(1L)

  observeEvent(input$create, {
    ns_id <- paste0("card_", card_id())
    insertUI("#cards", "beforeEnd", boxModuleUI(id = ns_id))
    boxModuleServer(id = ns_id, create_card = reactive(input$create))

    card_id(card_id() + 1L)
  })

  output$n_modules <- renderText({
    sum(grepl("destroy", names(input)))
  })

  output$inputs <- renderPrint({
    inputs <- vapply(names(input), \(x) paste0(x, ": ", input[[x]]), character(1L))
    cat(inputs, sep = "\n")
  })
}

shinyApp(ui, server)
