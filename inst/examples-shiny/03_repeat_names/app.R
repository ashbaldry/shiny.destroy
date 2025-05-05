library(bslib)
library(shiny)
library(shiny.destroy)

numericModuleUI <- function(id) {
  ns <- NS(id)

  bslib::card(
    id = ns("card"),
    h3(id),
    sliderInput(
      ns("slider"),
      label = "Range",
      min = min(iris[[id]]),
      max = max(iris[[id]]),
      value = range(iris[[id]]),
      step = 0.1
    ),
    actionButton(ns("destroy"), "Remove card")
  )
}

numericModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$slider, ignoreInit = TRUE, {
      session$userData$filters[[id]] <- input$slider
    })

    observeEvent(input$destroy, {
      destroyModule()
      session$userData$filters[[id]] <- NULL
      session$userData$col_choices(
        intersect(names(iris), c(id, session$userData$col_choices()))
      )
    })
  })
}

destroyableNumericModuleUI <- makeModuleUIDestroyable(numericModuleUI)
destroyableNumericModuleServer <- makeModuleServerDestroyable(numericModuleServer)

ui <- bslib::page_fluid(
  h1("Destroyable Filters"),
  span(
    selectInput("column", "Select Column", head(names(iris), -1L)),
    actionButton("create", "Create new filter")
  ),
  bslib::layout_columns(
    div(id = "filters"),
    div(
      h2("Iris"),
      DT::DTOutput("iris")
    ),
    col_widths = c(4, 8)
  )
)

server <- function(input, output, session) {
  session$userData$col_choices <- reactiveVal(head(names(iris), -1L))
  session$userData$filters <- reactiveValues()

  observeEvent(input$create, {
    insertUI("#filters", "beforeEnd", destroyableNumericModuleUI(id = input$column))
    destroyableNumericModuleServer(id = input$column)
    session$userData$col_choices(setdiff(session$userData$col_choices(), input$column))
  })

  observeEvent(session$userData$col_choices(), {
    updateSelectInput(inputId = "column", choices = session$userData$col_choices())
  })

  iris_filtered <- reactive({
    iris_x <- iris

    for (col_name in names(session$userData$filters)) {
      filter_vals <- session$userData$filters[[col_name]]
      if (length(filter_vals) == 2L) {
        iris_x <- iris_x[
          iris_x[[col_name]] >= filter_vals[1L] & iris_x[[col_name]] <= filter_vals[2L],
        ]
      }
    }

    iris_x
  })

  output$iris <- DT::renderDT(iris_filtered())
}

shinyApp(ui, server)
