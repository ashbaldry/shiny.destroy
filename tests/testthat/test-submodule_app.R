test_that("Input within a sub-module cannot be accessed after module is destroyed", {
  basicSubModuleUI <- function(id) {
    ns <- NS(id)
    actionButton(ns("click"), "Click Button")
  }

  basicSubModuleServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveVal(0L)
      observeEvent(input$click, rv(rv() + 1L))
      rv
    })
  }

  basicModuleUI <- function(id) {
    ns <- NS(id)
    basicSubModuleUI(ns("submodule"))
  }

  basicModuleServer <- function(id, subModuleServer = basicSubModuleServer) {
    moduleServer(id, function(input, output, session) {
      subModuleServer("submodule")
    })
  }

  destroyableModuleUI <- makeModuleUIDestroyable(basicModuleUI)
  destroyableModuleServer <- makeModuleServerDestroyable(basicModuleServer)
  destroyableSubModuleServer <- makeModuleServerDestroyable(basicSubModuleServer)

  ui <- fluidPage(
    destroyableModuleUI(id = "test"),
    actionButton("destroy", "Destroy module"),
    textOutput("reactive_value")
  )

  server <- function(input, output, session) {
    top_rv <- reactiveVal()
    reactive_value <- destroyableModuleServer("test", subModuleServer = destroyableSubModuleServer)
    observeEvent(reactive_value(), top_rv(reactive_value()))

    output$reactive_value <- renderText(top_rv())

    observeEvent(input$destroy, destroyModule("test"))
  }

  app <- shinytest2::AppDriver$new(shinyApp(ui, server), name = "basic_app")
  on.exit(app$stop())

  expect_equal(app$get_value(input = "test-submodule-click"), 0L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "reactive_value"), "0")

  app$click(input = "test-submodule-click")
  expect_equal(app$get_value(input = "test-submodule-click"), 1L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "reactive_value"), "1")

  app$click(input = "destroy")
  expect_error(app$click(input = "test-submodule-click"))
  expect_null(app$get_value(input = "test-submodule-click"))
  expect_identical(app$get_value(output = "reactive_value"), "1")
})
