test_that("Input within a module cannot be accessed after module is destroyed", {
  basicModuleUI <- function(id) {
    ns <- NS(id)
    actionButton(ns("click"), "Click Button")
  }

  basicModuleServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveVal(0L)
      observeEvent(input$click, rv(rv() + 1L))
      rv
    })
  }

  destroyableModuleUI <- makeModuleUIDestroyable(basicModuleUI)
  destroyableModuleServer <- makeModuleServerDestroyable(basicModuleServer)

  ui <- fluidPage(
    destroyableModuleUI(id = "test"),
    actionButton("destroy", "Destroy module"),
    textOutput("reactive_value")
  )

  server <- function(input, output, session) {
    top_rv <- reactiveVal()
    reactive_value <- destroyableModuleServer("test")
    observeEvent(reactive_value(), top_rv(reactive_value()))

    output$reactive_value <- renderText(top_rv())

    observeEvent(input$destroy, destroyModule("test"))
  }

  app <- shinytest2::AppDriver$new(shinyApp(ui, server), name = "basic_app")
  on.exit(app$stop())

  expect_equal(app$get_value(input = "test-click"), 0L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "reactive_value"), "0")

  app$click(input = "test-click")
  expect_equal(app$get_value(input = "test-click"), 1L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "reactive_value"), "1")

  app$click(input = "destroy")
  expect_error(app$click(input = "test-click"))
  expect_null(app$get_value(input = "test-click"))
  expect_identical(app$get_value(output = "reactive_value"), "1")
})

test_that("Input is not passed to module after module is destroyed", {
  basicModuleServer <- function(id, click) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveVal(0L)
      observeEvent(click(), rv(rv() + 1L))
      rv
    })
  }

  destroyableModuleServer <- makeModuleServerDestroyable(basicModuleServer)

  ui <- fluidPage(
    actionButton("click", "Click Button"),
    actionButton("destroy", "Destroy module"),
    textOutput("reactive_value")
  )

  server <- function(input, output, session) {
    top_rv <- reactiveVal()
    reactive_value <- destroyableModuleServer("test", click = reactive(input$click))
    observeEvent(reactive_value(), top_rv(reactive_value()))

    output$reactive_value <- renderText(top_rv())

    observeEvent(input$destroy, {print(1); destroyModule("test")})
  }

  app <- shinytest2::AppDriver$new(shinyApp(ui, server), name = "basic_app")
  on.exit(app$stop())

  expect_equal(app$get_value(input = "click"), 0L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "reactive_value"), "0")

  app$click(input = "click")
  expect_equal(app$get_value(input = "click"), 1L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "reactive_value"), "1")

  app$click(input = "destroy")
  app$click(input = "click")
  expect_equal(app$get_value(input = "click"), 2L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "reactive_value"), "1")
})
