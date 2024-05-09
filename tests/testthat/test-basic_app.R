test_that("Module can be destroyed within an application", {
  ui <- fluidPage(
    destroyableModuleUI(
      id = "test",
      actionButton("test-click", "Click Button")
    ),
    actionButton("destroy", "Destroy module"),
    textOutput("reactive_value")
  )

  basicModule <- function(id) {
    destroyableModuleServer(id, function(input, output, session) {
      rv <- reactiveVal(0L)
      observeEvent(input$click, rv(rv() + 1L))
      rv
    })
  }

  server <- function(input, output, session) {
    top_rv <- reactiveVal()
    reactive_value <- basicModule("test")
    observeEvent(reactive_value(), top_rv(reactive_value()))

    output$reactive_value <- renderText(top_rv())

    observeEvent(input$destroy, destroyModule("test"))
  }

  app <- shinytest2::AppDriver$new(shinyApp(ui, server), name = "basic_app")
  on.exit(app$stop())

  expect_identical(app$get_value(output = "reactive_value"), "0")

  app$click(input = "test-click")
  expect_identical(app$get_value(output = "reactive_value"), "1")

  app$click(input = "destroy")
  expect_error(app$click(input = "test-click"))
  expect_identical(app$get_value(output = "reactive_value"), "1")
})
