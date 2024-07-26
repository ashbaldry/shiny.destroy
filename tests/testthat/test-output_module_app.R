test_that("Input within a module cannot be accessed after module is destroyed", {
  basicModuleServer <- function(id, text_input) {
    moduleServer(id, function(input, output, session) {
      output$text <- renderText({
        session$userData$text <- text_input()
        text_input()
      })
    })
  }

  destroyableModuleServer <- makeModuleServerDestroyable(basicModuleServer)

  ui <- fluidPage(
    textInput("text", "Input text value"),
    actionButton("destroy", "Destroy module"),
    hr(),
    p("Server output:", textOutput("text_input", inline = TRUE)),
    p("Module output:", textOutput("test-text", inline = TRUE))
  )

  server <- function(input, output, session) {
    reactive_value <- destroyableModuleServer("test", text_input = reactive(input$text))

    output$text_input <- renderText(paste("Test:", input$text))

    observeEvent(input$destroy, destroyModule("test"))
  }

  app <- shinytest2::AppDriver$new(shinyApp(ui, server), name = "output_app")
  on.exit(app$stop())

  expect_equal(app$get_value(input = "text"), "", ignore_attr = TRUE)
  expect_equal(app$get_value(output = "test-text"), "", ignore_attr = TRUE)

  app$set_inputs(text = "foo")
  expect_equal(app$get_value(input = "text"), "foo", ignore_attr = TRUE)
  expect_equal(app$get_value(output = "text_input"), "Test: foo", ignore_attr = TRUE)
  expect_equal(app$get_value(output = "test-text"), "foo", ignore_attr = TRUE)

  app$click(input = "destroy")
  expect_equal(app$get_value(input = "text"), "foo", ignore_attr = TRUE)
  expect_equal(app$get_value(output = "text_input"), "Test: foo", ignore_attr = TRUE)
  # If ever to fully remove an output, this test will update. For now,
  # have to settle with validation error
  expect_setequal(app$get_value(output = "test-text")$type, c("shiny.silent.error", "validation"))
})
