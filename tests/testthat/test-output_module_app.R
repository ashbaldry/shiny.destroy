test_that("Input within a module cannot be accessed after module is destroyed", {
  basicModuleUI <- function(id) {
    ns <- NS(id)

    textOutput(ns("text"))
  }

  basicModuleServer <- function(id, text_input) {
    moduleServer(id, function(input, output, session) {
      output$text <- renderText({
        session$userData$text <- text_input()
        text_input()
      })
    })
  }

  destroyableModuleUI <- makeModuleUIDestroyable(basicModuleUI)
  destroyableModuleServer <- makeModuleServerDestroyable(basicModuleServer)

  ui <- fluidPage(
    textInput("text", "Input text value"),
    actionButton("destroy", "Destroy module"),
    # destroyableModuleUI(id = "test"),
    hr(),
    p("Module output:", textOutput("test-text", inline = TRUE))
  )

  server <- function(input, output, session) {
    top_rv <- reactiveVal()
    reactive_value <- destroyableModuleServer("test", text_input = reactive(input$text))
    observeEvent(reactive_value(), top_rv(reactive_value()))

    output$reactive_value <- renderText(top_rv())

    observeEvent(input$destroy, destroyModule("test"))
  }

  app <- shinytest2::AppDriver$new(shinyApp(ui, server), name = "output_app")
  on.exit(app$stop())

  expect_equal(app$get_value(input = "text"), "", ignore_attr = TRUE)
  expect_equal(app$get_value(output = "test-text"), "", ignore_attr = TRUE)

  app$set_inputs(text = "foo")
  expect_equal(app$get_value(input = "text"), "foo", ignore_attr = TRUE)
  expect_equal(app$get_value(output = "test-text"), "foo", ignore_attr = TRUE)

  app$click(input = "destroy")
  expect_equal(app$get_value(input = "text"), "foo", ignore_attr = TRUE)
  expect_null(app$get_value(output = "test-text"))
})
