test_that("Input is fully removed from shiny application", {
  ui <- fluidPage(
    numericInput("number", "Choose a number", 5, 1, 10),
    actionButton("destroy", "Destroy input"),
    textOutput("number_input")
  )

  server <- function(input, output, session) {
    output$number_input <- renderText(input$number)
    observeEvent(input$destroy, removeInput("number"))
  }

  app <- shinytest2::AppDriver$new(shinyApp(ui, server), name = "basic_app")
  on.exit(app$stop())

  expect_equal(app$get_value(input = "number"), 5L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "number_input"), "5")

  app$click(input = "destroy")
  expect_error(app$click(input = "number"))
  expect_null(app$get_value(input = "number"))
  expect_identical(app$get_value(output = "number_input"), "")
})
