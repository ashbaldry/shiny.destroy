test_that("Output is fully removed from shiny application", {
  testthat::skip_on_cran()

  ui <- fluidPage(
    numericInput("number", "Choose a number", 5L, 1L, 10L),
    actionButton("destroy", "Destroy input"),
    textOutput("number_input")
  )

  server <- function(input, output, session) {
    output$number_input <- renderText(input$number)
    observeEvent(input$destroy, removeOutput("number_input"))
  }

  app <- shinytest2::AppDriver$new(shinyApp(ui, server), name = "basic_app")
  on.exit(app$stop())

  expect_equal(app$get_value(input = "number"), 5L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "number_input"), "5")

  app$click(input = "destroy", wait_ = FALSE)
  expect_equal(app$get_value(input = "number"), 5L, ignore_attr = TRUE)
  # If ever to fully remove an output, this test will update. For now,
  # have to settle with validation error
  expect_setequal(app$get_value(output = "number_input")$type, c("shiny.silent.error", "validation"))
})
