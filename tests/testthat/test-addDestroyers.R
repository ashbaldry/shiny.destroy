test_that("addDestroyers adds each observer to the session userData", {
  basic_call <- function(input, output, session) {
    x <- reactive("foo")
    observe("bar")

    invisible(NULL)
  }

  destroy_call <- addModuleDestroyers(basic_call)
})

test_that("addDestroyers adds each observer to the session userData", {
  basic_call <- function(input, output, session) {
    x <- reactive("foo")
    observe("bar")
    observeEvent(input$x, {
      y <- 2 + 6
      z <- 4 + 7
      paste(x(), y, z)
    })

    lab <- observe("baz")

    invisible(NULL)
  }

  destroy_call <- addModuleDestroyers(basic_call)
})
