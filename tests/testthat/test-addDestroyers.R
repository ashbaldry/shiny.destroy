test_that("addDestroyers adds each observer to the session userData", {
  basic_module <- quote(
    function(input, output, session) {
      x <- reactive("foo")
      observe("bar")
      invisible(NULL)
    }
  )

  destroy_module <- addDestroyers(basic_module)
  destroy_body <- destroy_module[[3L]] |> as.list()

  expect_identical(match(INITIAL_DESTROYERS, destroy_body), c(2L, 3L))
  expect_identical(destroy_body[[5L]], quote(.shiny.destroyers[["obs_3"]] <- observe("bar")))
  expect_identical(match(TERMINAL_DESTROYERS, destroy_body), 6L)
})

test_that("addModuleDestroyers makes moduleServer include destroyable observers", {
  basic_module <- quote(
    moduleServer(id, function(input, output, session) {
      x <- reactive("foo")
      observe("bar")
      invisible(NULL)
    })
  )
  destroy_module <- addModuleDestroyers(basic_module)

  basic_module_body <- quote(
    function(input, output, session) {
      x <- reactive("foo")
      observe("bar")
      invisible(NULL)
    }
  )
  destroy_module_body <- addDestroyers(basic_module_body)

  expect_identical(destroy_module$module, destroy_module_body)
})
