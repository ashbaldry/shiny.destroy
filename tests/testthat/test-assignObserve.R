test_that("Able to assign observe to shiny destroyers list", {
  obs_call <- str2lang("observe(print(1L))")
  assigned_obs_call <- assignObserve(obs_call, idx = 1L)

  expect_length(assigned_obs_call, 3L)
  expect_identical(assigned_obs_call[[2L]], str2lang(".shiny.destroyers[[\"obs_1\"]]"))
  expect_identical(assigned_obs_call[[3L]], obs_call)
})

test_that("Able to assign observeEvent to shiny destroyers list", {
  obs_call <- str2lang("observeEvent(input$click, print(1L))")
  assigned_obs_call <- assignObserve(obs_call, idx = 1L)

  expect_length(assigned_obs_call, 3L)
  expect_identical(assigned_obs_call[[2L]], str2lang(".shiny.destroyers[[\"obs_1\"]]"))
  expect_identical(assigned_obs_call[[3L]], obs_call)
})

test_that("Ignores non-observe calls", {
  react_call <- str2lang("x <- reactive(input$click)")
  assigned_react_call <- assignObserve(react_call, idx = 1L)

  expect_identical(assigned_react_call, react_call)
})
