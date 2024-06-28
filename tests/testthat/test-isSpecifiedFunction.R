test_that("Able to check call against function list", {
  expect_true(isSpecifiedFunction(str2lang("mean(1:10)"), "mean"))
  expect_false(isSpecifiedFunction(str2lang("mean(1:10)"), "sum"))
})

test_that("Able to check assigned call against function list", {
  expect_true(isSpecifiedFunction(str2lang("x <- mean(1:10)"), "mean"))
  expect_true(isSpecifiedFunction(str2lang("x <<- mean(1:10)"), "mean"))

  expect_false(isSpecifiedFunction(str2lang("y <- mean(1:10)"), "sum"))
})

test_that("Returns false if the call isn't a function", {
  expect_false(isSpecifiedFunction(str2lang("iris"), "sum"))
})

test_that("Module server correctly identified", {
  expect_true(isModuleServerCall(str2lang("moduleServer(id, function(input, output, session) {})")))
  expect_false(isModuleServerCall(str2lang("reactive(input$click)")))
})

test_that("Observers correctly identified", {
  expect_true(isObserver(str2lang("observe(print(1L))")))
  expect_false(isObserver(str2lang("reactive(input$click)")))
})
