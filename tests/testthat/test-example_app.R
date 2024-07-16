test_that("01_boxes application works as expected", {
  example_dir <- system.file("examples-shiny", "01_boxes", package = "shiny.destroy")
  app <- shinytest2::AppDriver$new(example_dir, name = "example_app")
  on.exit(app$stop())

  expect_equal(app$get_value(input = "create"), 0L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "n_modules"), "0")

  app$click(input = "create")
  expect_equal(app$get_value(input = "create"), 1L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "n_modules"), "1")
  expect_equal(app$get_value(input = "card_1-destroy"), 0L, ignore_attr = TRUE)
  expect_match(
    app$get_value(output = "inputs"),
    "create: 1.*card_1-destroy: 0"
  )

  app$click(input = "create")
  expect_equal(app$get_value(input = "create"), 2L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "n_modules"), "2")
  expect_equal(app$get_value(input = "card_1-destroy"), 0L, ignore_attr = TRUE)
  expect_match(
    app$get_value(output = "inputs"),
    "create: 2.*card_1-destroy: 0.*card_2-destroy: 0"
  )

  app$click(input = "card_1-destroy")
  expect_equal(app$get_value(input = "create"), 2L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "n_modules"), "1")
  expect_null(app$get_value(input = "card_1-destroy"))
  expect_match(app$get_value(output = "inputs"), "create: 2\ncard_2")

  app$click(input = "create")
  expect_equal(app$get_value(input = "create"), 3L, ignore_attr = TRUE)
  expect_identical(app$get_value(output = "n_modules"), "2")
  expect_match(
    app$get_value(output = "inputs"),
    "create: 2.*card_2-destroy: 0.*card_3-destroy: 0"
  )
})
