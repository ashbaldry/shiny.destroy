#### UI ####
test_that("UI enabler errors when non-shiny tag is the returned value", {
  testModuleUI <- function(id) iris
  testDestroyModuleUI <- makeModuleUIDestroyable(testModuleUI)

  expect_error(
    testDestroyModuleUI("test"),
    "Module UI must either be a shiny.tag or a shiny.tag.list"
  )
})

test_that("shiny-destroy attribute added to shiny tag module", {
  testModuleUI <- function(id) div(id = id, span(), div())
  testDestroyModuleUI <- makeModuleUIDestroyable(testModuleUI)

  destroy_ui <- testDestroyModuleUI("test")
  expect_named(destroy_ui$attribs, c("id", "shiny-destroy"))
  expect_identical(destroy_ui$attribs[["shiny-destroy"]], "test")

  destroy_ui_clean <- htmltools::tagQuery(destroy_ui)$removeAttrs("shiny-destroy")$allTags()
  test_ui <- testModuleUI("test")
  expect_identical(destroy_ui_clean, test_ui)
})

test_that("HTML wrapper added to shiny tag list module", {
  testModuleUI <- function(id) tagList(span(), div())
  testDestroyModuleUI <- makeModuleUIDestroyable(testModuleUI, wrapper = a)

  destroy_ui <- testDestroyModuleUI("test")
  expect_identical(destroy_ui$name, "a")
  expect_named(destroy_ui$attribs, "shiny-destroy")
  expect_identical(destroy_ui$attribs[["shiny-destroy"]], "test")

  test_ui <- testModuleUI("test")
  expect_identical(destroy_ui$children[[1L]], test_ui)
})

#### Server ####
test_that("Server enabler errors when the module_fn is not a function call", {
  expect_error(
    makeModuleServerDestroyable(mean(1:10)),
    "`module_fn` is not a function"
  )
})

test_that("Server enabler errors when the module_fn does not contain a moduleServer call", {
  basicFunction <- function(id) {
    paste(id, "hello")
  }

  expect_error(
    makeModuleServerDestroyable(basicFunction),
    "There must be a call to shiny::moduleServer in the `module_fn` function"
  )
})

test_that("Observers are assigned to userData when server function passed to make destroyable", {
  basicModuleServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveVal(0L)
      observeEvent(input$click, rv(rv() + 1L))
      rv
    })
  }

  destroyableModuleServer <- makeModuleServerDestroyable(basicModuleServer)

  expect_type(destroyableModuleServer, "closure")
  expect_match(as.character(body(destroyableModuleServer)), ".shiny.destroy", all = FALSE)
})

test_that("bindEvent observers are assigned to the .shiny.destroy list", {
  basicModuleServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveVal(0L)
      observe(rv(rv() + 1L)) |> bindEvent(input$click)
      rv
    })
  }

  destroyableModuleServer <- makeModuleServerDestroyable(basicModuleServer)

  expect_type(destroyableModuleServer, "closure")
  expect_match(as.character(body(destroyableModuleServer)), ".shiny.destroyers\\[\\[\"obs_\\d\"\\]\\]", all = FALSE)
})
