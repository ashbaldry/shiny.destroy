#' Destroy Shiny Module
#'
#' @description
#' Given the namespace
#'
#' @param id The module namespace ID
#' @param session
#'
#' @export
destroyModule <- function(id, session = getDefaultReactiveDomain()) {
  destroyModuleUI(id, session = session)
  destroyModuleServer(id, session = session)

  invisible(NULL)
}

destroyModuleUI <- function(id, session = getDefaultReactiveDomain()) {
  ns_id <- session$ns(id)
  input <- session$input

  removeUI(selector = paste0("[shiny-destroy=\"", id, "\"]"), immediate = TRUE)
  purrr::walk(names(input), \(x) if (startsWith(x, ns_id)) destroyInput(input, x))

  invisible(NULL)
}

destroyInput <- function(input, id) {
  .subset2(input, "impl")$.values$remove(id)
}

destroyModuleServer <- function(id, session = getDefaultReactiveDomain()) {
  observers <- session$userData$.shiny.destroy
  ns_id <- session$ns(id)
  contains_id <- startsWith(names(observers), ns_id)

  observers <- unlist(observers[contains_id], recursive = FALSE)
  purrr::walk(observers, \(x) x$destroy())

  session$userData$.shiny.destroy <- session$userData$.shiny.destroy[!contains_id]

  invisible(NULL)
}
