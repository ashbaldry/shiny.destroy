#' Remove Output from Shiny Session
#'
#' The removal of the named output in a shiny session.
#'
#' @param id Output value name
#' @param session The Shiny session to remove the output from
#'
#' @noRd
destroyOutput <- function(id, session = getDefaultReactiveDomain()) {
  session$defineOutput(id, NULL, NULL)
  session$.__enclos_env__$private$.outputs[[id]] <- NULL
  session$.__enclos_env__$private$.outputOptions[[id]] <- NULL
}
