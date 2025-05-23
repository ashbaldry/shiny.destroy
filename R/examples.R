#' Run `shiny.destroy` example
#'
#' @description
#' To see how the `shiny.destroy` works, examples are provided within
#' the package.
#'
#' @inheritParams shiny::runExample
#' @param ... Additional parameters sent to `shiny::runExample`
#'
#' @return
#' The shiny application displayed in the specified location.
#'
#' @details
#' The following examples are available:
#'
#' \describe{
#' \item{01_boxes}{A simple application where the "create" button will
#' load a simple box with a "destroy" button. This highlights the full
#' removal of the module when the button is pressed.}
#' \item{02_sleep}{An application that has 2 side by side modules, one
#' using \{shiny\} to remove the UI and the other using \{shiny.destroy\}
#' to fully remove the boxes to display the incremental time gain from
#' removing the long-running observers.}
#' }
#'
#' @examplesIf interactive()
#' runDestroyExample("01_boxes")
#'
#' @export
runDestroyExample <- function(example = NA, ...) {
  shiny::runExample(example = example, ..., package = "shiny.destroy")
}
