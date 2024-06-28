#' Create Destroyable Module
#'
#' @description
#' A short description...
#'
#' @param module_fn The server-side part of the module
#' @param wrapper If the module is a `shiny.tag.list`, then an HTML tag
#' will be wrapped by an HTML tag so that a shiny.destroy attribute
#' can be attached
#'
#' @examples
#' library(shiny)
#'
#' # UI
#' basicModuleUI <- function(id) {
#'   ns <- NS(id)
#'   actionButton("click")
#' }
#'
#' makeModuleUIDestroyable(basicModuleUI)
#'
#' # Server-side
#' basicMoudleServer <- function(id) {
#'   moduleServer(id, function(input, output, session) {
#'     rv <- reactiveVal()
#'     observeEvent(input$click, rv(input$click))
#'   })
#' }
#'
#' makeModuleServerDestroyable(basicMoudleServer)
#'
#' # Shiny Application
#' ui <- fluidPage(
#'   destroyableModuleUI(id = "test"),
#'   actionButton("destroy", "Destroy module"),
#'   textOutput("reactive_value")
#' )
#'
#' server <- function(input, output, session) {
#'   top_rv <- reactiveVal()
#'
#'   reactive_value <- destroyableModuleServer("test")
#'   observeEvent(reactive_value(), top_rv(reactive_value()))
#'
#'   output$reactive_value <- renderText(top_rv())
#'
#'   observeEvent(input$destroy, destroyModule("test"))
#' }
#'
#' @rdname destroyableModule
#' @export
makeModuleUIDestroyable <- function(module_fn, wrapper = shiny::div) {
  stopifnot(
    "`module_fn` is not a function" = is.function(module_fn),
    "`id` argument must be present in function arguments" = "id" %in% rlang::fn_fmls_names(module_fn)
  )

  function(id, ...) {
    ui <- module_fn(id, ...)
    if (inherits(ui, "shiny.tag")) {
      ui$attribs[["shiny-destroy"]] <- id
      ui
    } else if (inherits(ui, "shiny.tag.list")) {
      wrapper("shiny-destroy" = id, ui)
    }  else {
      stop("Module UI must either be a shiny.tag or a shiny.tag.list")
    }
  }
}

#' @rdname destroyableModule
#' @export
makeModuleServerDestroyable <- function(module_fn) {
  stopifnot("`module_fn` is not a function" = is.function(module_fn))

  module_fn_body <- module_fn |> rlang::fn_body() |> as.list()
  if (isFALSE(any(purrr::map_lgl(module_fn_body, isModuleServerCall)))) {
    stop("There must be a call to shiny::moduleServer in the `module_fn` function", call. = FALSE)
  }

  module_fn_w_destroyers <- purrr::modify_if(
    module_fn_body,
    isModuleServerCall,
    addModuleDestroyers
  )

  rlang::fn_body(module_fn) <- as.call(module_fn_w_destroyers)
  invisible(module_fn)
}

addModuleDestroyers <- function(module) {
  module_args <- module |> rlang::call_match(fn = shiny::moduleServer) |> rlang::call_args()

  args_w_destroyers <- purrr::modify_at(module_args, "module", addDestroyers)

  module_w_destroyers <- rlang::call2(rlang::call_name(module), !!!args_w_destroyers)
  invisible(module_w_destroyers)
}

#' Add shiny.destroy Code to Module
#'
#' @description
#' For a given `moduleServer` call, add the code required for `{shiny.destroy}`
#' to work. This will involve creating the observer list to the user session,
#' and adds all observers within the list.
#'
#' @param module The function call to `moduleServer`
#'
#' @return
#' An updated version of `module`, where the `{shiny.destroy}`
#' code has been added.
addDestroyers <- function(module) {
  module_body <- module[[3L]] |>
    as.list() |>
    purrr::imap(assignObserve) |>
    append(INITIAL_DESTROYERS, after = 1L)

  if (isObserver(module_body[[length(module_body)]])) {
    module_body <- append(
      module_body,
      c(TERMINAL_DESTROYERS, module_body[[length(module_body)]][[2L]]),
      length(module_body)
    )
  } else {
    module_body <- append(module_body, TERMINAL_DESTROYERS, length(module_body) - 1L)
  }

  module[[3L]] <- as.call(module_body)
  module
}

INITIAL_DESTROYERS <- list(
  quote(if (!".shiny.destroy" %in% names(session$userData)) { session$userData$.shiny.destroy <- list() }),
  quote(.shiny.destroyers <- list())
)

TERMINAL_DESTROYERS <- list(
  quote(session$userData$.shiny.destroy[[session$ns(NULL)]] <- .shiny.destroyers)
)

#' Assign Observer Output
#'
#' @description
#' For `observe` and `observeEvent`, create an assignment
#' to use
#'
#' @param fn_call A function call
#' @param idx A number that will give a unique item in the captured observer list
#'
#' @noRd
assignObserve <- function(fn_call, idx) {
  stopifnot(
    "Index must be a numeric value" = is.numeric(idx),
    "Index must be of length one" = length(idx) == 1L
  )

  if (isObserver(fn_call)) {
    assign_val <- str2lang(paste0(".shiny.destroyers[[\"obs_", idx, "\"]]"))
    rlang::call2("<-", assign_val, fn_call)
  } else {
    fn_call
  }
}

#' Check if call is for `moduleServer`
#' @noRd
isModuleServerCall <- function(fn_call) isSpecifiedFunction(fn_call, "moduleServer")

#' Check if call is for `observe` or `observeEvent`
#' @noRd
isObserver <- function(fn_call) isSpecifiedFunction(fn_call, c("observe", "observeEvent"))

#' Check if Function Call is relevant function
#'
#' @description
#' A short description...
#'
#' @param fn_call A function call
#' @param fns A character vector of functions to comapre the function call against
#'
#' @return
#' A logical value stating whether or not the function call is in the collection.
isSpecifiedFunction <- function(fn_call, fns) {
  if (!is.language(fn_call) || is.symbol(fn_call)) return(FALSE)

  fn_name <- rlang::call_name(fn_call)
  fn_name %in% fns || (fn_name %in% ASSIGN_FNS && rlang::call_name(fn_call[[3L]]) %in% fns)
}
ASSIGN_FNS <- c("<-", "<<-", "assign")
