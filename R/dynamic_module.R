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

addDestroyers <- function(module) {
  module_body <- module[[3L]] |>
    as.list() |>
    purrr::imap(assignObserve) |>
    append(INITIAL_DESTROYERS, after = 1L)

  module[[3L]] <- as.call(module_body)
  module
}

assignObserve <- function(fn_call, idx) {
  if (isObserver(fn_call)) {
    assign_val <- str2lang(paste0(".shiny.destroyers[[\"obs_", idx, "\"]]"))
    rlang::call2("<-", assign_val, fn_call)
  } else {
    fn_call
  }
}

isSpecifiedFunction <- function(fn_call, fns) {
  if (!is.language(fn_call) || is.symbol(fn_call)) return(FALSE)

  fn_name <- rlang::call_name(fn_call)
  fn_name %in% fns || (fn_name %in% ASSIGN_FNS && rlang::call_name(fn_call[[3L]]) %in% fns)
}

isModuleServerCall <- function(fn_call) isSpecifiedFunction(fn_call, MODULE_FNS)
isObserver <- function(fn_call) isSpecifiedFunction(fn_call, OBSERVE_FNS)

MODULE_FNS <- "moduleServer"
OBSERVE_FNS <- c("observe", "observeEvent")
ASSIGN_FNS <- c("<-", "<<-", "assign")

INITIAL_DESTROYERS <- list(
  quote(if (!".shiny.destroy" %in% names(session$userData)) { session$userData$.shiny.destroy <- list() }),
  quote(.shiny.destroyers <- session$userData$.shiny.destroy[[session$ns(NULL)]] <- list())
)
