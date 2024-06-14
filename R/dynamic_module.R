#' Create Dynamic Module
#'
#' @import shiny
#' @rdname destroyableModule
#' @export
destroyableModuleUI <- function(id, ui, wrapper = div) {
  if (inherits(ui, "shiny.tag") && is.null(ui$attribs$id)) {
    ui$attribs$id <- paste0(id, "_destroy_container")
    ui
  } else if (inherits(ui, c("shiny.tag.list", "shiny.tag"))) {
    wrapper(id = paste0(id, "_destroy_container"), ui)
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
  module_args <- module |> rlang::call_match(fn = moduleServer) |> rlang::call_args()

  args_w_destroyers <- purrr::modify_if(
    module_args,
    "module",
    addDestroyers
  )

  module_w_destroyers <- rlang::call2(rlang::call_name(module), !!!args_w_destroyers)
  invisible(module_w_destroyers)
}

addDestroyers <- function(module) {
  module_body <- module |> rlang::() |> as.list()

}

assignObserve <- function(fn_call, idx) {
  if (isObserver(fn_call)) {
    assign_val <- str2lang(paste0("session$userData$.shiny.destroy[[session$ns(NULL)]][[\"obs_", idx, "\"]]"))
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
ASSIGN_FNS <- c("<-", "<<-")

createCheckDestroyInit <- function() {
  list(
    str2lang("if (!\".shiny.destroy\" %in% names(session$userData)) { session$userData$.shiny.destroy <- list() }"),
    str2lang(paste0("session$userData$.shiny.destroy[[session$ns(NULL)]] <- list()"))
  )
}
