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
destroyableModuleServer <- function(id, module, session = getDefaultReactiveDomain()) {
  module <- addDestroyers(id, module, session)
  shiny::moduleServer(id, module, session)
}

addDestroyers <- function(id, module, session = getDefaultReactiveDomain()) {
  id <- session$ns(id)

  module_body <- rlang::fn_body(module)
  module_body <- as.list(module_body)
  module_body <- purrr::imap(module_body, assignObserve, id = id)

  module_body <- append(module_body, createCheckDestroyInit(id), 1L)

  rlang::fn_body(module) <- as.call(module_body)
  invisible(module)
}

assignObserve <- function(fn_call, idx, id) {
  if (isObserver(fn_call)) {
    assign_val <- str2lang(paste0("session$userData$.shiny.destroy[[\"", id, "\"]][[\"obs_", idx, "\"]]"))
    rlang::call2("<-", assign_val, fn_call)
  } else {
    fn_call
  }
}

isObserver <- function(fn_call) {
  if (!is.language(fn_call) || is.symbol(fn_call)) return(FALSE)

  rlang::call_name(fn_call) %in% OBSERVE_FNS ||
    (rlang::call_name(fn_call) %in% ASSIGN_FNS && rlang::call_name(fn_call[[3L]]) %in% OBSERVE_FNS)
}

OBSERVE_FNS <- c("observe", "observeEvent")
ASSIGN_FNS <- c("<-", "<<-")

createCheckDestroyInit <- function(id) {
  list(
    str2lang("if (!\".shiny.destroy\" %in% names(session$userData)) { session$userData$.shiny.destroy <- list() }"),
    str2lang(paste0("session$userData$.shiny.destroy[[\"", id, "\"]] <- list()"))
  )
}
