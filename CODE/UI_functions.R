# UI functions

############# NEXT: modify ui_yeah to present model choices (see scratch.R)

# make simple if else logic to run simple outcome
# if (interactive()) {
#   if (ui_yeah("Shall we delete the ZIP file ({ui_path(zipfile, base_path)})?")) {
#     ui_done("Deleting {ui_path(zipfile, base_path)}")
#     file_delete(zipfile)
#   }

library(glue)
library(cli)

# Block styles ------------------------------------------------------------

#' @rdname ui
#' @export
ui_line <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  cat_line(x)
}

#' @rdname ui
#' @export
ui_todo <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  cat_bullet(x, crayon::red(clisymbols::symbol$bullet))
}

#' @rdname ui
#' @export
ui_done <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  cat_bullet(x, crayon::red(crayon::green(clisymbols::symbol$tick)))
}

#' @param copy If `TRUE`, the session is interactive, and the clipr package
#'   is installed, will copy the code block to the clipboard.
#' @rdname ui
#' @export
ui_code_block <- function(x, copy = interactive(), .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  
  block <- indent(x, "  ")
  block <- crayon::make_style("darkgrey")(block)
  cat_line(block)
  
  if (copy && clipr::clipr_available()) {
    x <- crayon::strip_style(x)
    clipr::write_clip(x)
    cat_line("  [Copied to clipboard]")
  }
}

# Conditions --------------------------------------------------------------

#' @rdname ui
#' @export
ui_stop <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  
  cnd <- structure(
    class = c("usethis_error", "error", "condition"),
    list(message = x)
  )
  
  stop(cnd)
}

#' @rdname ui
#' @export
ui_warn <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  
  warning(x, call. = FALSE, immediate. = TRUE)
}

# Questions ---------------------------------------------------------------

#' @rdname ui
#' @export
ui_yeah <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  
  if (!interactive()) {
    ui_stop(c(
      "User input required, but session is not interactive.",
      "Query: {x}"
    ))
  }
  
  ayes <- c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely")
  nays <- c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not")
  
  qs <- c(sample(ayes, 1), sample(nays, 2))
  ord <- sample(length(qs))
  
  cat_line(x)
  out <- utils::menu(qs[ord])
  out != 0L && (ord == 1)[[out]]
}

#' @rdname ui
#' @export
ui_nope <- function(x, .envir = parent.frame()) {
  !ui_yeah(x, .envir = .envir)
}

# Inline styles -----------------------------------------------------------

#' @rdname ui
#' @export
ui_field <- function(x) {
  x <- crayon::green(x)
  x <- glue_collapse(x, sep = ", ")
  x
}

#' @rdname ui
#' @export
ui_value <- function(x) {
  if (is.character(x)) {
    x <- encodeString(x, quote = "'")
  }
  x <- crayon::blue(x)
  x <- glue_collapse(x, sep = ", ")
  x
}

#' @rdname ui
#' @export
#' @param base If specified, paths will be displayed relative to this path.
ui_path <- function(x, base = NULL) {
  if (is.null(base)) {
    x <- proj_rel_path(x)
  } else {
    x <- path_rel(x, base)
  }
  
  x <- paste0(x, ifelse(is_dir(x), "/", ""))
  x <- ui_value(x)
  x
}

#' @rdname ui
#' @export
ui_code <- function(x) {
  x <- encodeString(x, quote = "`")
  x <- crayon::make_style("darkgrey")(x)
  x <- glue_collapse(x, sep = ", ")
  x
}
