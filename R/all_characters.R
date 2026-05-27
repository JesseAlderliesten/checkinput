#' Check that `x` is character
#'
#' Check that `x` is a character vector of the correct length with only allowed
#' values.
#'
#' @inheritParams is_logical
#' @param allow_empty `TRUE` or `FALSE`: allow empty strings (`""`) in `x`?
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a character vector of the
#' correct length with only allowed values.
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' The vignettes *Design choices regarding function input*:
#' `vignette("design_choices", package = "checkinput")` and
#' *Type coercion in vectors*:
#' `vignette("type_coercion", package = "checkinput")`.
#'
#' @examples
#' is_character("a") # TRUE
#' is_character(c("a", "b")) # FALSE: incorrect length
#' all_characters(c("a", "b")) # TRUE
#' is_character(1) # FALSE: incorrect type
#' is_character(NA_character_) # FALSE: default 'allow_NA' is FALSE
#' is_character(NA_character_, allow_NA = TRUE) # TRUE
#' is_character(NA, allow_NA = TRUE) # FALSE: incorrect type
#'
#' @export
all_characters <- function(x, allow_empty = FALSE, allow_zero_length = FALSE,
                           allow_NA = FALSE) {
  stopifnot(is_logical(allow_empty), is_logical(allow_zero_length), is_logical(allow_NA))
  is.character(x) && is.atomic(x) && is.null(dim(x)) &&
    (allow_empty || all(nzchar(x, keepNA = FALSE))) &&
    (allow_zero_length || length(x) > 0) &&
    (allow_NA || !anyNA(x))
}

#' @rdname all_characters
#' @export
is_character <- function(x, allow_empty = FALSE, allow_zero_length = FALSE,
                         allow_NA = FALSE) {
  # Argument checking is deferred to all_characters().
  length(x) < 2L &&
    all_characters(x, allow_empty = allow_empty, allow_zero_length = allow_zero_length,
                   allow_NA = allow_NA)
}
