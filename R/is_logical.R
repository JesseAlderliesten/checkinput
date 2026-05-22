#' Check that `x` is logical
#'
#' Check that `x` is a length-one logical vector with only allowed values.
#'
#' @param x object to test.
#' @param allow_zero `TRUE` or `FALSE`: allow zero-length `x` of the correct type?
#' @param allow_NA `TRUE` or `FALSE`: allow `NA`s of the correct type in `x`?
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a length-one logical vector
#' only containing allowed values.
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' The vignette *Design choices regarding function input*:
#' `vignette("design_choices", package = "checkinput")`.
#'
#' @examples
#' is_logical(TRUE) # TRUE
#' is_logical(c(TRUE, TRUE)) # FALSE: incorrect length
#' is_logical(1) # FALSE: incorrect type
#' is_logical(NA) # FALSE: default 'allow_NA' is FALSE
#' is_logical(NA, allow_NA = TRUE) # TRUE
#' is_logical(NA_character_, allow_NA = TRUE) # FALSE: incorrect type
#'
#' @export
is_logical <- function(x, allow_zero = FALSE, allow_NA = FALSE) {
  stopifnot(is.logical(allow_zero), length(allow_zero) == 1L, !is.na(allow_zero),
            is.logical(allow_NA), length(allow_NA) == 1L, !is.na(allow_NA))
  is.logical(x) && is.atomic(x) && is.null(dim(x)) &&
    (length(x) == 1L || (allow_zero && length(x) == 0L)) &&
    (allow_NA || !anyNA(x))
}
