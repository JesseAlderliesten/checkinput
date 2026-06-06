#' Check that `x` is logical
#'
#' Check that `x` is a length-one logical vector with only allowed values.
#'
#' @param x object to check.
#' @param allow_zerolength `TRUE` or `FALSE`: allow
#' [zerolength][is_zerolength()] `x` of the correct type?
#' @param allow_NA `TRUE` or `FALSE`: allow [NA]s of the correct type in `x`?
#'
#' @details
#' `is_logical()` returns `TRUE` for `x` with length one, for logical-type
#' [zero-length][is_zerolength()] `x` if `allow_zerolength` is `TRUE`, and for
#' logical-type `NA` if `allow_NA` is `TRUE`.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a length-one logical vector
#' only containing allowed values.
#'
#' @family
#' collections of checks on type and length
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
is_logical <- function(x, allow_zerolength = FALSE, allow_NA = FALSE) {
  stopifnot(is.logical(allow_zerolength), length(allow_zerolength) == 1L,
            !is.na(allow_zerolength), is.logical(allow_NA),
            length(allow_NA) == 1L, !is.na(allow_NA))

  is.logical(x) && is.atomic(x) && is.null(dim(x)) &&
    (length(x) == 1L || (allow_zerolength && length(x) == 0L)) &&
    (allow_NA || !anyNA(x))
}
