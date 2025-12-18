#' Check if `x` is logical
#'
#' Check if `x` is a length-one logical vector with only allowed logical values.
#'
#' @param x object to test.
#' @param allow_zero `TRUE` or `FALSE`: allow zero-length values in `x`?
#' @param allow_NA `TRUE` or `FALSE`: allow NAs of the correct type in `x`?
#'
#' @details The correct length of `x` is one if argument `allow_zero` is `FALSE`
#' and zero or one if argument `allow_zero` is `TRUE`. Argument `allow_zero`
#' only has an effect if `logical(0)` is the only value in `x`, because
#' `logical(0)` is discarded when it is put in a vector together with other
#' values.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a length-one logical vector
#' and contains only allowed logical values.
#'
#' @section Programming note: This implementation differs from the legacy-code:
#' `NA` is not allowed for arguments `allow_empty`, `allow_zero`, and
#' `allow_NA`, such that the return will never be `NA`.
#'
#' @note `is_logical()` is mainly used for argument checking and therefore by
#' default return `FALSE` for zero-length logical strings and `NA`. In contrast,
#' [is.logical()] returns `TRUE` for these inputs, which can be achieved with
#' `is_logical()` by setting arguments `allow_zero` or `allow_NA` to `TRUE`,
#' respectively.
#'
#' @seealso The vignette about type conversion:
#' `vignette("Type_Coercion", package = "checkinput")`.
#' @family collections of checks on type and length
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
  is.logical(x) && is.null(dim(x)) &&
    (length(x) == 1L || (allow_zero && length(x) == 0L)) &&
    (allow_NA || !anyNA(x))
}
