#' Check if x is a length-one logical vector.
#'
#' @param x object to test.
#' @param allow_zero logical of length 1 indicating if zero-length logical
#' strings (`logical(0)`) should be allowed, see `Details`.
#' @param allow_NA logical of length 1 indicating if `NA` should be allowed, see
#' `Details`.
#'
#' @returns A single logical value (`TRUE`, `FALSE`, or `NA`) indicating if `x`
#' is a length-one logical vector and contains only allowed logical values, see
#' `Details`.
#'
#' @export
#'
#' @details If argument `allow_zero` is `FALSE`, the correct length is 1 for
#' `is_logical()`. If `allow_zero` is `TRUE`, the zero-length logical value
#' logical(0) is allowed. `allow_zero` only has an effect if `logical(0)` is the
#' only value in `x`, because `logical(0)` is discarded when it is put in a
#' vector together with other values.
#'
#' `NA` is returned if arguments `allow_zero` or `allow_NA` are `NA` and `x` is
#' `logical(0)` or `NA`, respectively.
#'
#' @note `is_logical()` is mainly used for argument checking and therefore by
#' default return `FALSE` for zero-length logical strings and `NA`. In contrast,
#' [is.logical()] returns `TRUE` for these inputs, which can be achieved with
#' `is_logical()` by setting arguments `allow_zero` or `allow_NA` to `TRUE`,
#' respectively.
#'
#' @seealso [is_number()] [is_character()]
#'
#' @examples
#' is_logical(TRUE) # TRUE
#' is_logical(c(TRUE, TRUE)) # FALSE: incorrect length
#' is_logical(1) # FALSE: incorrect type
#' is_logical(NA) # FALSE: default 'allow_NA' is FALSE
#' is_logical(NA, allow_NA = TRUE) # TRUE
#' is_logical(NA_character_, allow_NA = TRUE) # FALSE: incorrect type
is_logical <- function(x, allow_NA = FALSE, allow_zero = FALSE) {
  stopifnot(is.logical(allow_NA), length(allow_NA) == 1L,
            is.logical(allow_zero), length(allow_zero) == 1L)
  is.logical(x) && is.null(dim(x)) &&
    (length(x) == 1L || (allow_zero && length(x) == 0L)) &&
    (allow_NA || !anyNA(x))
}
