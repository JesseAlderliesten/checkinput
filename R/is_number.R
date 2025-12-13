#' Check if `x` is numeric
#'
#' Checks if `x` is a numeric vector of the correct length with numbers of the
#' correct sign.
#'
#' @inheritParams all_characters
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a numeric vector of the
#' correct length with numbers of the correct sign.
#'
#' @export
#'
#' @note `NA_real_` and `NA_integer_` are numeric, whereas `NA_complex_` is not.
#'
#' @details The correct length of `x` is larger than zero for `all_numbers()`
#' and one for `is_number()`.
#'
#' @seealso The vignette about type conversion:
#' `vignette("Type_Coercion", package = "checkinput")`.
#'
#' @family collections of checks on type and length
#'
#' @examples
#' is_number(1) # TRUE
#' is_number(c(1, 2)) # FALSE: incorrect length
#' all_numbers(c(1, 2)) # TRUE
#' is_number("a") # FALSE: incorrect type
#' is_number(NA_real_) # TRUE: `allow_NA` has not yet been implemented
#' is_number(NA_character_) # FALSE: incorrect type
#' is_nonnegative(3) # TRUE
#' all_nonnegative(c(3, 0)) # TRUE
#' is_positive(3) # TRUE
#' is_positive(0) # FALSE
#' all_positive(c(3, 0)) # FALSE
is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.null(dim(x))
}
