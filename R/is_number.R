#' Check if `x` is numeric
#'
#' Check if `x` is a numeric vector of the correct length with numbers of the
#' correct sign.
#'
#' @inheritParams is_logical
#'
#' @details
#' The correct length of `x` is larger than zero for `all_numbers()`
#' and one for `is_number()`.
#'
#' `all_nonnegative()` and `is_nonnegative()` return `TRUE` for `0`, whereas
#' `is_positive()` returns `FALSE` for `0`.
#'
#' `all_numbers()` and `is_number()` return `TRUE` for `Inf`, `NA_real_`,
#' `NA_integer_`, and `NaN` because their `mode` is `numeric`.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a numeric vector of the
#' correct length with numbers of the correct sign.
#'
#' @section Wishlist:
#' Add argument `allow_zero` to optionally allow for zero-length numerics, see
#' the code of [is_logical()].
#'
#' @section Programming note:
#' [is.numeric()] tests the [mode()] of `x`, which is `numeric` for
#' floating-point numbers such as 3.2 and integers such as 3L. In contrast,
#' `class(x) == "numeric"` would test the [class()] of `x` which is `numeric`
#' for floating-point numbers but `integer` for integers (see the `Note on names`
#' in [is.numeric()]).
#'
#' Adding arguments `allow_NA` and `allow_NaN` to optionally allow `NA_real_`,
#' `NA_integer_`, or `NaN` (which all have `mode` `numeric`) would solve the
#' contradiction that using `is_number()` currently returns `TRUE` for these
#' values but using them in conditional statements leads to `logical(0)` and
#' thus to an error. However, implementing those arguments is complicated
#' because `is.na(x)` returns `TRUE` for `NaN` and `is.na(x)` and `is.nan(x)`
#' return `logical(0)` for zero-length `x`, see [are_NA_numeric()].
#'
#' @seealso The vignette about type coercion:
#' `vignette("Type_Coercion", package = "checkinput")`.
#' @family collections of checks on type and length
#'
#' @examples
#' is_number(1) # TRUE
#' is_number(c(1, 2)) # FALSE: incorrect length
#' all_numbers(c(1, 2)) # TRUE
#' is_number("a") # FALSE: incorrect type
#' is_number(numeric(0)) # FALSE: incorrect length
#' is_number(NA_real_) # TRUE: 'allow_NA' has not been implemented
#' is_number(NA_character_) # FALSE: incorrect type
#' is_number(NaN) # TRUE (!)
#' is_number(Inf) # TRUE
#' is_nonnegative(3) # TRUE
#' is_nonnegative(0) # TRUE
#' all_nonnegative(c(3, 0)) # TRUE
#' is_positive(3) # TRUE
#' is_positive(0) # FALSE
#'
#' @export
is_number <- function(x) {
  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value.
  is.numeric(x) && length(x) == 1L && is.null(dim(x))
}
