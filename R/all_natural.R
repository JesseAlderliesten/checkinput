#' Check if `x` is natural
#'
#' Check if `x` is a vector with natural numbers (i.e., positive or non-negative
#' integers), allowing for small numerical errors.
#'
#' @inheritParams is_logical
#' @param strict Exclude zero from the natural numbers?
#' @param tol Positive number indicating the maximum difference in value from
#' natural numbers.
#'
#' @details
#' The positive integers (`1`, `2`, `3`, etc.) are natural numbers. Zero is
#' considered a natural number if argument `strict` is `FALSE`. `Inf` is *never*
#' considered to be a natural number in this implementation.
#'
#' `NA_integer_` and `NA_real_` are allowed if `allow_NA` is `TRUE`. Other [NA]s
#' and [NaN] are never allowed.
#'
#' `all_natural()` allows for small numeric differences from the intended
#' natural number, e.g., because of rounding or representation error. That is
#' *not* the case for `x == round(x)` which tests exact equality. For background
#' see e.g.,
#' [\R FAQ 7.31](
#' https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a vector with only natural
#' numbers.
#'
#' @note
#' The code of `all_natural()` is partly based on the example `is.wholenumber()`
#' in [is.integer()].
#'
#' @section Programming note:
#' [is.integer()] does *not* check that `x` is a natural number (or even if `x`
#' is a whole number) but rather that `x` is of [type][typeof()] integer (see
#' the `Note` in [is.integer()]).
#'
#' Argument `strict` should *not* be renamed to `allow_zero` because in other
#' functions, e.g., [is_logical()], `allow_zero` is used to allow for
#' zero-*length* value of `x`.
#'
#' @seealso [all.equal()] to check equality more generally. [identical()] to
#' check exact equality. The vignette about type conversion:
#' `vignette("Type_Coercion", package = "checkinput")`.
#' @family collections of checks on type and length
#'
#' @examples
#' all_natural(x = c(3, 5 + 1e-10)) # TRUE
#' all_natural(x = c(1e-10, 3, 5)) # FALSE
#' all_natural(x = c(1e-10, 3, 5), strict = FALSE) # TRUE
#' all_natural(x = c(-1e-10, 3, 5), strict = FALSE) # FALSE
#' all_natural(x = c(3, 5, Inf)) # FALSE
#' all_natural(x = c(3, 5, Inf), strict = FALSE) # FALSE
#'
#' # Allowing for small numerical errors is important
#' x <- sqrt(2)^2
#' all_natural(x = x) # TRUE
#' x == 2 # FALSE!
#' x - 2 # about 4.44e-16
#'
#' @export
all_natural <- function(x, strict = TRUE, allow_NA = FALSE,
                        tol = .Machine$double.eps^0.5) {
  stopifnot(is.null(dim(x)), is_logical(strict), is_logical(allow_NA),
            is_positive(tol))

  if(!is.numeric(x)) {
    return(FALSE)
  }

  if(!allow_NA && anyNA(x)) {
    return(FALSE)
  }

  if(any(is.nan(x) | is.infinite(x) | x < 0 | (strict & x < 0.5), na.rm = TRUE)) {
    return(FALSE)
  }

  all(abs(x - round(x)) < tol, na.rm = TRUE)
}
