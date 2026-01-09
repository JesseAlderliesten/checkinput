#' Check if `x` is natural
#'
#' Check if `x` is a vector with natural numbers, allowing for small numerical
#' errors.
#'
#' @inheritParams is_logical
#' @param strict Exclude zero from the natural numbers?
#' @param tol Positive number indicating the maximum difference in value from
#' natural numbers.
#'
#' @details
#' Natural numbers are the positive integers (`1`, `2`, `3`, etc.). Zero is
#' considered a natural number if argument `strict` is `FALSE`. `Inf` is *never*
#' considered to be a natural number in this implementation.
#'
#' `NA_integer_` and `NA_real_` are allowed if `allow_NA` is `TRUE`. Other [NA]s
#' and [NaN] are never allowed.
#'
#' `all_natural()` allows for small numeric differences from the intended
#' natural number, e.g., because of rounding or representation error. As the
#' `Note` at [`==`] warns, that is *not* the case for `x == round(x)` which
#' tests exact equality. For background see e.g.,
#' [\R FAQ 7.31](
#' https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f).
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a vector with only natural
#' numbers.
#'
#' @note
#' The code of `all_natural()` is partly based on the example `is.wholenumber()`
#' in [is.integer()].
#'
#' @section Programming note:
#' [is.integer()] does *not* check that `x` is a natural number (nor if `x` is a
#' whole number) but rather that `x` is of [type][typeof()] integer (see the
#' `Note` in [is.integer()]).
#'
#' Argument `strict` should *not* be renamed to `allow_zero` because in other
#' functions, e.g., [is_logical()], `allow_zero` is used to allow for
#' zero-*length* value of `x`.
#'
#' @seealso
#' `progutils::are_equal()` to check for element-wise near-equality of numbers;
#' [all.equal()] to check more generally for near-equality; [identical()] to
#' check for exact equality; [Comparison] to compare two vectors using binary
#' operators; [match()] to compare character vectors. The vignette about type
#' coercion: `vignette("Type_Coercion", package = "checkinput")`.
#' @family collections of checks on type and length
#'
#' @examples
#' all_natural(x = c(3, 5 + 1e-10)) # TRUE
#' # Zero is not considered a natural number if 'strict' is TRUE:
#' all_natural(x = c(1e-10, 3, 5), strict = TRUE) # FALSE
#' all_natural(x = c(1e-10, 3, 5), strict = FALSE) # TRUE
#' all_natural(x = c(-1e-10, 3, 5), strict = FALSE) # FALSE: wrong sign
#' all_natural(x = c(3, 5, Inf), strict = FALSE) # FALSE
#' all_natural(x = "a") # FALSE
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
