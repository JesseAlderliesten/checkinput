#' Check that x is Nearly Equal to Natural Numbers
#'
#' Check element-wise near-equality to natural [numbers][is.numeric()] while
#' allowing for small numeric errors.
#'
#' @inheritParams is_number x allow_zerolength
#' @param strict exclude zero from the natural numbers?
#' @param allow_NA `TRUE` or `FALSE`: allow [`NA`] of the correct type in `x`?
#' [`NaN`] is never allowed in `x`.
#' @param tol a small [positive][is_positive()] number. Numbers that differ less
#' than `tol` are considered equal.
#' @param all `TRUE` or `FALSE`: use `all_natural()` instead of `is_natural()`?
#'
#' @details
#' Natural numbers are the positive integers (`1`, `2`, `3`, etc.).
#' `is_natural()` and `all_natural()` allow for small numeric errors when
#' comparing numbers to the natural numbers. Such numeric errors can arise
#' because of rounding or representation error. As the `Note` at `help("==")`
#' warns, `x == round(x)` does **not** allow for such errors but checks exact
#' equality.
#'
#' Zero is considered a natural number if argument `strict` is `FALSE`, but even
#' then small negative numbers are **not** considered natural numbers.
#'
#' `integer(0)` and `numeric(0)` are considered natural numbers if argument
#' `allow_zerolength` is `TRUE`, and `NA_integer_` and `NA_real_` are considered
#' natural numbers if `allow_NA` is `TRUE` (even then `NA_complex_` is **not**
#' considered a natural number because it has `mode` `complex` instead of
#' `numeric`).
#'
#' [`NULL`], [`NaN`], negative numbers, [`Inf`][is.finite], and numbers that are
#' [too large][.Machine] to be represented as [integers][integer] (about 2.1 *
#' 10^9) are **not** considered natural numbers.
#'
#' @returns
#' For `is_natural()` and `all_natural()`: `TRUE` or `FALSE` indicating if `x`
#' is a vector of the appropriate length with only natural numbers. For
#' `make_natural()`: `x`, [rounded][round()] to a whole number and coerced to
#' [integer] type.
#'
#' @section Usage:
#' `make_natural(x, all = FALSE)` and `make_natural(x, all = TRUE)` throw an
#' error if `x` is not natural according to `is_natural(x)` or `all_natural(x)`,
#' respectively. Therefore, their result can be assigned to `x` without the need
#' to use [stopifnot()].
#'
#' Alternatively, use `is_natural(x)` or `all_natural(x)` inside [stopifnot()],
#' followed by converting the rounded value of `x` to `integer` and assigning it
#' to `x`:
#'
#' ```
#' stopifnot(is_natural(x))
#' x <- as.integer(round(x))
#' ```
#'
#' @section Programming Notes:
#' The check for near-equality to the natural numbers in the last line of code
#' of `all_natural()` is partly based on function `is.wholenumber()` from the
#' `Examples` of [is.integer()].
#'
#' [is.integer()] does **not** check that `x` is a natural number (nor that `x`
#' is a whole number) but rather that `x` is of [type][typeof()] integer, see
#' the section 'Note' in `help("is.integer")`.
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' [`progutils::are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.html)
#' to check for element-wise near-equality of numbers;
#' [all.equal()] to check more generally for near-equality; [identical()] to
#' check for exact equality and [`Comparison`] to do so using binary operators;
#' [match()] and
#' [`progutils::not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.html)
#' to compare character vectors;
#' [\R FAQ 7.31](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
#' for background on numerical equality and its pitfalls.
#'
#' @examples
#' is_natural(x = 5 + 1e-10) # TRUE
#' # Zero is not considered a natural number if 'strict' is TRUE:
#' is_natural(x = 1e-10, strict = TRUE) # FALSE
#' try(make_natural(x = 1e-10, strict = TRUE)) # Error
#' is_natural(x = 1e-10, strict = FALSE) # TRUE
#' make_natural(x = 1e-10, strict = FALSE) # 0
#' is_natural(x = -1e-10, strict = FALSE) # FALSE: wrong sign
#' is_natural(x = Inf, strict = FALSE) # FALSE
#' is_natural(x = "a") # FALSE
#' is_natural(x = 1:2) # FALSE: wrong length
#'
#' # Allowing for small numeric errors is important
#' x <- sqrt(2)^2
#' is_natural(x = x) # TRUE
#' x == 2 # FALSE!
#' x - 2 # about 4.44e-16
#'
#' all_natural(x = c(3, 5 + 1e-10)) # TRUE
#' try(make_natural(x = c(3, 5 + 1e-10))) # c(3L, 5L)
#' # Zero is not considered a natural number if 'strict' is TRUE:
#' all_natural(x = c(1e-10, 3, 5), strict = TRUE) # FALSE
#' all_natural(x = c(1e-10, 3, 5), strict = FALSE) # TRUE
#' # FALSE because of wrong sign:
#' all_natural(x = c(-1e-10, 3, 5), strict = FALSE)
#' all_natural(x = c(3, 5, Inf), strict = FALSE) # FALSE
#' all_natural(x = "a") # FALSE
#' all_natural(x = 1:2) # TRUE
#'
#' # Illustrate the need to follow the use of is_natural(x) or
#' # all_natural(x) by assigning the rounded value to the argument
#' toy_fun_erroneous <- function(x) {
#'   stopifnot(is_natural(x))
#'   seq_len(x)
#' }
#'
#' toy_fun_correct <- function(x) {
#'   stopifnot(is_natural(x))
#'   x <- round(x)
#'   seq_len(x)
#' }
#'
#' toy_fun_safe <- function(x, all = TRUE) {
#'   x <- make_natural(x, all = all)
#'   seq_len(x)
#' }
#'
#' toy_fun_erroneous(x = 5 - 1e-8) # 1:4
#' toy_fun_correct(x = 5 - 1e-8) # 1:5
#' toy_fun_safe(x = 5 - 1e-8) # 1:5
#'
#' # Error: is_natural(x) is not TRUE
#' try(toy_fun_erroneous(x = 5.1))
#' try(toy_fun_correct(x = 5.1))
#' try(toy_fun_safe(x = 5.1, all = FALSE))
#'
#' # Error: all_natural(x) is not TRUE
#' try(toy_fun_safe(x = 5.1, all = TRUE))
#'
#' @export
is_natural <- function(x,
                       strict = TRUE,
                       allow_zerolength = FALSE,
                       allow_NA = FALSE,
                       tol = .Machine$double.eps^0.5) {
  all_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
              allow_NA = allow_NA, tol = tol) &&
    length(x) <= 1L
}

#' @rdname is_natural
#' @export
all_natural <- function(x,
                        strict = TRUE,
                        allow_zerolength = FALSE,
                        allow_NA = FALSE,
                        tol = .Machine$double.eps^0.5) {
  # Arguments 'allow_zerolength' and 'allow_NA' are checked in 'all_numbers()'.
  stopifnot(is_logical(strict), is_positive(tol), tol < 0.5)

  # 'NaN' is never allowed.
  if(!all_numbers(x = x, allow_zerolength = allow_zerolength,
                  allow_NA = allow_NA, allow_NaN = FALSE)) {
    return(FALSE)
  }

  # NaN and, if 'allow_NA' is FALSE, NA, have been catched above
  if(any(x > .Machine$integer.max | is.infinite(x) | x < 0 | (strict & x < 0.5),
         na.rm = TRUE)) {
    return(FALSE)
  }

  all(abs(x - round(x)) < tol, na.rm = TRUE)
}

#' @rdname is_natural
#' @export
make_natural <- function(x,
                         strict = TRUE,
                         allow_zerolength = FALSE,
                         allow_NA = FALSE,
                         all = FALSE,
                         tol = .Machine$double.eps^0.5) {
  name_x <- deparse1(substitute(x))
  stopifnot(is_logical(all))

  if(all) {
    if(!all_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
                    allow_NA = allow_NA, tol = tol)) {
      stop("checkinput::all_natural(", name_x, ") is not TRUE")
    }
  } else {
    if(!is_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
                   allow_NA = allow_NA, tol = tol)) {
      stop("checkinput::is_natural(", name_x, ") is not TRUE")
    }
  }

  as.integer(round(x))
}
