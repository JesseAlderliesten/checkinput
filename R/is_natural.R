#' Check that x is Nearly Equal to Natural Numbers
#'
#' Check element-wise near-equality to natural [numbers][is.numeric()] while
#' allowing for small numeric errors.
#'
#' @inheritParams is_number x allow_zerolength allow_NaN
#' @param strict exclude zero from the natural numbers?
#' @param allow_NA `TRUE` or `FALSE`: allow [`NA`] of the correct type in `x`?
#' See the section 'Large Numbers' below on the disadvantage of using `TRUE` for
#' `allow_NA` with large numbers.
#' @param allow_Inf `TRUE` or `FALSE`: allow positive [`Inf`] in `x`? See the
#' section 'Large Numbers' below.
#' @param tol a small [positive][is_positive()] number. Numbers that differ less
#' than `tol` are considered equal.
#' @param large_as_double `TRUE` or `FALSE`: return an object of [type][typeof]
#' `double` if `x` contains values that are too large to be represented as
#' [integer]? See the section 'Large Numbers' below.
#' @param all `TRUE` or `FALSE`: use `all_natural()` instead of `is_natural()`?
#'
#' @details
#' Natural numbers are the positive integers (`1`, `2`, `3`, etc.). The
#' functions `*_natural()` allow for small numeric errors when comparing numbers
#' to the natural numbers, which is important because rounding errors and
#' representation errors during normal calculations can lead to such small
#' numeric errors. As the `Note` at `help("==")` warns, using `x == round(x)`
#' does **not** allow for such errors but checks exact equality, see the first
#' `Examples`.
#'
#' Zero is considered a natural number if argument `strict` is `FALSE`, but even
#' then small negative numbers are **not** considered natural numbers.
#'
#' Although using `TRUE` for arguments `allow_zerolength`, `allow_NA`,
#' `allow_NaN`, or `allow_Inf` might be useful during specific programming
#' checks and are provided for consistency with [is_number()], using `TRUE` for
#' those arguments is **not** useful when testing that `x` is a natural number
#' before using it as an index.
#'
#' `integer(0)` and `numeric(0)` are considered natural numbers if argument
#' `allow_zerolength` is `TRUE`.  `make_natural()` then returns `integer(0)` for
#' both.
#'
#' [`NA_integer_`][NA] and `NA_real_` are considered natural numbers if
#' `allow_NA` is `TRUE`. `make_natural()` then returns `NA_integer_` for both.
#' Even then `NA_complex_` is **not** considered a natural number because it is
#' [`complex`][is.complex()] instead of [`numeric`][is.numeric()].
#'
#' [`NaN`] (which has [`mode`] `numeric`, despite meaning 'not a number') is
#' considered a natural number if `allow_NaN` is `TRUE`. `make_natural()` then
#' returns `NA_integer_` for `NaN`.
#'
#' Positive [`Inf`] is considered a natural number if `allow_Inf` is `TRUE`.
#' `make_natural()` then returns positive `Inf` unchanged if `large_as_double`
#' is `TRUE`, and `NA_integer_` for positive `Inf` if `large_as_double` is
#' `FALSE`.
#'
#' [`NULL`] and negative numbers are **not** considered natural numbers and
#' `make_natural()` throws an [error][stop()] for them.
#'
#' See the section `Large Numbers` below on the treatment of numbers that are
#' too large to be represented as integers.
#'
#' @returns
#' `is_natural()` and `all_natural()` return `TRUE` or `FALSE` indicating if `x`
#' is a vector of the appropriate length with only natural numbers according to
#' the values of their other arguments.
#'
#' `make_natural()` throws an [error][stop()] if `x` contains values that are
#' not natural according to `is_natural()` or `all_natural()`. Otherwise,
#' `make_natural()` returns `x`, [rounded][round()] to a whole number. The
#' returned value is coerced to [`type`][typeof()] [`integer`] if
#' `large_as_double` is `FALSE` or `x` does not contain [`Inf`] nor values
#' larger than [`.Machine$integer.max`][.Machine] (about 2 * 10^9), otherwise it
#' is kept as [`type`][typeof()] [`double`], see the section 'Large Numbers'
#' below.
#'
#' @section Large Numbers:
#' Numbers larger than [`.Machine$integer.max`][.Machine] (about 2 * 10^9) are
#' too large to be represented as 32-bit [integers][is.integer()] used by \R.
#' When such large numbers are produced in [arithmetic operations][Arithmetic]
#' that should result in integers, they are coerced to [`NA_integer_`] with the
#' warning '`NAs produced by integer overflow`'.
#'
#' If argument `large_as_double` is `FALSE`, that approach is also followed by
#' `make_natural()`: then it coerces `Inf` and numbers that are too large to be
#' represented as `integer` to `NA_integer_`. If argument `large_as_double` is
#' `TRUE`, `make_natural()` returns the actual values as [`type`][typeof()]
#' [`double`][is.double()] instead of `integer`, with the warning
#' '`The return of 'make_natural()' has type 'double'`' that also indicates
#' which values are too large.
#'
#' @section Programming Notes:
#' `make_natural(x, ..., all = FALSE)` and `make_natural(x, ..., all = TRUE)`
#' throw an error if `x` is not natural according to `is_natural(x, ...)` or
#' `all_natural(x, ...)`, respectively. Therefore, their result can be assigned
#' to `x` without the need to use [stopifnot()].
#'
#' [is.integer()] does **not** check that `x` is a natural number (nor that `x`
#' is a whole number) but rather that `x` is of [type][typeof()] integer, see
#' the section 'Note' in `help("is.integer")`.
#'
#' The check for near-equality to the natural numbers in the last line of code
#' of `all_natural()` is partly based on function `is.wholenumber()` from the
#' `Examples` of [is.integer()].
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
#' help(`%/%`) on integer division;
#' [\R FAQ 7.31](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
#' for background on numerical equality and its pitfalls.
#'
#' @examples
#' # Allowing for small numeric errors is important
#' x <- sqrt(3)^2
#' x == 3        # FALSE!
#' x - 3         # about -4.44e-16
#' is_natural(x) # TRUE
#' seq_len(x)    # 1:2 (incorrect)
#' seq_len(make_natural(x)) # 1:3 (correct)
#'
#' # Normal behaviour
#' is_natural(5 + 1e-9, tol = 1e-8) # TRUE
#' is_natural(5 + 1e-7, tol = 1e-8) # FALSE
#' is_natural(1:2)  # FALSE: wrong length
#' all_natural(1:2) # TRUE
#' is_natural("a")  # FALSE: wrong type
#'
#' # Zero is not considered a natural number if 'strict' is TRUE:
#' is_natural(1e-9, strict = TRUE)   # FALSE
#' is_natural(1e-9, strict = FALSE)  # TRUE
#' is_natural(-1e-9, strict = FALSE) # FALSE: wrong sign
#'
#' # Zero-length input
#' is_natural(integer(0), allow_zerolength = FALSE) # FALSE
#' is_natural(integer(0), allow_zerolength = TRUE)  # TRUE
#' is_natural(numeric(0), allow_zerolength = FALSE) # FALSE
#' is_natural(numeric(0), allow_zerolength = TRUE)  # TRUE
#' is_natural(logical(0), allow_zerolength = TRUE)  # FALSE: wrong type
#' is_natural(NULL, allow_zerolength = TRUE)        # FALSE: wrong type
#'
#' # make_natural()
#' make_natural(5 + 1e-9) # 5L
#' try(make_natural(5 + 1e-7)) # error: checkinput::is_natural(5 + 1e-07) is not TRUE
#' try(make_natural(c(3, 5 + 1e-9), all = FALSE)) # error: wrong length
#' make_natural(c(3, 5 + 1e-9), all = TRUE) # c(3L, 5L)
#'
#' # Handling of large numbers by make_natural()
#' large_double <- .Machine$integer.max + 1
#' make_natural(c(4, large_double), all = TRUE, large_as_double = TRUE)
#' # c(4, 2147483648) with a warning that the return is of type 'double'
#' make_natural(c(4, large_double), all = TRUE, large_as_double = FALSE)
#' # c(4L, NA_integer_) with a warning that NAs have been introduced
#'
#' is_natural(Inf, allow_Inf = FALSE) # FALSE
#' try(make_natural(Inf, allow_Inf = FALSE)) # Error
#' is_natural(Inf, allow_Inf = TRUE) # TRUE
#' make_natural(Inf, allow_Inf = TRUE, large_as_double = TRUE)
#' # Inf, with a warning that the return is of type 'double'
#' make_natural(Inf, allow_Inf = TRUE, large_as_double = FALSE)
#' # NA_integer_, with a warning that NAs have been introduced
#'
#' @export
is_natural <- function(x,
                       strict = TRUE,
                       allow_zerolength = FALSE,
                       allow_NA = FALSE,
                       allow_NaN = FALSE,
                       allow_Inf = FALSE,
                       tol = .Machine$double.eps^0.5) {
  all_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
              allow_NA = allow_NA, allow_NaN = allow_NaN, allow_Inf = allow_Inf,
              tol = tol) &&
    length(x) <= 1L
}

#' @rdname is_natural
#' @export
all_natural <- function(x,
                        strict = TRUE,
                        allow_zerolength = FALSE,
                        allow_NA = FALSE,
                        allow_NaN = FALSE,
                        allow_Inf = FALSE,
                        tol = .Machine$double.eps^0.5) {
  # The other arguments are checked in 'all_numbers()'.
  stopifnot(is_logical(strict), is_logical(allow_Inf),
            is_positive(tol), tol < 0.5)

  if(!all_numbers(x = x, allow_zerolength = allow_zerolength,
                  allow_NA = allow_NA, allow_NaN = allow_NaN)) {
    return(FALSE)
  }

  if(!allow_Inf && any(is.infinite(x), na.rm = TRUE)) {
    return(FALSE)
  }

  if(any(x < 0 | (strict & x < 0.5), na.rm = TRUE)) {
    return(FALSE)
  }

  is.integer(x) || all(abs(x - round(x)) < tol, na.rm = TRUE)
}

#' @rdname is_natural
#' @export
make_natural <- function(x,
                         strict = TRUE,
                         allow_zerolength = FALSE,
                         allow_NA = FALSE,
                         allow_NaN = FALSE,
                         allow_Inf = FALSE,
                         tol = .Machine$double.eps^0.5,
                         large_as_double = TRUE,
                         all = FALSE) {
  name_x <- deparse1(substitute(x))
  stopifnot(is_logical(all), is_logical(large_as_double))

  if(all) {
    if(!all_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
                    allow_NA = allow_NA, allow_NaN = allow_NaN,
                    allow_Inf = allow_Inf, tol = tol)) {
      stop("checkinput::all_natural(", name_x, ") is not TRUE")
    }
  } else {
    if(!is_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
                   allow_NA = allow_NA, allow_NaN = allow_NaN,
                   allow_Inf = allow_Inf, tol = tol)) {
      stop("checkinput::is_natural(", name_x, ") is not TRUE")
    }
  }

  x <- round(x)

  if(!large_as_double) {
    x <- as.integer(x)
  } else {
    bool_too_large <- abs(x) > .Machine$integer.max
    if(any(bool_too_large, na.rm = TRUE)) {
      warning("The return of 'make_natural()' has type 'double' because",
              " 'large_as_double'\nis 'TRUE' and a return of type 'integer'",
              " would produce 'NA_integer_'\nthrough integer overflow for the",
              " following rounded values of 'x':\n",
              paste_quoted(x[bool_too_large]), call. = FALSE)
    } else {
      x <- as.integer(x)
    }
  }

  x
}
