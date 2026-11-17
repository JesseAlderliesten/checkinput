#' Check that x is Nearly Equal to Natural Numbers
#'
#' Check element-wise near-equality to natural [numbers][is.numeric()] while
#' allowing for small numeric errors.
#'
#' @inheritParams is_number x allow_zerolength
#' @param strict exclude zero from the natural numbers?
#' @param allow_NA `TRUE` or `FALSE`: allow [`NA`] of the correct type in `x`?
#' See the section 'Large Numbers' below on the disadvantage of using `TRUE` for
#' `allow_NA` with large numbers. [`NaN`] is never allowed in `x`.
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
#' `integer(0)` and `numeric(0)` are considered natural numbers if argument
#' `allow_zerolength` is `TRUE`.
#'
#' [`NA_integer_`][NA] and `NA_real_` are considered natural numbers if
#' `allow_NA` is `TRUE`. Even then `NA_complex_` is **not** considered a natural
#' number because it is [`complex`][is.complex()] instead of
#' [`numeric`][is.numeric()].
#'
#' Positive [`Inf`] is considered a natural number if `allow_Inf` is `TRUE`.
#'
#' [`NULL`], [`NaN`], and negative numbers are **not**
#' considered natural numbers.
#'
#' See section `Large Numbers` below on the treatment of numbers that are too
#' large to be represented as integers.
#'
#' @returns
#' For `is_natural()` and `all_natural()`: `TRUE` or `FALSE` indicating if `x`
#' is a vector of the appropriate length with only natural numbers.
#'
#' For `make_natural()`: `x`, [rounded][round()] to a whole number. The return
#' has [`type`][typeof()] [`integer`], unless argument `large_as_double` is
#' `TRUE` and `x` contains values larger than [`.Machine$integer.max`][.Machine]
#' (about 2 * 10^9) or [`Inf`]: then the return has type `double`, see the
#' section 'Large Numbers' below.
#'
#' `make_natural()` throws an [error][stop()] if `x` contains values that are
#' not allowed or values that differ more than `tol` from the natural numbers.
#'
#' @section Large Numbers:
#' Numbers larger than [`.Machine$integer.max`][.Machine] (about 2 * 10^9) are
#' too large to be represented as [`integer`], see section 'Details' in
#' `help("integer")`. When such large numbers or [`Inf`] are produced in
#' [arithmetic operations][Arithmetic] that should result in integers, they are
#' coerced to [`NA_integer_`] with the warning
#' '`NAs produced by integer overflow`'. `NA_integer_` itself is considered
#' natural if `allow_NA = TRUE`, which is **not** useful when e.g., testing
#' that `x` is a natural number before using it as an index.
#'
#' `make_natural()` also coerces numbers that are too large to be represented as
#' `integer` to `NA_integer_` if argument `large_as_double` is `FALSE`. If
#' argument `large_as_double` is `TRUE`, `make_natural()` returns the actual
#' values as [`type`][typeof()] [`double`][is.double()] instead of `integer`,
#' with the warning '`The return of 'make_natural()' has type 'double'`' that
#' also indicates which values are too large.
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
#' # NA_integer_ with a warning that NAs have been introduced
#'
#' @export
is_natural <- function(x,
                       strict = TRUE,
                       allow_zerolength = FALSE,
                       allow_NA = FALSE,
                       allow_Inf = FALSE,
                       tol = .Machine$double.eps^0.5) {
  all_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
              allow_NA = allow_NA, allow_Inf = allow_Inf, tol = tol) &&
    length(x) <= 1L
}

#' @rdname is_natural
#' @export
all_natural <- function(x,
                        strict = TRUE,
                        allow_zerolength = FALSE,
                        allow_NA = FALSE,
                        allow_Inf = FALSE,
                        tol = .Machine$double.eps^0.5) {
  # Arguments 'allow_zerolength' and 'allow_NA' are checked in 'all_numbers()'.
  stopifnot(is_logical(strict), is_logical(allow_Inf),
            is_positive(tol), tol < 0.5)

  # 'NaN' is not considered a natural number.
  if(!all_numbers(x = x, allow_zerolength = allow_zerolength,
                  allow_NA = allow_NA, allow_NaN = FALSE)) {
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
                         allow_Inf = FALSE,
                         tol = .Machine$double.eps^0.5,
                         large_as_double = TRUE,
                         all = FALSE) {
  name_x <- deparse1(substitute(x))
  stopifnot(is_logical(all), is_logical(large_as_double))

  if(all) {
    if(!all_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
                    allow_NA = allow_NA, allow_Inf = allow_Inf, tol = tol)) {
      stop("checkinput::all_natural(", name_x, ") is not TRUE")
    }
  } else {
    if(!is_natural(x = x, strict = strict, allow_zerolength = allow_zerolength,
                   allow_NA = allow_NA, allow_Inf = allow_Inf, tol = tol)) {
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
