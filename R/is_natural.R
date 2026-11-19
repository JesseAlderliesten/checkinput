#' Check that x is Nearly Equal to Natural Numbers
#'
#' Check element-wise near-equality to natural [numbers][is.numeric()] while
#' allowing for small numeric errors.
#'
#' @inheritParams is_number x allow_zerolength allow_NaN
#' @param include_zero include zero among the natural numbers?
#' @param allow_NA `TRUE` or `FALSE`: allow [`NA_integer_`] and [`NA_real_`] in
#' `x`?
#' @param tol a [positive][is_positive()] number smaller than `0.5`. Numbers
#' that differ less than `tol` are considered equal.
#' @param all `TRUE` or `FALSE`: use `all_natural()` instead of `is_natural()`?
#'
#' @details
#' Natural numbers are the positive integers (`1`, `2`, `3`, etc.). The
#' functions `*_natural()` allow for small numeric errors when comparing numbers
#' to the natural numbers, which is important because rounding errors and
#' representation errors during normal calculations can lead to such small
#' numeric errors, see the first `Examples`. As the `Note` at `help("==")`
#' warns, using `x == round(x)` does **not** allow for such errors but checks
#' exact equality, see the first `Examples`.
#'
#' Zero is considered a natural number if argument `include_zero` is `TRUE`.
#' `make_natural()` then returns `0L` for it. Even then small negative numbers
#' are **not** considered natural numbers.
#'
#' `integer(0)` and `numeric(0)` are considered natural numbers if argument
#' `allow_zerolength` is `TRUE`. `make_natural()` then returns `integer(0)` for
#' both.
#'
#' [`NA_integer_`][NA] and `NA_real_` are considered natural numbers if
#' `allow_NA` is `TRUE`. `make_natural()` then returns `NA_integer_` for both.
#' Even then `NA_complex_` is **not** considered a natural number because it is
#' [`complex`][is.complex()] instead of [`numeric`][is.numeric()].
#'
#' [`NaN`] is considered a natural number if `allow_NaN` is `TRUE`.
#' `make_natural()` then returns `NA_integer_` for `NaN`.
#'
#' [`NULL`], negative numbers, [`Inf`], and numbers larger than
#' `.Machine$integer.max` are **not** considered natural numbers.
#' `make_natural()` throws an [error][stop()] for them.
#'
#' @returns
#' `is_natural()` and `all_natural()` return `TRUE` or `FALSE` indicating if `x`
#' is a vector of the appropriate length with only natural numbers according to
#' the values of their other arguments.
#'
#' `make_natural(x, ..., all = FALSE)` and `make_natural(x, ..., all = TRUE)`
#' throw an [error][stop()] if `x` contains values that are not natural
#' according to `is_natural(x, ...)` or `all_natural(x, ...)`, respectively. If
#' no error is thrown, `make_natural(x, ...)` [rounds][round()] `x` to a whole
#' number and coerces it to [`type`][typeof()] [`integer`], which might lead to
#' the return containing `integer(0)` or `NA_integer_`, see 'Details'.
#'
#' @section Warning:
#' `TRUE` should be used **cautiously** for arguments `include_zero`,
#' `allow_zerolength`, `allow_NA`, and `allow_NaN` because `is_natural()` and
#' `all_natural()` then return `TRUE` and `make_natural()` then returns `0L` for
#' `0L` and `0`; `integer(0)` for `integer(0)` and `numeric(0)`; and
#' `NA_integer_` for `NA_integer_`, `NA_real_`, or `NaN` in `x`.
#'
#' Such returns make it **impossible** to use the `*_natural()` functions to
#' confirm that `x` can be used in places where whole numbers are expected,
#' for example when using `x` to create a range through [seq()] or [`:`] to
#' create a counter or to [Extract] parts of an object.
#'
#' Argument `include_zero` is included because 0 is considered a natural number
#' in some definitions of a natural number. Arguments `allow_zerolength`,
#' `allow_NA` and `allow_NaN` are provided for consistency with functions like
#' [is_number()].
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
#' Numbers larger than [`.Machine$integer.max`][.Machine] (about 2 * 10^9) are
#' too large to be represented as 32-bit [integers][is.integer()] used by \R.
#' Such large numbers are **not** considered natural by `is_natural()` and
#' `all_natural()`, such that `make_natural()` throws an error for them. This
#' prevents an inconsistency between `is_natural()` and `all_natural()`
#' returning `TRUE` for such numbers if they differ less than `tol` from the
#' natural numbers while `make_natural()` would return `NA_integer_` for them
#' because they would be coerced to `type` `integer`, with the warning
#' '`NAs produced by integer overflow`'. Letting `make_natural()` convert such
#' large numbers to `type` `double` to extend the range of whole numbers that
#' can be handled by the `*_natural` functions, the approach used in
#' **checkinput** 1.7.0 if argument `large_as_double` was `TRUE`, would create
#' the problem that the numbers are **not** exact anymore, while creating exact
#' numbers is the point of the `*_natural` functions.
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
#' `help(Arithmetic)` on using `%/%` for integer division;
#' section 'Warning' in [round()] and
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
#' # Zero is considered a natural number if 'include_zero' is TRUE:
#' is_natural(1e-9, include_zero = FALSE) # FALSE
#' is_natural(1e-9, include_zero = TRUE)  # TRUE
#' is_natural(-1e-9, include_zero = TRUE) # FALSE: wrong sign
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
#' # Handling of large numbers
#' large_double <- .Machine$integer.max + 1
#' is_natural(large_double) # FALSE
#' try(make_natural(c(4, large_double), all = TRUE))
#' # error: checkinput::all_natural(c(4, large_double)) is not TRUE
#' is_natural(Inf) # FALSE
#' try(make_natural(Inf))
#' # Error: checkinput::is_natural(Inf) is not TRUE
#'
#' @export
is_natural <- function(x,
                       include_zero = FALSE,
                       allow_zerolength = FALSE,
                       allow_NA = FALSE,
                       allow_NaN = FALSE,
                       tol = .Machine$double.eps^0.5) {
  all_natural(x = x,
              include_zero = include_zero,
              allow_zerolength = allow_zerolength,
              allow_NA = allow_NA,
              allow_NaN = allow_NaN,
              tol = tol) &&
    length(x) <= 1L
}

#' @rdname is_natural
#' @export
all_natural <- function(x,
                        include_zero = FALSE,
                        allow_zerolength = FALSE,
                        allow_NA = FALSE,
                        allow_NaN = FALSE,
                        tol = .Machine$double.eps^0.5) {
  # The other arguments are checked in 'all_numbers()'.
  stopifnot(is_logical(include_zero), is_positive(tol), tol < 0.5)

  if(!all_numbers(x = x,
                  allow_zerolength = allow_zerolength,
                  allow_NA = allow_NA,
                  allow_NaN = allow_NaN)) {
    return(FALSE)
  }

  if(include_zero) {
    if(any(x < 0 | x > .Machine$integer.max | is.infinite(x), na.rm = TRUE)) {
      return(FALSE)
    }
  } else {
    if(any(x < 0.5 | x > .Machine$integer.max | is.infinite(x), na.rm = TRUE)) {
      return(FALSE)
    }
  }

  is.integer(x) || all(abs(x - round(x)) < tol, na.rm = TRUE)
}

#' @rdname is_natural
#' @export
make_natural <- function(x,
                         include_zero = FALSE,
                         allow_zerolength = FALSE,
                         allow_NA = FALSE,
                         allow_NaN = FALSE,
                         tol = .Machine$double.eps^0.5,
                         all = FALSE) {
  name_x <- deparse1(substitute(x))
  stopifnot(is_logical(all))

  if(all) {
    if(!all_natural(x = x,
                    include_zero = include_zero,
                    allow_zerolength = allow_zerolength,
                    allow_NA = allow_NA,
                    allow_NaN = allow_NaN,
                    tol = tol)) {
      stop("checkinput::all_natural(", name_x, ") is not TRUE")
    }
  } else {
    if(!is_natural(x = x,
                   include_zero = include_zero,
                   allow_zerolength = allow_zerolength,
                   allow_NA = allow_NA,
                   allow_NaN = allow_NaN,
                   tol = tol)) {
      stop("checkinput::is_natural(", name_x, ") is not TRUE")
    }
  }

  as.integer(round(x))
}
