#' Check that x is nearly equal to natural numbers
#'
#' Test element-wise near-equality to the natural numbers while allowing for
#' small numeric errors.
#'
#' @inheritParams is_logical
#' @param strict Exclude zero from the natural numbers?
#' @param tol A small [positive][is_positive()] number. Numbers that differ less
#' in value than `tol` are considered equal.
#'
#' @details
#' Natural numbers are the positive integers (`1`, `2`, `3`, etc.). Zero is
#' considered a natural number if argument `strict` is `FALSE`. `integer(0)` and
#' `numeric(0)` are considered natural numbers if argument `allow_zero` is
#' `TRUE`. Numbers that are [too large][.Machine] to be represented as
#' [integers][integer], [Inf], [NaN], and [NULL] are *never* considered natural
#' numbers in this implementation.
#'
#' `is_natural()` and `all_natural()` allow for small numeric errors when
#' comparing numbers. Such numeric errors can arise because of rounding or
#' representation error. As the `Note` at [`==`] warns, `x == round(x)` does
#' *not* allow for such errors but tests exact equality. Functions from other
#' packages with names like `integerish` frequently do *not* allow for small
#' numeric errors but are instead intended to allow values that are stored as
#' doubles (e.g., `3`) in addition to integer-type values (e.g., `3L`).
#'
#' If `allow_NA` is `TRUE`, `is_natural()` and `all_natural()` return `TRUE` for
#' `NA_integer_` and `NA_real_` but not for the other [NA]s or [NaN].
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a vector with only natural
#' numbers.
#'
#' @section Notes:
#' The code of `is_natural()` and `all_natural()` is partly based on the example
#' `is.wholenumber()` in [is.integer()].
#'
#' @section Programming notes:
#' Use of `is_natural(x)` or `all_natural(x)` should be followed by assigning
#' the rounded value to the argument, e.g., `x <- round(x)` or
#' `x <- as.integer(round(x))`.
#'
#' [is.integer()] does *not* check that `x` is a natural number (nor if `x` is a
#' whole number) but rather that `x` is of [type][typeof()] integer (see the
#' `Note` in [is.integer()]).
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' [progutils::are_equal()](https://github.com/JesseAlderliesten/progutils) to
#' check for element-wise near-equality of numbers;
#' [all.equal()] to check more generally for near-equality; [identical()] to
#' check for exact equality; [Comparison] to compare two vectors using binary
#' operators; [match()] and
#' [progutils::not_in()](https://github.com/JesseAlderliesten/progutils) to
#' compare character vectors;
#' [\R FAQ 7.31](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
#' for background on numerical equality.
#'
#' The vignettes about [design choices](../doc/design_choices.html) and about
#' [type coercion](../doc/type_coercion.html).
#'
#' @examples
#' is_natural(x = 5 + 1e-10) # TRUE
#' # Zero is not considered a natural number if 'strict' is TRUE:
#' is_natural(x = 1e-10, strict = TRUE) # FALSE
#' is_natural(x = 1e-10, strict = FALSE) # TRUE
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
#' # Zero is not considered a natural number if 'strict' is TRUE:
#' all_natural(x = c(1e-10, 3, 5), strict = TRUE) # FALSE
#' all_natural(x = c(1e-10, 3, 5), strict = FALSE) # TRUE
#' all_natural(x = c(-1e-10, 3, 5), strict = FALSE) # FALSE: wrong sign
#' all_natural(x = c(3, 5, Inf), strict = FALSE) # FALSE
#' all_natural(x = "a") # FALSE
#' all_natural(x = 1:2) # TRUE
#'
#' # Illustrate the need to follow use of is_natural(x) or all_natural(x) by
#' # assigning the rounded value to the argument
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
#' toy_fun_erroneous(x = 5 - 1e-8) # 1:4
#' toy_fun_correct(x = 5 - 1e-8) # 1:5
#'
#' @export
is_natural <- function(x, strict = TRUE, allow_zero = FALSE, allow_NA = FALSE,
                       tol = .Machine$double.eps^0.5) {
  all_natural(x = x, strict = strict, allow_zero = allow_zero,
              allow_NA = allow_NA, tol = tol) &&
    length(x) <= 1L
}

#' @rdname is_natural
#' @export
all_natural <- function(x, strict = TRUE, allow_zero = FALSE, allow_NA = FALSE,
                        tol = .Machine$double.eps^0.5) {
  stopifnot(is_logical(strict), is_logical(allow_zero), is_logical(allow_NA),
            is_positive(tol), tol < 0.5)

  if(!is.numeric(x) || !is.atomic(x) || !is.null(dim(x))) {
    return(FALSE)
  }

  if(!allow_zero && length(x) == 0L) {
    return(FALSE)
  }

  # It is not a problem that 'anyNA' also returns TRUE for NaNs: NaNs are never
  # allowed.
  if(!allow_NA && anyNA(x)) {
    return(FALSE)
  }

  # If 'allow_NA' is FALSE, NaNs have been catched above with the condition
  # '!allow_NA && anyNA(x)'
  if(any(x > .Machine$integer.max | is.nan(x) | is.infinite(x) | x < 0 |
         (strict & x < 0.5), na.rm = TRUE)) {
    return(FALSE)
  }

  all(abs(x - round(x)) < tol, na.rm = TRUE)
}
