#' Check that x is nearly equal to natural numbers
#'
#' Test element-wise near-equality to natural numbers while allowing for small
#' numeric errors.
#'
#' @inheritParams is_logical
#' @param strict Exclude zero from the natural numbers?
#' @param tol A small [positive][is_positive()] number. Numbers that differ less
#' than `tol` are considered equal.
#' @param all `TRUE` or `FALSE`: use `all_natural()` instead of `is_natural()`?
#'
#' @details
#' Natural numbers are the positive integers (`1`, `2`, `3`, etc.). Zero is
#' considered a natural number if argument `strict` is `FALSE`. `integer(0)` and
#' `numeric(0)` are considered natural numbers if argument `allow_zero` is
#' `TRUE`. [Inf], [NaN], [NULL], and numbers that are [too large][.Machine] to
#' be represented as [integers][integer] are **never** considered natural
#' numbers in this implementation.
#'
#' If `allow_NA` is `TRUE`, `is_natural()` and `all_natural()` return `TRUE` for
#' `NA_integer_` and `NA_real_` but not for the other [NA]s or [NaN].
#'
#' `is_natural()`, `all_natural()` and `make_natural()` allow for small numeric
#' errors when
#' comparing numbers. Such numeric errors can arise because of rounding or
#' representation error. As the `Note` at [`==`] warns, `x == round(x)` does
#' **not** allow for such errors but tests exact equality.
#'
#' @returns `is_natural()` and `all_natural()`: `TRUE` or `FALSE` indicating if
#' `x` is a vector of the appropriate length with only natural numbers.
#' `make_natural()`: `x`, [rounded][round] to a whole number and coerced to
#' [integer] type.
#'
#' @section Notes:
#' `make_natural(x, all = FALSE)` and `make_natural(x, all = TRUE)` throw an
#' error if `x` is not natural according to `is_natural(x)` or `all_natural(x)`,
#' respectively.
#'
#' The code of `is_natural()` and `all_natural()` is partly based on the example
#' `is.wholenumber()` in [is.integer()].
#'
#' @section Programming notes:
#' Use of `is_natural(x)` or `all_natural(x)` inside [stopifnot()] should be
#' followed by assigning the rounded value to the argument:
#' `x <- as.integer(round(x))`. Alternatively, use `make_natural(x)` and assign
#' the result to `x` (then there is no need to use [stopifnot()]: `make_natural()`
#' throws an error if `x` is not natural.
#'
#' [is.integer()] does **not** check that `x` is a natural number (nor if `x` is
#' a whole number) but rather that `x` is of [type][typeof()] integer, see the
#' `Note` in [is.integer()].
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' `progutils::are_equal()` to check for element-wise near-equality of numbers;
#' [all.equal()] to check more generally for near-equality; [identical()] to
#' check for exact equality; [Comparison] to compare two vectors using binary
#' operators; [match()] and `progutils::not_in()` to compare character vectors;
#' [\R FAQ 7.31](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
#' for background on numerical equality.
#'
#' The vignettes *Design choices regarding function input*:
#' `vignette("design_choices", package = "checkinput")` and
#' *Type coercion in vectors*:
#' `vignette("type_coercion", package = "checkinput")`.
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
#' toy_fun_safe <- function(x, all = TRUE) {
#'   x <- make_natural(x, all = all)
#'   seq_len(x)
#' }
#'
#' toy_fun_erroneous(x = 5 - 1e-8) # 1:4
#' toy_fun_correct(x = 5 - 1e-8) # 1:5
#' toy_fun_safe(x = 5 - 1e-8) # 1:5
#'
#' try(toy_fun_erroneous(x = 5.1)) # Error: is_natural(x) is not TRUE
#' try(toy_fun_correct(x = 5.1)) # Error: is_natural(x) is not TRUE
#' try(toy_fun_safe(x = 5.1, all = FALSE)) # Error: is_natural(x) is not TRUE
#' try(toy_fun_safe(x = 5.1, all = TRUE)) # Error: all_natural(x) is not TRUE
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

#' @rdname is_natural
#' @export
make_natural <- function(x, strict = TRUE, allow_zero = FALSE, allow_NA = FALSE,
                         all = FALSE, tol = .Machine$double.eps^0.5) {
  name_x <- deparse1(substitute(x))
  stopifnot(is_logical(all))
  if(all && !all_natural(x = x, strict = strict, allow_zero = allow_zero,
                         allow_NA = allow_NA, tol = tol)) {
    stop("checkinput::all_natural(", name_x, ") is not TRUE")
  }
  if(!all && !is_natural(x = x, strict = strict, allow_zero = allow_zero,
                         allow_NA = allow_NA, tol = tol)) {
    stop("checkinput::is_natural(", name_x, ") is not TRUE")
  }
  as.integer(round(x))
}
