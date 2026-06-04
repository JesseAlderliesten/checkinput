#' Check that `x` is numeric
#'
#' Check that `x` is a numeric vector of the correct length with numbers of the
#' correct sign.
#'
#' @inheritParams is_logical x allow_zero_length allow_NA
#' @param allow_NaN `TRUE` or `FALSE`: allow [NaN]s?
#'
#' @details
#' `is_number()`, `all_numbers()`, `all_nonnegative()` and `is_nonnegative()`
#' return `TRUE` for zero, whereas `is_positive()` returns `FALSE` for zero. All
#' these functions return `TRUE` for `-Inf` and `Inf` if it has the correct sign.
#'
#' `is_number()`, `is_nonnegative()`, and `is_positive()` return `TRUE` for `x`
#' with length one. `all_numbers()` and `all_nonnegative()` return `TRUE` for
#' `x` with length larger than zero. All these functions return `TRUE` for
#' numeric-type [zero-length][is_zerolength()] `x` if `allow_zero_length` is
#' `TRUE`.
#'
#' All these functions return `TRUE` for `NA_integer_` and `NA_real_` if
#' `allow_NA` is `TRUE`. Even then they return `FALSE` for `NA_complex_` because
#' its mode is `complex` instead of `numeric`.
#'
#' All these functions return `TRUE` for [NaN] (which has [mode] `numeric`,
#' despite meaning 'not a number') if `allow_NaN` is `TRUE`.
#'
#' @returns
#' `TRUE` or `FALSE` indicating if `x` is a numeric vector of the correct length
#' only containing allowed numbers.
#'
#' @section Programming notes:
#' [is.numeric()] tests the [mode()] of `x`, which is `numeric` for
#' floating-point numbers such as `3.2` and integers such as `3L`. In contrast,
#' `class(x) == "numeric"` (or, more robust, `inherits(x = x, what = "numeric")`)
#' would test the [class()] of `x` which is `numeric` for floating-point numbers
#' but `integer` for integers (see the `Note on names` in [is.numeric()]).
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' The vignettes *Design choices*:
#' `vignette("design_choices", package = "checkinput")` and
#' *Type coercion*: `vignette("type_coercion", package = "checkinput")`.
#'
#' @examples
#' is_number(x = 1) # TRUE
#' is_number(x = 3.14) # TRUE
#' is_number(x = c(1, 2)) # FALSE: incorrect length
#' all_numbers(x = c(1, 2)) # TRUE
#' is_number(x = "a") # FALSE: incorrect type
#' is_number(x = numeric(0)) # FALSE: incorrect length
#' is_number(x = numeric(0), allow_zero_length = TRUE) # TRUE
#' is_number(x = NA_real_) # FALSE
#' is_number(x = NA_real_, allow_NA = TRUE) # TRUE
#' is_number(x = NA_character_, allow_NA = TRUE) # FALSE: incorrect type
#' is_number(x = NaN, allow_NA = TRUE) # FALSE, need allow_NaN = TRUE to allow NaN
#' is_number(x = NaN, allow_NaN = TRUE) # TRUE
#' is_number(x = Inf) # TRUE
#' is_nonnegative(x = 3) # TRUE
#' is_nonnegative(x = 0) # TRUE
#' all_nonnegative(x = c(3, 0)) # TRUE
#' all_nonnegative(x = numeric(0), allow_zero_length = TRUE) # TRUE
#' is_positive(x = 3) # TRUE
#' is_positive(x = 0) # FALSE
#'
#' @export
is_number <- function(x, allow_zero_length = FALSE, allow_NA = FALSE,
                      allow_NaN = FALSE) {
  length_x <- length(x)
  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value.
  is.numeric(x) && is.atomic(x) && is.null(dim(x)) &&
    ((allow_zero_length && length_x == 0L) ||
       (length_x == 1L &&
          (allow_NA || !any(is.na(x) & !is.nan(x))) &&
          (allow_NaN || !any(is.nan(x)))))
}

#' @rdname is_number
#' @export
all_numbers <- function(x, allow_zero_length = FALSE, allow_NA = FALSE,
                        allow_NaN = FALSE) {
  length_x <- length(x)
  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value.
  is.numeric(x) && is.atomic(x) && is.null(dim(x)) &&
    ((allow_zero_length && length_x == 0L) ||
       (length_x >= 1L &&
          (allow_NA || !any(is.na(x) & !is.nan(x))) &&
          (allow_NaN || !any(is.nan(x)))))
}

#' @rdname is_number
#' @export
is_nonnegative <- function(x, allow_zero_length = FALSE, allow_NA = FALSE,
                           allow_NaN = FALSE) {
  is_number(x, allow_zero_length = allow_zero_length, allow_NA = allow_NA,
            allow_NaN = allow_NaN) &&
    all(x >= 0, na.rm = TRUE)
}

#' @rdname is_number
#' @export
all_nonnegative <- function(x, allow_zero_length = FALSE, allow_NA = FALSE,
                            allow_NaN = FALSE) {
  all_numbers(x, allow_zero_length = allow_zero_length, allow_NA = allow_NA,
              allow_NaN = allow_NaN) &&
    all(x >= 0, na.rm = TRUE)
}

#' @rdname is_number
#' @export
is_positive <- function(x, allow_zero_length = FALSE, allow_NA = FALSE,
                        allow_NaN = FALSE) {
  is_number(x = x, allow_zero_length = allow_zero_length, allow_NA = allow_NA,
            allow_NaN = allow_NaN) &&
    all(x > 0, na.rm = TRUE)
}
