#' Check that `x` is numeric
#'
#' Check that `x` is a numeric vector of the correct length with numbers of the
#' correct sign.
#'
#' @inheritParams is_logical
#' @param allow_NA `TRUE` or `FALSE`: allow numeric [NA]s (i.e., `NA_integer_`
#' and `NA_real_`)?
#' @param allow_NaN `TRUE` or `FALSE`: allow [NaN]s?
#'
#' @details
#' The correct length of `x` is one for `is_...()` and larger than zero for
#' `all_...()`, unless `allow_zero` is `TRUE`: then numeric-type zero-length `x`
#' is also allowed for both types of functions.
#'
#' `all_nonnegative()` and `is_nonnegative()` return `TRUE` for `0`, whereas
#' `is_positive()` returns `FALSE` for `0`.
#'
#' `all_...()` and `is_...()` return `TRUE` for `-Inf` and `Inf` if it has the
#' correct sign.
#'
#' `all_...()` and `is_...()` return `FALSE` for `NA_complex_`, even if
#' `allow_NA` is `TRUE`, because its mode is `complex` instead of `numeric`.
#'
#' [NaN] has [mode] `numeric`, despite meaning 'not a number'.
#'
#' @returns
#' `TRUE` or `FALSE` indicating if `x` is a numeric vector of the correct length
#' with numbers of the correct sign that adheres to the limitations set by the
#' other arguments.
#'
#' @section Programming notes:
#' [is.numeric()] tests the [mode()] of `x`, which is `numeric` for
#' floating-point numbers such as 3.2 and integers such as 3L. In contrast,
#' `class(x) == "numeric"` would test the [class()] of `x` which is `numeric`
#' for floating-point numbers but `integer` for integers (see the `Note on names`
#' in [is.numeric()]).
#'
#' The functions duplicate code instead of calling `is_number()` or
#' `all_numbers()`, to prevent performing checks twice.
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' The vignettes about [design choices](../doc/design_choices.html) and about
#' [type coercion](../doc/type_coercion.html).
#'
#' @examples
#' is_number(x = 1) # TRUE
#' is_number(x = 3.14) # TRUE
#' is_number(x = c(1, 2)) # FALSE: incorrect length
#' all_numbers(x = c(1, 2)) # TRUE
#' is_number(x = "a") # FALSE: incorrect type
#' is_number(x = numeric(0)) # FALSE: incorrect length
#' is_number(x = numeric(0), allow_zero = TRUE) # TRUE
#' is_number(x = NA_real_) # FALSE
#' is_number(x = NA_real_, allow_NA = TRUE) # TRUE
#' is_number(x = NA_character_, allow_NA = TRUE) # FALSE: incorrect type
#' is_number(x = NaN, allow_NA = TRUE) # FALSE, need allow_NaN = TRUE to allow NaN
#' is_number(x = NaN, allow_NaN = TRUE) # TRUE
#' is_number(x = Inf) # TRUE
#' is_nonnegative(x = 3) # TRUE
#' is_nonnegative(x = 0) # TRUE
#' all_nonnegative(x = c(3, 0)) # TRUE
#' all_nonnegative(x = numeric(0), allow_zero = TRUE) # TRUE
#' is_positive(x = 3) # TRUE
#' is_positive(x = 0) # FALSE
#'
#' @export
is_number <- function(x, allow_zero = FALSE, allow_NA = FALSE,
                      allow_NaN = FALSE) {
  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value.
  is.numeric(x) && is.atomic(x) && is.null(dim(x)) &&
    ((allow_zero && length(x) == 0L) ||
       (length(x) == 1L &&
          (allow_NA || !any(is.na(x) & !is.nan(x))) &&
          (allow_NaN || !any(is.nan(x)))))
}

#' @rdname is_number
#' @export
all_numbers <- function(x, allow_zero = FALSE, allow_NA = FALSE,
                        allow_NaN = FALSE) {
  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value.
  is.numeric(x) && is.atomic(x) && is.null(dim(x)) &&
    ((allow_zero && length(x) == 0L) ||
       (length(x) >= 1L &&
          (allow_NA || !any(is.na(x) & !is.nan(x))) &&
          (allow_NaN || !any(is.nan(x)))))
}

#' @rdname is_number
#' @export
is_nonnegative <- function(x, allow_zero = FALSE, allow_NA = FALSE,
                           allow_NaN = FALSE) {
  length_x <- length(x)
  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value.
  ok_p1 <- is.numeric(x) && is.atomic(x) && is.null(dim(x)) &&
    length_x <= 1L && (allow_zero || length_x == 1L)

  if(ok_p1) {
    if(length_x == 1L) {
      is_NA_x <- is.na(x)
      is_NaN_x <- is.nan(x)
      ok_p2 <- allow_NaN || !is_NaN_x
      ok_p3 <- allow_NA || !is_NA_x || is_NaN_x
      ok_p2 && ok_p3 && (is_NA_x || x >= 0)
    } else {
      TRUE
    }
  } else {
    FALSE
  }
}

#' @rdname is_number
#' @export
all_nonnegative <- function(x, allow_zero = FALSE, allow_NA = FALSE,
                            allow_NaN = FALSE) {
  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value.
  is.numeric(x) && is.atomic(x) && is.null(dim(x)) &&
    ((allow_zero && length(x) == 0L) ||
       (length(x) > 0L &&
          (allow_NA || !any(is.na(x) & !is.nan(x))) &&
          (allow_NaN || !any(is.nan(x))) &&
          all(x >= 0, na.rm = TRUE)))
}

#' @rdname is_number
#' @export
is_positive <- function(x, allow_zero = FALSE, allow_NA = FALSE,
                        allow_NaN = FALSE) {
  length_x <- length(x)
  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value.
  ok_p1 <- is.numeric(x) && is.atomic(x) && is.null(dim(x)) &&
    length_x <= 1L && (allow_zero || length_x == 1L)

  if(ok_p1) {
    is_NA_x <- is.na(x)
    is_NaN_x <- is.nan(x)
    length_x == 0L || ((allow_NaN || !is_NaN_x) && (allow_NA || !is_NA_x || is_NaN_x) &&
                         all(x > 0, na.rm = TRUE))
  } else {
    FALSE
  }
}
