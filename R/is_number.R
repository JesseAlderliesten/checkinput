#' Check if x is a numeric vector of the correct length with numbers of the
#' correct sign.
#'
#' @inheritParams is_character
#'
#' @returns A single non-NA logical value indicating if `x` is a numeric vector
#' of the correct length with numbers of the correct sign.
#'
#' @export
#'
#' @note `NA_real_` and `NA_integer_` are considered numeric, whereas
#' `NA_complex_` is not.
#'
#' @seealso [is_character()]
#'
#' @examples
#' is_number(1) # TRUE
#' is_number(c(1, 2)) # FALSE: incorrect length
#' all_numbers(c(1, 2)) # TRUE
#' is_number("a") # FALSE: incorrect type
#' is_number(NA_real_) # TRUE: `allow_NA` has not yet been implemented
#' is_number(NA_character_) # FALSE: incorrect type
is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.atomic(x) && is.null(dim(x))
}
