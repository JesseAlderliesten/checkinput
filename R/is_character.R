#' Check if `x` is character
#'
#' Checks if `x` is a character vector of the correct length with only allowed
#' character values.
#'
#' @param x object to test.
#' @param allow_empty logical of length 1 indicating if empty strings (`""`)
#' should be allowed, see `Details`.
#' @param allow_zero logical of length 1 indicating if zero-length character
#' strings (`character(0)`) should be allowed, see `Details`.
#' @param allow_NA logical of length 1 indicating if `NA_character_` should be
#' allowed, see `Details`.
#'
#' @returns A single logical value (`TRUE`, `FALSE`, or `NA`) indicating if `x`
#' is a character vector of the correct length with only allowed character
#' values, see `Details`.
#'
#' @export
#'
#' @details If argument `allow_zero` is `FALSE`, the correct length is 1 for
#' `is_character()` and >= 1 for `all_characters()`. If `allow_zero` is `TRUE`,
#' zero-length characters are also allowed. `allow_zero` only has an effect if
#' `character(0)` is the only value in `x`, because `character(0)` is discarded
#' when it is put in a vector together with other values.
#'
#' `NA` is returned if arguments `allow_empty`, `allow_zero`, or `allow_NA` are
#' `NA` and `x` is `""`, `character(0)`, or `NA_character_`, respectively.
#'
#' @note `is_character()` and `all_characters()` are mainly used for argument
#' checking and therefore by default return `FALSE` for empty strings,
#' zero-length character strings, and `NA_character_`. In contrast,
#' [is.character()] returns `TRUE` for these inputs, which can be achieved
#' with `is_character()` and `all_characters()` by setting arguments
#' `allow_empty`, `allow_zero`, or `allow_NA` to `TRUE`, respectively.
#'
#' @seealso The vignette about type conversion:
#' `vignette("Type_Coercion", package = "checkinput")`.
#'
#' @family collections of checks on type and length
#'
#' @examples
#' is_character("a") # TRUE
#' is_character(c("a", "b")) # FALSE: incorrect length
#' all_characters(c("a", "b")) # TRUE
#' is_character(1) # FALSE: incorrect type
#' is_character(NA_character_) # FALSE: default 'allow_NA' is FALSE
#' is_character(NA_character_, allow_NA = TRUE) # TRUE
#' is_character(NA, allow_NA = TRUE) # FALSE: incorrect type
is_character <- function(x, allow_empty = FALSE, allow_zero = FALSE,
                         allow_NA = FALSE) {
  length(x) < 2L &&
    all_characters(x, allow_empty = allow_empty, allow_zero = allow_zero,
                   allow_NA = allow_NA)
}
