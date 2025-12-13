#' Check if `x` is character
#'
#' Checks if `x` is a character vector of the correct length with only allowed
#' character values.
#'
#' @inheritParams is_logical
#' @param allow_empty `TRUE` or `FALSE`: allow empty strings (`""`) in `x`? See
#' `Details`.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a character vector of the
#' correct length with only allowed character values, see `Details`.
#'
#' @export
#'
#' @details The correct length of `x` for `is_character()` is one if argument
#' `allow_zero` is `FALSE` and zero or one if argument `allow_zero` is `TRUE`.
#' The correct length of `x` for `all_character()` is one or larger if argument
#' `allow_zero` is `FALSE` and zero or larger if argument `allow_zero` is
#' `TRUE`. Argument `allow_zero` only has an effect if `character(0)` is the
#' only value in `x`, because `character(0)` is discarded when it is put in a
#' vector together with other values.
#'
#' @note `is_character()` and `all_characters()` are mainly used for argument
#' checking and therefore by default return `FALSE` for empty strings,
#' zero-length character strings, and `NA_character_`. In contrast,
#' [is.character()] returns `TRUE` for these inputs, which can be achieved
#' with `is_character()` and `all_characters()` by setting arguments
#' `allow_empty`, `allow_zero`, or `allow_NA` to `TRUE`, respectively.
#'
#' @inheritSection is_logical Programming note
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
  # Argument checking is deferred to all_characters().
  length(x) < 2L &&
    all_characters(x, allow_empty = allow_empty, allow_zero = allow_zero,
                   allow_NA = allow_NA)
}
