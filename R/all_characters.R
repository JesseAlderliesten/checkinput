#' Check if `x` is character
#'
#' Checks if `x` is a character vector of the correct length with only allowed
#' character values.
#'
#' @inheritParams is_logical
#' @param allow_empty `TRUE` or `FALSE`: allow empty strings (`""`) in `x`?
#'
#' @details The correct length of `x` for `all_character()` is one or larger if
#' argument `allow_zero` is `FALSE` and zero or larger if argument `allow_zero`
#' is `TRUE`. The correct length of `x` for `is_character()` is one if argument
#' `allow_zero` is `FALSE` and zero or one if argument `allow_zero` is `TRUE`.
#' Argument `allow_zero` only has an effect if `character(0)` is the only value
#' in `x`, because `character(0)` is discarded when it is put in a vector
#' together with other values.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a character vector of the
#' correct length with only allowed character values.
#'
#' @inheritSection is_logical Programming note
#' @note `all_characters()` and `is_character()` are mainly used for argument
#' checking and therefore by default return `FALSE` for empty strings,
#' zero-length character strings, and `NA_character_`. In contrast,
#' [base::is.character()] returns `TRUE` for these inputs, which can be achieved
#' with `all_characters()` and `is_character()` by setting arguments
#' `allow_empty`, `allow_zero`, or `allow_NA` to `TRUE`, respectively.
#'
#' @seealso The vignette about type conversion:
#' `vignette("Type_Coercion", package = "checkinput")`.
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
#'
#' @export
all_characters <- function(x, allow_empty = FALSE, allow_zero = FALSE,
                           allow_NA = FALSE) {
  stopifnot(is_logical(allow_empty), is_logical(allow_zero), is_logical(allow_NA))
  is.character(x) && is.null(dim(x)) &&
    (allow_empty || all(nzchar(x, keepNA = FALSE))) &&
    (allow_zero || length(x) > 0) &&
    (allow_NA || !anyNA(x))
}
