#' Check that x is Logical
#'
#' Check that `x` is a length-one [logical vector][is.logical()] with only
#' allowed values.
#'
#' @param x object to check.
#' @param allow_zerolength `TRUE` or `FALSE`: allow
#' [zero-length][is_zerolength()] `x` of the correct [type][typeof()]?
#' @param allow_NA `TRUE` or `FALSE`: allow [`NA`] of the correct type in `x`?
#'
#' @details
#' `is_logical()` returns `TRUE` if `x` is
#'
#' - `TRUE`
#' - `FALSE`
#' - [`logical(0)`][is_zerolength()] and `allow_zerolength` is `TRUE`
#' - logical-type [`NA`] and `allow_NA` is `TRUE`
#'
#' @returns `TRUE` or `FALSE`
#'
#' @family
#' collections of checks on type and length
#'
#' @examples
#' is_logical(TRUE) # TRUE
#' is_logical(c(TRUE, TRUE)) # FALSE: incorrect length
#' is_logical(1) # FALSE: incorrect type
#' is_logical(NA) # FALSE: default 'allow_NA' is FALSE
#' is_logical(NA, allow_NA = TRUE) # TRUE
#' is_logical(NA_character_, allow_NA = TRUE) # FALSE: incorrect type
#' is_logical(matrix(TRUE)) # FALSE: has non-NULL dimensions
#' is_logical(data.frame(TRUE)) # FALSE: has non-NULL dimensions
#'
#' @export
is_logical <- function(x,
                       allow_zerolength = FALSE,
                       allow_NA = FALSE) {
  stopifnot(isFALSE(allow_zerolength) || isTRUE(allow_zerolength),
            is.null(dim(allow_zerolength)),
            isFALSE(allow_NA) || isTRUE(allow_NA),
            is.null(dim(allow_NA)))

  # is.null(dim(x)) is needed to return `FALSE` for matrices with a single value
  is.logical(x) && is.null(dim(x)) &&
    (length(x) == 1L || (allow_zerolength && length(x) == 0L)) &&
    (allow_NA || !anyNA(x))
}
