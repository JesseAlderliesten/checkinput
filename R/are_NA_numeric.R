#' Are elements of vector `x` strictly numeric `NA`?
#'
#' @inheritParams is_logical
#' @param allow_NaN `TRUE` or `FALSE`: return `TRUE` for `NaN`s in `x`?
#'
#' @details
#' Zero-length `x` gives `FALSE` for `are_NA_numeric(x)`. Zero-length `x` would
#' give `logical(0)` for `is.na(x)` and `is.nan(x)`, making those functions less
#' suitable for use in logical statements.
#'
#' @returns A boolean vector indicating for each element of `x` if it is numeric
#' `NA`, or, if `allow_NaN` is `TRUE`, is numeric `NA` or `NaN`. `FALSE` for
#' zero-length `x`, and a vector of the same the length of `x` filled with
#' `FALSE` for non-numeric `x`.
#'
#' @section Programming note:
#' `is.na(x)` is `TRUE` for all types of `NA` but also for `NaN`, whereas
#' `is.nan(x)` is  `TRUE` for `NaN` but not for any type of `NA`.
#'
#' @seealso The vignette about type coercion:
#' `vignette("Type_Coercion", package = "checkinput")`. [is.na()] and [is.nan()]
#' that are used in this function.
#' @family collections of checks on type and length
#'
#' @examples
#' are_NA_numeric(c(-Inf, -3, 0, 3, Inf), allow_NaN = TRUE) # all FALSE
#'
#' are_NA_numeric(c(NA_real_, NA_integer_, NaN)) # c(TRUE, TRUE, FALSE)
#' are_NA_numeric(c(NA_real_, NA_integer_, NaN), allow_NaN = TRUE) # all TRUE
#' are_NA_numeric(NA, allow_NaN = TRUE) # FALSE: wrong type
#'
#' for(x in list(numeric(0), logical(0), character(0))) {
#'   print(are_NA_numeric(x = x)) # FALSE
#' }
#'
#' @export
are_NA_numeric <- function(x, allow_NaN = FALSE) {
  stopifnot(is.null(dim(x)), is_logical(allow_NaN))

  if(length(x) == 0L) {
    return(FALSE)
  }

  if(!is.numeric(x)) {
    return(rep(FALSE, length(x)))
  }

  is.na(x) & (allow_NaN | !is.nan(x))
}
