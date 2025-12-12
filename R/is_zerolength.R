#' Check if `x` is a zero-length object
#'
#' @param x object to test.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a zero-length object.
#'
#' @export
#'
#' @details Currently no check is performed on dimensions >> Check how zero-row and zero-column matrices / dataframes behave!
#'
#' @note
#' Zero-[length()] objects can have different [types][typeof()]: NULL ([NULL]),
#' logical (`logical(0)`), integer (`integer(0)`), double (`numeric(0)`),
#' complex (`complex(0)`), and character (`character(0)`). `""` is not a
#' zero-length object: it has a [width][nchar()] of 0 characters but a `length`
#' of 1.
#'
#' A [matrix] with zero rows is a zero-length object, whereas a [data.frame]
#' with zero rows is *not* (see the `Examples`).
#'
#' Checking equality to zero-length objects should be done using
#' `isTRUE(all.equal(x, <zero-length object>))` (`is.null()` can be used to
#' check if an object is `NULL`). Using [==][Comparison] leads to `logical(0)`,
#' which gives an error when used as complete [conditional statement][Control].
#'
#' `all(logical(0))`, and hence `all(numeric(0))` and `all(character(0))` that
#' get coerced to type `logical`, returns `TRUE`, see the `Note` in [all()].
#'
#' Although zero-length objects are discarded when combined into a vector with
#' other values, their types are taken into account for type coercion. For example,
#' numeric `314` will be coerced to character `"314"` when it is combined into a
#' vector with zero-length `character(0)`, such that `c(314, character(0))`
#' results in the character string `"314"`, not in the numeric value `314`.
#'
#' @seealso The vignette about type conversion:
#' `vignette("Type_Coercion", package = "checkinput")`.
#'
#' @family collections of checks on type and length
#'
#' @examples
#' is_zerolength(x = character(0)) # TRUE
#' is_zerolength(x = 0) # FALSE
#' # A matrix with zero rows is a zero-length object ...
#' is_zerolength(x = as.matrix(data.frame(a = 314))[numeric(0), , drop = FALSE])
#' # whereas a dataframe with zero rows is *not* a zero-length object
#' is_zerolength(x = data.frame(a = 314)[numeric(0), , drop = FALSE])
#'
#' # Zero-length objects affect type coercion.
#' num <- 314
#' str(num) # num 314
#' zerochar <- character(0)
#' str(zerochar) # chr(0)
#' str(c(num, zerochar)) # chr "314", not num 314
is_zerolength <- function(x) {
  length(x) == 0L
}
