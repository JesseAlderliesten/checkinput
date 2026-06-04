#' Check that `x` is a zero-length object
#'
#' @inheritParams is_logical x
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a zero-length object.
#'
#' @section Notes:
#' [Zero-length][length()] objects can have different [types][typeof()]: NULL
#' ([NULL]),
#' logical (`logical(0)`), integer (`integer(0)`), double (`numeric(0)`),
#' complex (`complex(0)`), character (`character(0)`), and list ([list()] and
#' [data.frame()]). `""` is not a zero-length object: it has a `length` of one
#' despite its [width][nchar()] of zero characters. A dataframe with zero rows
#' is **not** a zero-length object: it has `length` equal to the number of
#' columns. In contrast, a [matrix][matrix()] with zero rows **is** a
#' zero-length object, see the `Examples`.
#'
#' [is.null()] should be used to check that an object is `NULL` and, more
#' generally, `isTRUE(all.equal(x, <zero-length object>))` should be used to
#' check equality to a zero-length object. Testing equality should **not** be
#' done by using [==][Comparison] because that leads to `logical(0)` if any of
#' the sides contains a zero-length object, which gives an error when used as
#' complete [conditional statement][Control].
#'
#' `all(logical(0))` returns `TRUE`, see the `Note` in [all()]. This is also the
#' case for `all(numeric(0))` and `all(character(0))` that get coerced to type
#' `logical`.
#'
#' Although zero-length objects are discarded when combined into a vector with
#' other values, their types **are** taken into account for type coercion, see
#' the vignette *Type coercion in vectors*:
#' `vignette("type_coercion", package = "checkinput")`. For example,
#' numeric `314` will be coerced to character `"314"` when it is combined into a
#' vector with zero-length `character(0)`, such that `c(314, character(0))`
#' results in the character string `"314"`, not in the numeric value `314`. If
#' zero-length objects are combined into a vector with only zero-length values,
#' the type of the vector is the highest type of the components in the hierarchy
#' `NULL` < `raw` < `logical` < `integer` < `double` < `complex` < `character` <
#' `list` < `expression`, see the section `Details` in `help(c)`.
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' The vignettes *Design choices regarding function input*:
#' `vignette("design_choices", package = "checkinput")` and
#' *Type coercion in vectors*:
#' `vignette("type_coercion", package = "checkinput")`.
#'
#' @examples
#' is_zerolength(x = character(0)) # TRUE
#' is_zerolength(x = 0) # FALSE
#' # A matrix with zero rows *is* a zero-length object ...
#' is_zerolength(x = as.matrix(data.frame(a = 314))[numeric(0), , drop = FALSE])
#' # ... whereas a dataframe with zero rows is *not* a zero-length object.
#' is_zerolength(x = data.frame(a = 314)[numeric(0), , drop = FALSE])
#'
#' # Zero-length objects affect type coercion.
#' num <- 314
#' str(num) # num 314
#' zerochar <- character(0)
#' str(zerochar) # chr(0)
#' str(c(num, zerochar)) # chr "314", not num 314
#'
#' @export
is_zerolength <- function(x) {
  length(x) == 0L
}
