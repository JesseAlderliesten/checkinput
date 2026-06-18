#' Quote and concatenate `x` to a string
#'
#' Quote elements of a dimensionless atomic object and concatenate the result to
#' a single character string.
#'
#' @details
#' `NULL` is returned as `"'NULL'"`, other zero-length objects are returned as
#' `"'<class>(0)'"` (e.g., `"'logical(0)'"`), `""` as `'""'`, logical `NA` as
#' `"'NA'"`, and non-logical `NA`s as `"'NA_<class>_'"` (e.g., `"'NA_real_'"`;
#' for [factors][factor] this is `"'NA_character_'"`).
#'
#' @param x Dimensionless atomic object to be converted to a single character
#' string.
#'
#' @returns
#' A character string consisting of the elements of `x` surrounded by single
#' quotes, separated by commas. See `Details` on the handling of some special
#' values.
#'
#' @section Notes:
#' An error is thrown if multiple arguments are provided because then `x`
#' probably was accidentally not [combined][c()]. For example, the call
#' `paste_quoted("a", "b")` will return the error `unused argument ("b")`. The
#' probably intended call is `paste_quoted(c("a", "b"))`, returning `"'a', 'b'"`.
#'
#' `paste_quoted()` drops [names][names()] of `x`, which is pointed out in a
#' [warning][warning()] if `x` has names. Use [unname()] on named `x` to prevent
#' these warnings.
#'
#' @seealso
#' [toString()] which can be used instead of `paste(x, collapse = ", ")`;
#' [`Quotes`] and [sQuote()] for documentation on quotes;
#' [paste0()];
#' `progutils::unpaste_unquote()` for the approximate opposite of `paste_quoted()`;
#' `progutils::vect_to_char()` to preserve names of numeric `x`
#'
#' @family functions to modify character vectors
#'
#' @examples
#' paste_quoted(c(3, 4)) # "'3', '4'"
#' paste_quoted(NULL) # "'NULL'"
#' paste_quoted(c(a = 3, b = 4)) # "'3', '4'" # Warns about dropping names.
#'
#' @export
paste_quoted <- function(x) {
  # Need condition 'is.null(x)' because is.atomic(NULL) was FALSE before R 4.4.0.
  stopifnot(is.atomic(x) || is.null(x), is.null(dim(x)))

  if(!is.null(names(x))) {
    warning_text <- "'x' has names, these will be discarded."
    if(is.numeric(x)) {
      warning_text <- paste0(warning_text,
                             "\nUse progutils::vect_to_char() instead of",
                             " paste_quoted() to preserve names of numeric 'x'.")
    }
    warning(warning_text)
  }

  if(is.factor(x)) {
    x <- as.character(x)
  }

  if(length(x) == 0L) {
    if(is.null(x)) {
      x <- "NULL"
    } else {
      x <- paste0(class(x), "(0)")
    }
  }

  bool_NA <- is.na(x) & !is.nan(x)
  if(any(bool_NA)) {
    if(!is.logical(x)) {
      x[bool_NA] <- paste0("NA_", class(x), "_")
    }
  }

  bool_zchar <- !nzchar(x)
  if(any(bool_zchar)) {
    x[bool_zchar] <- "\"\""
  }

  # Same as paste0(sQuote(x, q = FALSE), collapse = ", ") but much faster
  paste0("'", paste(x, collapse = "', '"), "'")
}
