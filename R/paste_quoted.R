#' Internal function to concatenate a vector to a single string of quoted
#' elements, representing `NULL` as `"'NULL'"` and other zero-length objects as
#' `"'<class>(0)'"`, e.g., `"'logical(0)'"`. A fuller, exported version of this
#' function is present in package
#' [progutils](https://github.com/JesseAlderliesten/progutils).
#'
#' @noRd
paste_quoted <- function(x) {
  stopifnot(is.null(dim(x)))

  if(is.null(x)) {
    x <- "NULL"
  }

  if(length(x) == 0L) {
    x <- paste0(class(x), "(0)")
  }

  paste0("'", paste(x, collapse = "', '"), "'")
}
