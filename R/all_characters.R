#' @rdname is_character
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
