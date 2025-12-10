#' @rdname is_character
#'
#' @export
all_characters <- function(x, allow_empty = FALSE, allow_zero = FALSE,
                           allow_NA = FALSE) {
  is.character(x) &&
    (allow_zero || length(x) > 0) &&
    (allow_empty || all(nzchar(x, keepNA = FALSE))) &&
    (allow_NA || !anyNA(x))
}
