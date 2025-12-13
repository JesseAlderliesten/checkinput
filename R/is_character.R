#' @rdname all_characters
#'
#' @export
is_character <- function(x, allow_empty = FALSE, allow_zero = FALSE,
                         allow_NA = FALSE) {
  # Argument checking is deferred to all_characters().
  length(x) < 2L &&
    all_characters(x, allow_empty = allow_empty, allow_zero = allow_zero,
                   allow_NA = allow_NA)
}
