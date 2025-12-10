#' @rdname is_number
#'
#' @export
is_nonnegative <- function(x) {
  is_number(x) && x >= 0
}
