#' @rdname is_number
#'
#' @export
is_positive <- function(x) {
  is_number(x) && x > 0
}
