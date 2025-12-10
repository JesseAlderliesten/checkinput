#' @rdname is_number
#'
#' @export
all_positive <- function(x) {
  all_numbers(x) && all(x > 0)
}
