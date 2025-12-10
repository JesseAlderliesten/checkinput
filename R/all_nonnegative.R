#' @rdname is_number
#'
#' @export
all_nonnegative <- function(x) {
  all_numbers(x) && all(x >= 0)
}
