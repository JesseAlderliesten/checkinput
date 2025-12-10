#' @rdname is_number
#'
#' @export
all_numbers <- function(x) {
  is.numeric(x) && length(x) > 0 && is.null(dim(x))
}
