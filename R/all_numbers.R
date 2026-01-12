#' @rdname is_number
#'
#' @export
all_numbers <- function(x) {
  is.numeric(x) && is.atomic(x) && is.null(dim(x)) && length(x) > 0
}
