#' Check if `x` only contains syntactically valid names
#'
#' Check if `x` is a character vector that only contains unique, syntactically
#' valid names that do not suggest they were automatically created or modified.
#'
#' @param x Vector of names to test.
#' @param allow_dupl `TRUE` or `FALSE`: allow duplicate names?
#' @param allow_susp `TRUE` or `FALSE`: allow suspicious names?
#' @param allow_underscores `TRUE` or `FALSE`: allow underscores?
#'
#' @details
#' [Syntactically valid names][make.names] are names that (1) only consist of
#' letters, numbers, dots and underscores; (2) start with a letter or with a dot
#' not followed by a number; (3) are not [reserved words][Reserved] such as
#' [for] or any of the [NA]s. The definition of 'letter' depends on the current
#' locale, as noted in the section 'Details' in [make.names()].
#'
#' Duplicated names, suspicious names, and names containing underscores (`_`)
#' *are* syntactically valid but are *not* allowed if arguments `allow_dupl`,
#' `allow_susp`, or `allow_underscores` are `FALSE`, respectively.
#'
#' This function distinguishes two kinds of suspicious names (see the
#' `Programming note` for the structure of the regular expressions used to
#' identify them):
#'
#' - Names that might have been created by [utils::read.csv()] to name unnamed
#'   columns, either because a particular column was unnamed or because data was
#'   inadvertently present in a supposedly empty (and thus unnamed) column that
#'   was read into \R: column names created by `read.csv()` have pattern `X`,
#'   `X.1`, `X.2`, `...` if `header` is `TRUE` and pattern `V1`, `V2`, `V3`,
#'   `...` if `header` is `FALSE`.
#' - Names that might have been modified by
#'   [make.names(..., unique = TRUE)][make.names()] to make duplicated names
#'   unique: a dot and a number (starting at `1` for the first duplicate) is
#'   added to duplicated names to make them unique. This is also used by
#'   read.csv().
#'
#' Syntactically invalid names are *not* reported as suspicious names. In
#' addition, duplicated names are *not* duplicated in warnings emitted by
#' `all_names()`.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a character vector that only
#' contains syntactically valid names and adhere to the restrictions imposed by
#' the other function arguments.
#'
#' @section Programming note: The [regular expressions][regex] that are used
#' to identify suspicious names contain the following elements: (1) require a
#' pattern to start at the beginning of a string: `^`; (2) contain an `X` or a
#' `V` followed by a literal dot: `X\\.` or `V\\.`; (3) contain one or more
#' digits: `[[:digit:]]+`; (4) require a pattern to reach the end of the string:
#' `$`.
#'
#' These elements are used to identify suspicious names as: (1) column names
#' that start with an `X` followed by a dot and one or more digits until the end
#' of the string: `^X\\.[[:digit:]]+$`; (2) column names that start with a `V`
#' followed by one or more digits until the end of the string: `^V[[:digit:]]+$`;
#' (3) column names that contain a dot followed by one or more digits until the
#' end of the string: `\\.[[:digit:]]+$`. Indicating the end of the string
#' prevents matching names that start as suspicious names but are not suspicious
#' because they have non-digit characters appended, e.g. `X.2a`.
#'
#' @section Programming note 2: This implementation is based on legacy-code of
#' `has_colnames()` and `has_names()`, with significant changes.
#'
#' @section To do:
#' - Add an explanation and examples showing the problem of using syntactically
#'   invalid names: see https://stackoverflow.com/questions/54597535/.
#' - See https://github.com/r-lib/testthat/blob/main/R/expect-named.R for code
#'   to check against allowed names, with or without ordering.
#'
#' @family collections of checks on type and length
#' @seealso [all.names()] to find all names in an expression or call; [names()]
#' to get or set the names of an object.
#'
#' @examples
#' all_names(x = names(c(a = 1, b = 2))) # TRUE
#'
#' all_names(x = names(c(a = 1, 2))) # FALSE: empty name.
#' all_names(x = NULL) # FALSE: NULL
#'
#' all_names(x = c("a", "b", "a")) # FALSE: duplicated name
#' all_names(x = c("a", "b", "a"), allow_dupl = TRUE) # TRUE
#'
#' all_names(x = "X.1") # FALSE: name created by read.csv()
#' all_names(x = "X.1", allow_susp = TRUE) # TRUE
#'
#' all_names(x = "e.1") # FALSE: name modified by make.names()
#' all_names(x = "e.1", allow_susp = TRUE) # TRUE
#'
#' x_underscores <- c("abc_def", "ghi", "jk_l")
#' all_names(x = x_underscores, allow_underscores = TRUE) # TRUE
#' all_names(x = x_underscores, allow_underscores = FALSE) # FALSE: underscores.
#'
#' @export
all_names <- function(x, allow_dupl = FALSE, allow_susp = FALSE,
                      allow_underscores = TRUE) {
  stopifnot(is.null(dim(x)), is_logical(allow_dupl), is_logical(allow_susp),
            is_logical(allow_underscores))

  warn_text <- character(0)
  warn_text_dupl <- character(0)
  suggest_make_names <- FALSE
  chars_ok <- all_characters(x, allow_empty = FALSE, allow_zero = FALSE,
                             allow_NA = FALSE)
  if(!chars_ok && !is.null(x) && length(x) == 0L) {
    warn_text <- c(warn_text, "zero-length values")
    suggest_make_names <- TRUE
  }

  if(anyDuplicated(x) != 0L) {
    bool_dupl <- duplicated(x)
    if(!allow_dupl) {
      warn_text_dupl <- paste0("duplicated names: '",
                          paste0(unique(x[bool_dupl]), collapse = "', '"), "'")
      suggest_make_names <- TRUE
    }
    x <- x[!bool_dupl]
  }

  bool_underscores <- grepl(pattern = "_", x = x, fixed = TRUE)
  if(!allow_underscores && any(bool_underscores)) {
    warn_text_underscores <- paste0(
      "values containing underscores: '",
      paste0(x[bool_underscores], collapse = "', '"), "'")
    suggest_make_names <- TRUE
  } else {
    warn_text_underscores <- character(0)
  }

  # Notes:
  # - make.names() replaces empty names ('""') with "X", so there is no need to
  #   separately test for these.
  # - Using 'TRUE' for argument 'allow_' because values in 'x' containing
  #   underscores have been catched above.
  # - Although make.names() replaces NAs in 'x' with "NA.", equality tests using
  #   '==' or '!=' on the NAs in 'x' will still return NA (see 'Details' in
  #   ?'=='). Therefore argument 'na.rm' in any() is set to TRUE to prevent
  #   getting NA as condition and '| is.na(x)' is used to catch the NAs.
  # - Argument 'unique' of make.names() is set to FALSE because duplicated names
  #   have been catched above.
  bool_NA <- is.na(x)
  out_make_names <- make.names(x, unique = FALSE, allow_ = TRUE)
  bool_invalid <- x != out_make_names
  if(any(bool_invalid, na.rm = TRUE) || any(bool_NA)) {
    bool_zchar_x <- !nzchar(x)
    bool_other_invalid <- bool_NA | (bool_invalid & !bool_zchar_x)
    invalid <- character(0)
    if(any(bool_other_invalid)) {
      invalid <- paste0(invalid, "'", paste0(x[bool_other_invalid],
                                             collapse = "', '"), "'")
    }
    if(any(bool_zchar_x)) {
      invalid <- paste0(c(invalid, "'\"\"' (i.e., an empty string)"),
                        collapse = ", ")
    }
    warn_text <- c(warn_text, paste0("syntactically invalid values: ", invalid))
    suggest_make_names <- TRUE
    x <- x[!bool_invalid]
  }

  warn_text <- c(warn_text, warn_text_dupl, warn_text_underscores)

  if(!allow_susp) {
    # See the 'Programming note' for an explanation of the regular expressions.
    bool_susp_v1 <- (x == "X" |
                       grepl(pattern = "^X\\.[[:digit:]]+$", x = x, fixed = FALSE) |
                       grepl(pattern = "^V[[:digit:]]+$", x = x, fixed = FALSE))
    if(any(bool_susp_v1, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("names that might have been created by read.csv: '",
               paste0(x[bool_susp_v1], collapse = "', '"), "'."))
      x <- x[!bool_susp_v1]
    }

    bool_susp_v2 <- grepl(pattern = "\\.[[:digit:]]+$", x = x, fixed = FALSE)
    if(any(bool_susp_v2, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("names that might have been modified by make.names(..., unique = TRUE): '",
               paste0(x[bool_susp_v2], collapse = "', '"), "'."))
    }
  }

  if(length(warn_text) > 0L) {
    chars_ok <- FALSE
    warn_text <- paste0("'x' contains ", paste0(warn_text, collapse = "; and "))
  }

  if(is.null(x)) {
    chars_ok <- FALSE
    warn_text_p1 <- paste0("'x' is NULL: did you use names(x) on an object",
                           " without names,\nor colnames(x) on an object",
                           " without column names?")
  } else {
    warn_text_p1 <- character(0)
  }

  warn_text <- paste0(c(warn_text_p1, warn_text), collapse = " and ")
  if(suggest_make_names) {
    warn_text <- paste0(warn_text,
                        ".\nUse 'x <- make.names(x, unique = TRUE)' to create",
                        " unique, syntactically valid names!")
  }

  if(nchar(warn_text) > 0L) {
    warning(x = warn_text)
  }

  chars_ok
}
