#' Check if `x` only contains syntactically valid names
#'
#' Check if `x` is a character vector that only contains unique, syntactically
#' valid names that do not suggest they were automatically created or modified.
#'
#' @param x Vector of names to test.
#' @param allow_dupl `TRUE` or `FALSE`: allow duplicate names?
#' @param allow_susp `TRUE` or `FALSE`: allow suspicious names?
#'
#' @details
#' [Syntactically valid names][base::make.names] are names that (1) only consist
#' of letters, numbers, dots and underscores; (2) start with a letter or with a
#' dot not followed by a number; (3) are not [reserved words][Reserved] such as
#' [for] or any of the [NA]s.
#'
#' Duplicated names, which *are* syntactically valid, are *not* allowed if
#' argument `allow_dupl` is `FALSE`. Suspicious names are not allowed if
#' argument `allow_susp` is `FALSE`. This function distinguishes two kinds of
#' suspicious names (see `Programming note 2` for the structure of the regular
#' expressions used to identify them):
#'
#' - Names that might have been created by [utils::read.csv()] to name unnamed
#'   columns, either because a particular column was unnamed or because data was
#'   inadvertently present in a supposedly empty (and thus unnamed) column that
#'   was read into \R: column names created by `read.csv()` have pattern `X`,
#'   `X.1`, `X.2`, `...` if `header` is `TRUE` and pattern `V1`, `V2`, `V3`,
#'   `...` if `header` is `FALSE`.
#' - Names that might have been modified by
#'   [make.names(..., unique = TRUE)][base::make.names()] to make duplicated
#'   names unique: a dot and a number (starting at `1` for the first duplicate)
#'   is added to duplicated names to make them unique. This is also used by
#'   read.csv().
#'
#' Invalid names are *not* reported as suspicious names. In addition, duplicated
#' names are reported only *once* in warnings emitted by `all_names()`.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a character vector that only
#' contains unique, syntactically valid names.
#'
#' @section Programming note: This implementation is based on the legacy-code of
#' `has_colnames()` and `has_names()`, although significant changes have been
#' made: `x` should be the names themselves, not an object to check the names
#' of; `NA` is not allowed for arguments other than `x`; argument
#' `allow_duplicated` was shortened to `allow_dupl`; arguments `check_syntax`
#' and `silently` were removed.
#'
#' @section Programming note 2: The [regular expressions][base::regex] that are
#' used to identify suspicious names contain the following elements: (1) require
#' a pattern to start at the beginning of a string: `^`; (2) contain an `X` or a
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
#' @section Programming note 3: Could add argument 'allow_' and pass that to
#' make.names(), to catch names with underscores?
#'
#' @family collections of checks on type and length
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
#' @export
all_names <- function(x, allow_dupl = FALSE, allow_susp = FALSE) {
  stopifnot(is.null(dim(x)), is_logical(allow_dupl), is_logical(allow_susp))

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

  # Notes:
  # - make.names() replaces empty names ('""') with "X", so there is no need to
  #   separately test for these.
  # - Although make.names() replaces NAs in 'x' with "NA.", equality tests using
  #   '==' or '!=' on the NAs in 'x' will still return NA (see 'Details' in
  #   ?'=='). Therefore argument 'na.rm' in any() is set to TRUE to prevent
  #   getting NA as condition and '| is.na(x)' is used to catch the NAs.
  # - Argument 'unique' of make.names() is set to FALSE because duplicated names
  #   have been catched above.
  out_make_names <- make.names(x, unique = FALSE, allow_ = TRUE)
  bool_invalid <- x != out_make_names
  if(any(bool_invalid, na.rm = TRUE) || anyNA(x)) {
    bool_zchar_x <- !nzchar(x)
    bool_other_invalid <- is.na(x) | (bool_invalid & !bool_zchar_x)
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

  warn_text <- c(warn_text, warn_text_dupl)

  if(!allow_susp) {
    # See 'Programming note 2' for an explanation of the regular expressions.
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
    warn_text_p1 <- paste0("'x' is NULL: did you use names(x) or colnames(x)",
                           " on an object without\nnames or column names?")
  } else {
    warn_text_p1 <- character(0)
  }

  warn_text <- paste0(c(warn_text_p1, warn_text), collapse = " and ")
  if(suggest_make_names) {
    warn_text <- paste0(warn_text,
                        ". Use 'x <- make.names(x, unique = TRUE)' to create",
                        " unique, syntactically valid names!")
  }

  if(nchar(warn_text) > 0L) {
    warning(x = warn_text)
  }

  chars_ok
}
