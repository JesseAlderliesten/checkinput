#' Check that x contains syntactically valid names
#'
#' Check that `x` is a character vector with length larger than zero that
#' contains unique, syntactically valid names that do not consist of only dots
#' and do not suggest they were automatically created or modified.
#'
#' @param x Vector of names to test.
#' @param allow_underscores `TRUE` or `FALSE`: allow underscores?
#' @param allow_susp `TRUE` or `FALSE`: allow suspicious names?
#'
#' @details
#' [Syntactically valid][make.names] names ([R FAQ](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#What-are-valid-names_003f))
#' are names that (1) only consist of letters, numbers, dots and underscores;
#' (2) start with a letter or with a dot not followed by a number; (3) are not
#' [reserved words][Reserved] such as [for] or any of the [NA]s.
#'
#' The definition of *letter*, and thus what are syntactically valid names,
#' depends on the current [locale][locales] (see the references in the
#' `See Also` section). A conservative approach to prevent this from causing
#' problems would be to only use digits and unaccented Latin letters, but that
#' is *not* enforced by `all_names()`.
#'
#' Duplicated names and names that consist of only dots are *not* allowed by
#' `all_names()`, even though they *are* syntactically valid. Use
#' `all_names(x = unique(x), ...)` instead of `all_names(x = x, ...)` to allow
#' for duplicated names in checks. Duplicated names are *not* duplicated in
#' warnings.
#'
#' Names containing underscores (`_`) and suspicious names are *not* allowed by
#' `all_names()` if arguments `allow_underscores` or `allow_susp` are `FALSE`,
#' respectively, even though such names *are* syntactically valid.
#'
#' This function distinguishes two kinds of suspicious names:
#'
#' - Names that might have been created by [utils::read.csv()] to name unnamed
#'   columns, either because a particular column was unnamed or because data was
#'   inadvertently present in a supposedly empty (and thus unnamed) column that
#'   was read into \R: column names created by `read.csv()` have pattern `X`,
#'   `X.1`, `X.2`, etc. if `header` is `TRUE` and pattern `V1`, `V2`, `V3`, etc.
#'   if `header` is `FALSE`.
#' - Names that might have been modified by
#'   [make.names(x, unique = TRUE)][make.names()] (which is called by
#'   [utils::read.csv()] and by [data.frame()]) to make duplicated names unique:
#'   duplicated names get pattern `.1`, `.2`, `.3`, etc. added to them to make
#'   them unique, starting with adding `.1` to the first duplicate.
#'
#' It is *not* checked if a complete sequence of automatically created or
#' modified names is present in `x`, i.e., `X.2` will be flagged as suspicious
#' even if `X` and `X.1` are not present in `x`, and `e.2`  will be flagged as
#' suspicious even if `e` and `e.1` are not present in `x`. See the
#' `Programming note` for the structure of the regular expressions used to
#' identify suspicious names.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a character vector that only
#' contains syntactically valid names that satisfy the restrictions imposed by
#' the other function arguments.
#'
#' To get a named boolean vector indicating for each element of `x` if it is a
#' valid name, use `vapply(X = x, FUN.VALUE = logical(1), FUN = all_names, ...)`
#' instead of `all_names(x, ...)`.
#'
#' @section Programming note: The [regular expressions][regex] that are used
#' to identify suspicious names contain the following elements: (1) require a
#' pattern to start at the beginning of a string: `^`; (2) contain an `X` or a
#' `V` followed by a literal dot: `X\\.` or `V\\.`; (3) contain one or more
#' digits: `[[:digit:]]+`; (4) require a pattern to reach the end of the string:
#' `$`, to prevent matching names that start as suspicious but are not
#' suspicious because they have non-digit characters appended, e.g. `X.2a`.
#'
#' These elements are used to identify suspicious names as: (1) names that start
#' with an `X` followed by a dot and one or more digits until the end of the
#' string: `^X\\.[[:digit:]]+$`; (2) names that start with a `V` followed by one
#' or more digits until the end of the string: `^V[[:digit:]]+$`; (3) names that
#' contain a dot followed by one or more digits until the end of the string:
#' `\\.[[:digit:]]+$`.
#'
#' @seealso `janitor::make_clean_names()` for options to *change* names, such as
#' adjusting case and transliterating non-ASCII characters; [names()] to get or
#' set the names of an object; [all.names()] to find all names in an expression
#' or call; [\R FAQ 7.14](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#What-are-valid-names_003f)
#' for some ways 'name' is used in \R, with remarks about the validity of names.
#'
#' The section 'Details' of [make.names()] notes that the definition of a
#' *letter*, and thus what are syntactically valid names, depends on the current
#' [locale][locales]: see [Encoding], [validUTF8()] for background on encodings
#' and character sets; [iconv()] on conversions between encodings;
#' `tools::showNonASCII()` to show the non-ASCII bytes.
#'
#' @family
#' collections of checks on type and length
#'
#' @seealso
#' `vignette("design_choices", package = "checkinput")` and
#' `vignette("type_coercion", package = "checkinput")`.
#'
#' @examples
#' all_names(x = names(c(a = 1, b = 2))) # TRUE
#'
#' all_names(x = names(c(a = 1, 2))) # FALSE: empty name
#' all_names(x = NULL) # FALSE: NULL
#'
#' all_names(x = c("a", "b", "a")) # FALSE: duplicated name
#'
#' all_names(x = "X.2") # FALSE: name created by read.csv()
#' all_names(x = "X.2", allow_susp = TRUE) # TRUE
#'
#' all_names(x = "e.2") # FALSE: name modified by make.names()
#' all_names(x = "e.2", allow_susp = TRUE) # TRUE
#'
#' x_underscores <- c("abc_def", "ghi", "jk_l")
#' all_names(x = x_underscores, allow_underscores = TRUE) # TRUE
#' all_names(x = x_underscores, allow_underscores = FALSE) # FALSE: underscores
#'
#' x_dots <- c("abc.def", "..abc..def..", ".", "..", "...", "....")
#' all_names(x = x_dots) # FALSE: names that consist of only dots
#'
#' @export
all_names <- function(x, allow_underscores = TRUE, allow_susp = FALSE) {
  stopifnot(is_logical(allow_underscores), is_logical(allow_susp))

  # 'NULL' is catched later on with an informative message
  if(!is.null(x) && (!is.atomic(x) || !is.null(dim(x)) || !is.character(x))) {
    warning("'x' is not a character vector!")
    return(FALSE)
  }

  warn_text <- character(0)
  suggest_make_names <- FALSE

  if(anyDuplicated(x) != 0L) {
    bool_dupl <- duplicated(x)
    warn_text <- c(warn_text,
                   paste0("are duplicated: ", paste_quoted(unique(x[bool_dupl]))))
    suggest_make_names <- TRUE
    x <- x[!bool_dupl]
  }

  # Idea for the test inspired by vctrs:::two_to_three_dots()
  warn_text_onlydots <- character(0)
  bool_onlydots <- grepl(pattern = "^\\.+$", x = x)
  if(any(bool_onlydots)) {
    warn_text_onlydots <- paste0(
      "consist only of dots: ", paste_quoted(x[bool_onlydots]))
  }

  warn_text_underscores <- character(0)
  if(!allow_underscores) {
    bool_underscores <- grepl(pattern = "_", x = x, fixed = TRUE)
    if(any(bool_underscores)) {
      warn_text_underscores <- paste0(
        "contain underscores: ", paste_quoted(x[bool_underscores]))
      suggest_make_names <- TRUE
    }
  }

  # Notes:
  # - Checking for zero-length 'x' should be done before removing syntactically
  #   invalid names because that can also lead to zero-length 'x'.
  warn_text_zerolength <- character(0)
  if(length(x) == 0L) {
    if(is.null(x)) {
      warn_text_zerolength <- paste0(
        "'x' is NULL: did you use names(x) on an object without names,\nor",
        " colnames(x) on an object without column names?")
    } else {
      warn_text_zerolength <- "x has length zero but is not NULL"
    }
  }

  # Notes:
  # - Argument 'unique' of make.names() is 'FALSE' because duplicated names have
  #   been catched above.
  # - Argument 'allow_' of make.names() is 'TRUE' because names containing
  #   underscores have been catched above.
  # - make.names() replaces empty names ('""') with "X", so there is no need to
  #   separately test for these.
  # - Although make.names() replaces NAs in 'x' with "NA.", equality tests using
  #   '==' or '!=' on the NAs in 'x' will still return NA (see section 'Details'
  #   in ?'=='). Therefore argument 'na.rm' in any() is set to TRUE to prevent
  #   getting NA as condition and '|| any(bool_NA)' is used to catch the NAs.
  out_make_names <- make.names(x, unique = FALSE, allow_ = TRUE)
  bool_invalid <- x != out_make_names
  bool_NA <- is.na(x)
  if(any(bool_invalid, na.rm = TRUE) || any(bool_NA)) {
    bool_zchar_x <- !nzchar(x)
    bool_other_invalid <- bool_NA | (bool_invalid & !bool_zchar_x)
    invalid <- character(0)
    if(any(bool_other_invalid)) {
      invalid <- paste0(invalid, paste_quoted(x[bool_other_invalid]))
    }
    if(any(bool_zchar_x)) {
      invalid <- toString(c(invalid, "'\"\"' (i.e., an empty string)"))
    }
    warn_text <- c(warn_text, paste0("are syntactically invalid: ", invalid))
    suggest_make_names <- TRUE
    x <- x[!bool_invalid]
  }

  warn_text <- c(warn_text, warn_text_underscores, warn_text_onlydots)

  if(!allow_susp) {
    # See the 'Programming note' for an explanation of the regular expressions.
    bool_susp_v1 <- (x == "X" |
                       grepl(pattern = "^X\\.[[:digit:]]+$", x = x, fixed = FALSE) |
                       grepl(pattern = "^V[[:digit:]]+$", x = x, fixed = FALSE))
    if(any(bool_susp_v1, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been created by read.csv: ",
               paste_quoted(x[bool_susp_v1])))
      x <- x[!bool_susp_v1]
    }

    bool_susp_v2 <- grepl(pattern = "\\.[[:digit:]]+$", x = x, fixed = FALSE)
    if(any(bool_susp_v2, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = TRUE): ",
               paste_quoted(x[bool_susp_v2])))
    }
  }

  if(length(warn_text) > 0L) {
    warn_text <- paste0("Names ", paste0(warn_text, collapse = "; and "))
  }

  warn_text <- paste0(c(warn_text_zerolength, warn_text), collapse = " and ")

  if(suggest_make_names) {
    warn_text <- paste0(
      warn_text, ".\nUse 'x <- make.names(x, unique = TRUE",
      if(!allow_underscores) {", allow_ = FALSE"},
      ")' to\ncreate unique, syntactically valid names",
      if(!allow_underscores) {" without underscores"},
      "!")
  }

  # Another early return occurs if 'x' is not a character vector and not NULL.
  if(length(warn_text) > 1L || nchar(warn_text) > 0L) {
    chars_ok <- FALSE
    warning(x = warn_text)
    return(FALSE)
  }

  TRUE
}
