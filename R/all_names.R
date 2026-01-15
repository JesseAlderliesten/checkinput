#' Check that x contains syntactically valid names
#'
#' Check that `x` is a character vector that only consists of unique, syntactically
#' valid names that do not only consists of dots and do not suggest they were
#' automatically created or modified when data was read into \R.
#'
#' @param x Vector of names to test.
#' @param allow_underscores `TRUE` or `FALSE`: allow underscores?
#' @param allow_susp `TRUE` or `FALSE`: allow suspicious names?
#'
#' @details
#' Only syntactically valid names should be used in data analyses because \R
#' functions might handle invalid names incorrectly.
#' [Syntactically valid][make.names] names ([\R FAQ](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#What-are-valid-names_003f)):
#' (1) only consist of letters, numbers, dots and underscores;
#' (2) start with a letter, or with a dot not followed by a number;
#' (3) are not [reserved words][Reserved] such as [for] or any of the [NA]s.
#'
#' The definition of *letter*, and thus what are syntactically valid names,
#' depends on the current [locale][locales] (see the references in the
#' `See Also` section). A conservative approach to ensure syntactically valid
#' names on one system are also syntactically valid on another system would be
#' to only use digits and unaccented Latin letters, but that is *not* enforced
#' by `all_names()`.
#'
#' Duplicated names are *not* allowed by `all_names()`, even if they *are*
#' syntactically valid, because not all \R functions will handle duplicated
#' names as 'expected'. For example, not all operations on data frames will
#' preserve duplicated column names (as documented in [data.frame()]).
#' Duplicated names are *not* duplicated in warnings.
#'
#' Names that consist of only dots are *not* allowed by `all_names()`, even
#' though they *are* syntactically valid, because such names are not informative
#' and probably arose because names were modified, see the next paragraph.
#'
#' Suspicious names are *not* allowed by `all_names()` if argument `allow_susp`
#' is `FALSE`. This is intended to warn about syntactically invalid names that
#' have been silently changed to syntactically valid names when data was read
#' into \R. The warning emitted if a suspicious name is found distinguishes
#' several kinds of suspicious names, see the `Programming note` for details.
#'
#' Names containing underscores (`_`) are *not* allowed by `all_names()` if
#' argument `allow_underscores` is `FALSE` (which is *not* the default), even
#' though they *are* syntactically valid. This is useful to check names that
#' will be concatenated by underscores later on, for example to create an ID-tag.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a character vector that only
#' contains unique, syntactically valid names that do not consist of only dots
#' and do not suggest they were automatically created or modified when data was
#' read into \R.
#'
#' @section Programming note:
#' The [regular expressions][regex] that are used to identify suspicious names
#' can:
#' - require a pattern to start at the beginning of a string: `^`;
#' - contain `X`, `V`, or a dot: `X`, `V`, or `\\.`, respectively;
#' - contain one or more digits: `[[:digit:]]+`;
#' - require a pattern to reach the end of the string: `$`.
#'
#' These elements are used to identify the following types of suspicious names:
#' - names that start with `X.` or `V`, followed by one or more digits until the
#'   end of the string: `^X\\.[[:digit:]]+$` or `^V[[:digit:]]+$`: such names
#'   are created by [utils::read.csv()] to name unnamed columns if argument
#'   `header` is `TRUE` or `FALSE`, respectively;
#' - names that contain one dot followed by one or more digits until the end of
#'   the string: `\\.[[:digit:]]+$`: such names are created by
#'   [make.names(x, unique = TRUE)][make.names()] (which is used by
#'   [utils::read.csv()] and by [data.frame()]) to make duplicated names unique
#'   by adding `.1`, `.2`, `.3`, etc., starting with adding `.1` to the first
#'   duplicate;
#' - names that contain two dots followed by one or more digits until the end of
#'   the string: `\\.\\.[[:digit:]]+$`: such names are created by
#'   `vctrs::vec_as_names(x)` to make duplicated names unique;
#' - names that start with one or more dots: `^\\.+`: such names are created by
#'   `vctrs::vec_as_names(x)`. Names that only consist of dots are not allowed
#'   by `all_names()` are thus considered invalid instead of suspicious).
#'
#' It is *not* checked if a complete sequence of automatically created or
#' modified names is present in `x`, i.e., `X.2` will be flagged as suspicious
#' even if `X` and `X.1` are not present in `x`.
#'
#' @seealso
#' `janitor::make_clean_names()` for options to *change* names, such as
#' adjusting case and transliterating non-ASCII characters; [names()] to get or
#' set the names of an object; [all.names()] to find all names in an expression
#' or call; [\R FAQ 7.14](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#What-are-valid-names_003f)
#' for some ways 'name' is used in \R, with remarks about the validity of names.
#'
#' The section 'Details' of [make.names()] and the section `Names and Identifiers`
#' in [Quotes] note that the definition of a *letter*, and thus what are
#' syntactically valid names, depends on the current [locale][locales]: see
#' [Encoding], [validUTF8()] for background on encodings and character sets;
#' [iconv()] on conversions between encodings; `tools::showNonASCII()` to show
#' the non-ASCII bytes; and package
#' [stringi](https://CRAN.R-project.org/package=stringi) that provides
#' facilities to process character strings.
#'
#' The [vignette about design choices](../doc/design_choices.html) and the
#' [vignette about type coercion](../doc/type_coercion.html).
#'
#' @family
#' collections of checks on type and length
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
#' all_names(x = c(".", "..", "...", "....")) # FALSE: consist of only dots
#' all_names(x = c("abc.def", "abc..def..")) # TRUE
#'
#' all_names(x = "..abc..def..") # FALSE: might have been modified by vctrs::vec_as_names(x)
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

  # Notes:
  # - Checking for zero-length 'x' should be done before removing any invalid
  #   or suspicious names because that can also lead to zero-length 'x'.
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
      "only consist of dots: ", paste_quoted(x[bool_onlydots]))
    x <- x[!bool_onlydots]
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
    bool_susp_readcsv <- (x == "X" |
                            grepl(pattern = "^X\\.[[:digit:]]+$", x = x) |
                            grepl(pattern = "^V[[:digit:]]+$", x = x))
    if(any(bool_susp_readcsv, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been created by read.csv: ",
               paste_quoted(x[bool_susp_readcsv])))
      x <- x[!bool_susp_readcsv]
    }

    bool_susp_makenames <- grepl(pattern = "\\.[[:digit:]]+$", x = x)
    # Names that only consist of dots have already been removed from 'x' above
    bool_susp_vecasnames <- grepl(pattern = "^\\.+|\\.\\.[[:digit:]]+$", x = x)
    bool_susp_both <- bool_susp_makenames & bool_susp_vecasnames
    if(any(bool_susp_both, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = TRUE) or by",
               " vctrs::vec_as_names(x): ",
               paste_quoted(x[bool_susp_both])))
      bool_susp_makenames[bool_susp_both] <- FALSE
      bool_susp_vecasnames[bool_susp_both] <- FALSE
    }

    if(any(bool_susp_makenames, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = TRUE): ",
               paste_quoted(x[bool_susp_makenames])))
    }

    if(any(bool_susp_vecasnames, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by vctrs::vec_as_names(x): ",
               paste_quoted(x[bool_susp_vecasnames])))
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
