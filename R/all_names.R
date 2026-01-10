#' Check if `x` only contains syntactically valid names
#'
#' Check if `x` is a character vector that only contains unique, syntactically
#' valid names that do not suggest they were automatically created or modified.
#'
#' @param x Vector of names to test.
#' @param allow_dupl `TRUE` or `FALSE`: allow duplicate names?
#' @param allow_susp `TRUE` or `FALSE`: allow suspicious names?
#' @param allow_underscores `TRUE` or `FALSE`: allow underscores?
#' @param allow_onlydots `TRUE` or `FALSE`: allow names that consist only of
#' dots?
#' @param allow_nonASCII: allow non-ASCII characters?
#'
#' @details
#' [Syntactically valid names][make.names] are names that (1) only consist of
#' letters, numbers, dots and underscores; (2) start with a letter or with a dot
#' not followed by a number; (3) are not [reserved words][Reserved] such as
#' [for] or any of the [NA]s.
#'
#' The definition of 'letter', and thus what are syntactically valid names,
#' depends on the current [locale][locales] (as noted in the section 'Details'
#' in [make.names()]). Using `FALSE` for `allow_nonASCII` to only allow
#' ASCII-characters tries to minimize this problem (see the `See Also` section
#' for references).
#'
#' Duplicated names, suspicious names, names containing underscores (`_`), and
#' names names that consist only of dots *are* syntactically valid (and names
#' that contain non-ASCII characters might be valid as well) but such names are
#' *not* allowed if arguments `allow_dupl`, `allow_susp`, `allow_underscores`,
#' `allow_alldots`, or `allow_nonASCII` are `FALSE`, respectively.
#'
#' This function distinguishes two kinds of suspicious names (see the
#' `Programming note` for the structure of the regular expressions used to
#' identify them):
#'
#' - Names that might have been created by [utils::read.csv()] to name unnamed
#'   columns, either because a particular column was unnamed or because data was
#'   inadvertently present in a supposedly empty (and thus unnamed) column that
#'   was read into \R: column names created by `read.csv()` have pattern `X`,
#'   `X.1`, `X.2`, etc. if `header` is `TRUE` and pattern `V1`, `V2`, `V3`, etc.
#'   if `header` is `FALSE`.
#' - Names that might have been modified by
#'   [make.names(x, unique = TRUE)][make.names()] to make duplicated names
#'   unique: duplicated names get pattern `.1`, `.2`, `.3`, etc. added to them
#'   to make them unique, starting with adding `.1` to the first duplicate. That
#'   is also used by read.csv().
#'
#' It is *not* checked if a complete sequence of automatically created or
#' modified names is present in `x`, i.e., `X.2` and `e.2` will be flagged as
#' suspicious even if `X` and `X.1` or `e` and `e.1` are not present in `x`,
#' respectively.
#'
#' Syntactically invalid names are *not* reported as suspicious names, and
#' duplicated names are *not* duplicated in warnings.
#'
#' @returns `TRUE` or `FALSE` indicating if `x` is a character vector that only
#' contains syntactically valid names that adhere to the restrictions imposed by
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
#' @section To do:
#' Add an explanation and examples showing the problem of using syntactically
#' invalid names: see https://stackoverflow.com/questions/54597535/.
#'
#' See the stub 'progutils::all_present()' to require all names in 'vals' to be
#' present, and optionally not allow other values.
#'
#' @section Wishlist:
#' Implement showing offending non-ASCII characters if `tools` is installed,
#' using [tools::showNonASCII()]. See
#' https://r-pkgs.org/dependencies-in-practice.html#sec-dependencies-in-suggests?
#'
#' @note
#' To get a named boolean vector indicating for each element of vector `x` if it
#' is a name, use `vapply(X = x, FUN.VALUE = logical(1), FUN = all_names, ...)`.
#'
#' @seealso `janitor::make_clean_names()` for more options, such as adjusting
#' case and transliterating non-ASCII characters. [names()] to get or set the
#' names of an object; [all.names()] to find all names in an expression or call.
#'
#' On ASCII, see [Encoding], [iconv()], [locales], [tools::showNonASCII()],
#' [stringi::stri_enc_toascii](https://github.com/gagolews/stringi)
#' (`janitor::make_clean_names()` suggests using
#' `stringi::stri_trans_general(x, id="Any-Latin;Greek-Latin;Latin-ASCII")`),
#' the [Wikipedia article about ASCII](https://en.wikipedia.org/wiki/ASCII).
#'
#' On UTF-8, see the above, and furthermore [intToUtf8()], [utf8ToInt()],
#' [validUTF8()],
#' vignette [utf8::utf8](https://CRAN.R-project.org/package=utf8/vignettes/utf8.html),
#' the section [Non-English text](https://r4ds.hadley.nz/strings.html#sec-other-languages),
#' and the [Wikipedia article about UTF-8](https://en.wikipedia.org/wiki/UTF-8).
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
#' all_names(x = "X.2") # FALSE: name created by read.csv()
#' all_names(x = "X.2", allow_susp = TRUE) # TRUE
#'
#' all_names(x = "e.2") # FALSE: name modified by make.names()
#' all_names(x = "e.2", allow_susp = TRUE) # TRUE
#'
#' x_underscores <- c("abc_def", "ghi", "jk_l")
#' all_names(x = x_underscores, allow_underscores = TRUE) # TRUE
#' all_names(x = x_underscores, allow_underscores = FALSE) # FALSE: underscores.
#'
#' x_dots <- c("abc.def", "..abc..def..", ".", "..", "...", "....")
#' all_names(x = x_dots, allow_onlydots = TRUE) # TRUE
#' all_names(x = x_dots, allow_onlydots = FALSE) # FALSE: onlydots.
#'
#' @export
all_names <- function(x, allow_dupl = FALSE, allow_susp = FALSE,
                      allow_underscores = TRUE, allow_onlydots = FALSE,
                      allow_nonASCII = FALSE) {
  stopifnot(is.null(dim(x)), is_logical(allow_dupl), is_logical(allow_susp),
            is_logical(allow_underscores), is_logical(allow_onlydots),
            is_logical(allow_nonASCII))

  warn_text <- character(0)
  warn_text_dupl <- character(0)
  suggest_make_names <- FALSE
  suggest_transliterate <- FALSE
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

  # Inspired by vctrs:::two_to_three_dots()
  warn_text_onlydots <- character(0)
  if(!allow_onlydots) {
    bool_onlydots <- grepl(pattern = "^\\.+$", x = x)
    if(any(bool_onlydots)) {
      warn_text_onlydots <- paste0(
        "values that consist only of dots: '",
        paste0(x[bool_onlydots], collapse = "', '"), "'")
    }
  }

  bool_NA <- is.na(x)
  if(!allow_nonASCII) {
    x_ASCII <- iconv(x = x, to = "ASCII")
    bool_nonASCII <- !bool_NA & (x != x_ASCII | is.na(x_ASCII))
    if(any(bool_nonASCII)) {
      suggest_transliterate <- TRUE
      warn_text_onlydots <- paste0(
        "values that contain non-ASCII characters: '",
        paste0(x[bool_nonASCII], collapse = "', '"), "'")
    }
  }

  # Notes:
  # - Argument 'unique' of make.names() is 'FALSE' because duplicated names have
  #   been catched above.
  # - Argument 'allow_' Using 'TRUE' for  because values in 'x' containing
  #   underscores have been catched above.
  # - make.names() replaces empty names ('""') with "X", so there is no need to
  #   separately test for these.
  out_make_names <- make.names(x, unique = FALSE, allow_ = TRUE)
  bool_invalid <- x != out_make_names
  # Although make.names() replaces NAs in 'x' with "NA.", equality tests using
  # '==' or '!=' on the NAs in 'x' will still return NA (see section 'Details'
  # in ?'=='). Therefore argument 'na.rm' in any() is set to TRUE to prevent
  # getting NA as condition and '|| any(bool_NA)' is used to catch the NAs.
  # 'bool_NA' has been created above so it could also be used inside the
  # if(!allow_nonASCII) {...} code.
  bool_NA <- is.na(x)
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

  warn_text <- c(warn_text, warn_text_dupl, warn_text_underscores,
                 warn_text_onlydots)

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
        paste0("names that might have been modified by make.names(x, unique = TRUE): '",
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
                        ".\nUse 'x <- make.names(x, unique = TRUE",
                        if(!allow_underscores) {", allow_ = FALSE"},
                        ")' to create unique,\nsyntactically valid names",
                        if(!allow_underscores) {" without underscores"},
                        "!")
  }

  if(suggest_transliterate) {
    warn_text <- paste0(
      warn_text,
      ".\nUse text with valid ASCII (https://en.wikipedia.org/wiki/ASCII),",
      " e.g., by using the 'stringi' package",
      " (https://CRAN.R-project.org/package=stringi) ",
      if(requireNamespace("stringi")) {
        "which is already installed"} else {"after installing it"},
      ": 'make.names(names = stringi::stri_enc_toascii(str = x),",
      " unique = TRUE, allow_ = ", allow_underscores,
      ")'. Alternatively, set 'allow_nonASCII' to 'TRUE'.")
  }

  if(nchar(warn_text) > 0L) {
    warning(x = warn_text)
  }

  chars_ok
}
