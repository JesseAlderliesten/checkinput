#' Check that x contains syntactically valid names
#'
#' Check that `x` is a character vector that consists of only unique,
#' syntactically valid names that do not consists of only dots and do not
#' suggest they were modified or automatically created.
#'
#' @param x Vector of names to test.
#' @param allow_suspicious `TRUE` or `FALSE`: allow suspicious names? A
#' suspicious name contains a pattern that suggests it originally was
#' syntactically invalid and has been *modified* into a syntactically valid name.
#' @param allow_underscores `TRUE` or `FALSE`: allow underscores?
#'
#' @details
#' [Duplicated][duplicated] or syntactically invalid names are not allowed by
#' `all_names()` because \R functions are not guaranteed to handle such names
#' correctly. For example, not all operations on [data frames][data.frame()]
#' will preserve duplicated column names, and operations involving syntactically
#' invalid names might, by definition, give undocumented results. In addition,
#' although names that consist of only dots are syntactically valid, such names
#' are not allowed by `all_names()` because such names are not informative and
#' easy to confuse.
#'
#' [Syntactically valid][make.names] names only consist of letters, numbers,
#' dots and underscores; start with a letter, or with a dot not followed by a
#' number; and are not [reserved words][Reserved] such as [for] or any of the
#' [NA]s. The definition of *letter* depends on the current [locale][locales].
#' A conservative check for names that are syntactically valid on all locales
#' would be to only allow digits and unaccented Latin letters, but that is *not*
#' enforced by `all_names()`.
#'
#' Names containing underscores (`_`) are not allowed by `all_names()` if
#' `allow_underscores` is `FALSE`. That is *not* the default, because underscores
#' do not make names syntactically invalid or uninformative. Setting
#' `allow_underscores` to `FALSE` is useful to check names that will be
#' concatenated by underscores later on, for example to create an ID-tag.
#'
#' Suspicious names are not allowed by `all_names()` if `allow_suspicious` is
#' `FALSE`. A suspicious name contains a pattern that suggests it originally was
#' syntactically invalid and has been *modified* into a syntactically valid name,
#' for example when data was read into \R, which usually occurs silently. That
#' is a problem because then it cannot be reliably assumed the original column
#' names are present.
#'
#' `all_names()` *tries* to recognise changes made by [make.names()], which is
#' used by [data.frame()], [read.csv()][utils::read.csv()], and
#' `data.table::fread(x, header = TRUE, check.names = TRUE)`; and changes made
#' by `vctrs::vec_as_names(x, repair = "universal")`, which is used throughout
#' the [tidyverse](https://tidyverse.org/):
#' - changes to make duplicated names unique: `make.names(x, unique = TRUE)`
#'   appends a dot followed by a number;
#'   `vctrs::vec_as_names(x, repair = "universal")` appends three dots followed
#'   by a number.
#' - changes to make names that did not start with a letter, nor with a dot not
#'   followed by a number, syntactically valid: `make.names()` prepends `X`;
#'   `vctrs::vec_as_names(x, repair = "universal")` prepends one or more dots.
#' - changes to make [reserved][Reserved] words valid: `make.names()` appends a
#'   dot; `vctrs::vec_as_names(x, repair = "universal")` prepends a dot. Names
#'   that consist of only dots, or consist of two dots followed by only digits,
#'   are considered invalid by `vctrs::vec_as_names()` but not by `make.names()`.
#' - changes to name unnamed columns: `data.frame()` and
#'   `read.csv(..., header = FALSE)` use pattern `V1`, `V2`, `V3`;
#'   `read.csv(..., header = TRUE)` uses pattern `X`, `X.1`, `X.2`.
#'
#' It is not always possible to unambiguously determine which changes have been
#' made: `"X.1"` can arise if `make.names()` modifies the invalid name `.1` but
#' also as the name created by `read.csv()` for a second unnamed column (both
#' options are suggested by `all_names()`). Furthermore, `all_names()` does
#' *not* recognise replacement of invalid characters with dots because that
#' would require assuming that names originally did not contain dots. Therefore,
#' even though `make.names(<invalid name>)` should produce a suspicious name,
#' that is not always recognised as such by `all_names()`, e.g., `make.names()`
#' modifies `c("a-b", "ab#cd", "c/d")` to `c("a.b", "ab.cd", "c.d")` which are
#' not identified as suspicious names by `all_names()`.
#'
#' In addition, it is *not* checked if a complete sequence of suspicious names
#' is present in `x`, e.g., `X.2` will be flagged as suspicious even if `X` and
#' `X.1` are absent.
#'
#' @returns
#' `TRUE` or `FALSE`, indicating if `x` is a character vector that only contains
#' unique, syntactically valid names that do not consist of only dots and do not
#' suggest they were modified or automatically created.
#'
#' @section Programming note:
#' The patterns used to identify suspicious names are created using
#' [regular expressions][base::regex] with the following elements:
#' - require a pattern to start at the beginning of a string (`^`) or reach the
#'   end of a string (`$`);
#' - specify characters that should be present: any character (`.`), a dot (`\\.`
#'   or `[.]`), an underscore (`_`), any digit (`[0-9]` or `[:digit:]`), digits
#'   one to nine (`[1-9]`), `X`, `V`);
#' - indicate presence: absent (`^`); absent, or present one or more times (`*`);
#'   present one or more times (`+`);
#' - match any of two patterns: `|` (the normal operator indicating
#'   [logical OR][|]).
#'
#' @seealso
#' Section `Details` of [make.names()], section `Names and Identifiers` of
#' [Quotes], and the [\R FAQ about valid names](
#' https://CRAN.R-project.org/doc/manuals/R-FAQ.html#What-are-valid-names_003f)
#' on the syntactical validity of names.
#'
#' [locales] and [Encoding] on encodings and character sets; [validUTF8()] to
#' check for valid encodings; [iconv()] to convert between encodings;
#' `tools::showNonASCII()` to show the non-ASCII bytes of strings; package
#' [stringi](https://CRAN.R-project.org/package=stringi) that provides
#' facilities to process character strings.
#'
#' `janitor::make_clean_names()` to *change* names, e.g., through adjusting case
#' and transliterating non-ASCII characters; `rlang::names_inform_repair()` for
#' a method to report name changes if old and new names are provided; [names()]
#' to get or set the names of an object.
#'
#' The vignettes about [design choices](../doc/design_choices.html) and about
#' [type coercion](../doc/type_coercion.html).
#'
#' @family
#' collections of checks on type and length
#'
#' @examples
#' all_names(x = c("a", "b.1a")) # TRUE
#' all_names(x = c("a", "b.1a", "a")) # FALSE: duplicated name
#'
#' # '#' makes names invalid, '""' is an empty name, 'for' is a reserved word,
#' # and '..' consists of only dots.
#' all_names(x = c("a", "ab#cd", "", "for", "..")) # FALSE
#' all_names(x = NULL) # FALSE: NULL
#'
#' all_names(x = "V3") # FALSE: created by read.csv() or data.frame()
#' all_names(x = "V3", allow_suspicious = TRUE) # TRUE
#'
#' all_names(x = "X3") # FALSE: modified by make.names(x, unique = FALSE)
#' all_names(x = "X3", allow_suspicious = TRUE) # TRUE
#'
#' all_names(x = "e.2") # FALSE: modified by make.names(x, unique = TRUE)
#' all_names(x = "e.2", allow_suspicious = TRUE) # TRUE
#'
#' # modified by make.names(x, unique = FALSE) or make.names(x, unique = TRUE)
#' all_names(x = "X.2.1") # FALSE
#' all_names(x = "X.2.1", allow_suspicious = TRUE) # TRUE
#'
#' # modified by make.names(x, unique = FALSE), make.names(x, unique = TRUE),
#' # or created by read.csv()
#' all_names(x = "X.2") # FALSE
#' all_names(x = "X.2", allow_suspicious = TRUE) # TRUE
#'
#' all_names(x = c("..22c", ".TRUE")) # FALSE: modified by vctrs::vec_as_names(x)
#' all_names(x = c("..22c", ".TRUE"), allow_suspicious = TRUE) # TRUE
#'
#' # modified by make.names(x, unique = FALSE), make.names(x, unique = TRUE),
#' # or vctrs::vec_as_names()
#' all_names(x = "X0...11") # FALSE
#' all_names(x = "X0...11", allow_suspicious = TRUE) # TRUE
#'
#' # modified by make.names(x, unique = TRUE) or by vctrs::vec_as_names()
#' all_names(x = c(".if...4", "a...2")) # FALSE
#' all_names(x = c(".if...4", "a...2"), allow_suspicious = TRUE) # TRUE
#'
#' all_names(x = "abc_def", allow_underscores = FALSE) # FALSE: underscores
#' all_names(x = "abc_def", allow_underscores = TRUE) # TRUE (is the default)
#'
#' @export
all_names <- function(x, allow_suspicious = FALSE, allow_underscores = TRUE) {
  stopifnot(is_logical(allow_suspicious), is_logical(allow_underscores))

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
        "'x' is NULL: did you use names(x) on an object without names, or",
        " colnames(x) on\nan object without column names?")
    } else {
      warn_text_zerolength <- "x has length zero but is not NULL"
    }
  }

  bool_dupl <- FALSE
  if(anyDuplicated(x) != 0L) {
    bool_dupl <- duplicated(x)
    warn_text <- c(warn_text,
                   paste0("are duplicated: ", paste_quoted(unique(x[bool_dupl]))))
    suggest_make_names <- TRUE
    x <- x[!bool_dupl]
  }

  # Notes:
  # - Argument 'unique' of make.names() is 'FALSE' because duplicated names have
  #   been catched above.
  # - Argument 'allow_' of make.names() is 'TRUE' because names containing
  #   underscores will be catched below.
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

  # '...' and pattern '..1', '..2', etc are not catched by make.names but are
  # invalid because they are reserved words.
  warn_text_dots <- character(0)
  bool_dots <- grepl(pattern = "^\\.+$", x = x)
  if(any(bool_dots)) {
    warn_text_dots <- paste0(
      "consist of only dots (make.names() will not fix that): ",
      paste_quoted(x[bool_dots]))
  }

  # Names that consist of only dots, or consist of two dots followed by only
  # digits, are considered invalid by vctrs::vec_as_names() but not by
  # make.names(). The code is taken from vctrs:::two_to_three_dots().
  bool_dotdotdigits <- grepl(pattern = "^[.][.][1-9][0-9]*$", x = x)
  if(any(bool_dotdotdigits)) {
    warn_text_dots <- c(
      warn_text_dots,
      paste0("consist of two dots followed by digits, which is a reserved",
             " pattern: ", paste_quoted(x[bool_dotdotdigits])))
  }

  warn_text_underscores <- character(0)
  if(!allow_underscores) {
    bool_underscores <- grepl(pattern = "_", x = x, fixed = TRUE)
    if(any(bool_underscores)) {
      warn_text_underscores <- paste0(
        "contain underscores: ", paste_quoted(x[bool_underscores]))
      suggest_make_names <- TRUE
      # Removing names that contain underscores because they are invalid, and
      # thus not suspicious, if 'allow_underscores' is FALSE.
      x <- x[!bool_underscores]
    }
  }

  warn_text <- c(warn_text, warn_text_underscores, warn_text_dots)

  if(!allow_suspicious) {
    # Notes:
    # - The 'Programming note' explains the structure of the regular expressions.
    # - Names that consist of only dots, or of two dots followed by digits, are
    #   considered invalid instead of suspicious and have already been removed
    #   from 'x' above.

    # read.csv(..., header = FALSE) creates names with pattern `V1`, `V2`, `V3`,
    # etc. to name unnamed columns. Such names are also created by data.frame()
    # if a matrix without column names is converted to a data.frame.
    bool_csv_df <- grepl(pattern = "^V[1-9][0-9]*$", x = x)

    # make.names() prepends 'X' to make names syntactically valid.
    bool_makenm_F_p1 <- grepl(pattern = "^X", x = x)
    if(any(bool_makenm_F_p1)) {
      substr_p1 <- substring(text = x[bool_makenm_F_p1], first = 2L)
      ind_FALSE <- which(bool_makenm_F_p1)[
        substr_p1 == make.names(names = substr_p1)]
      bool_makenm_F_p1[ind_FALSE] <- FALSE
    }

    # make.names() appends a dot to reserved words. No need to check again those
    # names that have been identified as suspicious by bool_makenm_F_p1 above.
    bool_makenm_F_p2 <- grepl(pattern = ".\\.$", x = x) & !bool_makenm_F_p1
    if(any(bool_makenm_F_p2)) {
      substr_p2 <- substr(x = x[bool_makenm_F_p2], start = 1L,
                          stop = nchar(x[bool_makenm_F_p2]) - 1L)
      ind_FALSE <- which(bool_makenm_F_p2)[
        substr_p2 == make.names(names = substr_p2)]
      bool_makenm_F_p2[ind_FALSE] <- FALSE
    }
    bool_makenm_F <- bool_makenm_F_p1 | bool_makenm_F_p2

    # make.names(..., unique = TRUE) appends a dot followed by a number to make
    # duplicated names unique.
    bool_makenm_T <- grepl(pattern = ".\\.[1-9][0-9]*$", x = x)

    bool_makenm <- bool_makenm_F & bool_makenm_T
    if(any(bool_makenm, na.rm = TRUE)) {
      bool_makenm_F[bool_makenm] <- FALSE
      bool_makenm_T[bool_makenm] <- FALSE
    }

    # The suspicious names identified here as possibly created by read.csv()
    # might also have been created by makenm_p1 (but suspicious names possibly
    # created by makenm_p1 cannot always also have been created by read.csv()):
    # read.csv(..., header = TRUE) creates names with pattern `X`, `X.1`, `X.2`,
    # etc. to name unnamed columns; and make.names() prepends 'X' to make
    # syntactically invalid names valid (which for the empty string '""' results
    # in 'X').
    bool_makenm_csv <- grepl(pattern = "^X$|^X\\.[1-9][0-9]*$", x = x)
    if(any(bool_makenm_csv, na.rm = TRUE)) {
      bool_makenm_F[bool_makenm_csv] <- FALSE
      bool_makenm_T[bool_makenm_csv] <- FALSE
      bool_makenm[bool_makenm_csv] <- FALSE
    }

    # vctrs::vec_as_names() prepends one or more dots to make syntactically
    # invalid names valid, and appends three dots followed by a number to make
    # duplicated names unique. It does not produce names that only consist of
    # dots, names that consist of two dots followed by only digits, or names
    # that start with a dot followed by a digit.
    # To do:
    # - Move the boolean vectors to check for these patterns closer together to
    #   prevent repeating e.g., checking for ^\\.+$.
    bool_nodotspattern <- !grepl(pattern = "^\\.+$|^[.][.][1-9][0-9]*$", x = x)
    bool_vecasnm <- bool_nodotspattern & grepl(pattern = "^\\.[^[:digit:]]", x = x)
    if(any(bool_vecasnm)) {
      substr_p1 <- gsub(pattern = "^\\.+", replacement = "", x = x[bool_vecasnm])
      ind_FALSE <- which(bool_vecasnm)[
        substr_p1 == make.names(names = substr_p1)]
      bool_vecasnm[ind_FALSE] <- FALSE
    }
    bool_vecasnm <- bool_vecasnm |
      (bool_nodotspattern & grepl(pattern = "[^.]\\.\\.\\.[1-9][0-9]*$", x = x))

    bool_makenm_vecasnm <- bool_makenm & bool_vecasnm
    if(any(bool_makenm_vecasnm, na.rm = TRUE)) {
      bool_makenm[bool_makenm_vecasnm] <- FALSE
      bool_vecasnm[bool_makenm_vecasnm] <- FALSE
    }

    bool_makenm_T_vecasnm <- bool_makenm_T & bool_vecasnm
    if(any(bool_makenm_T_vecasnm, na.rm = TRUE)) {
      bool_makenm_T[bool_makenm_T_vecasnm] <- FALSE
      bool_vecasnm[bool_makenm_T_vecasnm] <- FALSE
    }

    if(any(bool_csv_df, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been created by read.csv() or data.frame(): ",
               paste_quoted(x[bool_csv_df])))
    }

    if(any(bool_makenm_F, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = FALSE): ",
               paste_quoted(x[bool_makenm_F])))
    }

    if(any(bool_makenm_T, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = TRUE): ",
               paste_quoted(x[bool_makenm_T])))
    }

    if(any(bool_makenm, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = FALSE) or",
               " make.names(x, unique = TRUE): ",
               paste_quoted(x[bool_makenm])))
    }

    if(any(bool_makenm_csv, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = FALSE) or",
               " make.names(x, unique = TRUE), or have been created by read.csv(): ",
               paste_quoted(x[bool_makenm_csv])))
    }

    if(any(bool_vecasnm, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by vctrs::vec_as_names(): ",
               paste_quoted(x[bool_vecasnm])))
    }

    if(any(bool_makenm_vecasnm, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = FALSE),",
               " make.names(x, unique = TRUE), or vctrs::vec_as_names(): ",
               paste_quoted(x[bool_makenm_vecasnm])))
    }

    if(any(bool_makenm_T_vecasnm, na.rm = TRUE)) {
      warn_text <- c(
        warn_text,
        paste0("might have been modified by make.names(x, unique = TRUE) or",
               " vctrs::vec_as_names(): ",
               paste_quoted(x[bool_makenm_T_vecasnm])))
    }
  }

  if(length(warn_text) > 0L) {
    warn_text <- paste0("Names ", paste0(warn_text, collapse = "; and "))
  }

  warn_text <- paste0(c(warn_text_zerolength, warn_text), collapse = " and ")

  if(suggest_make_names) {
    if(allow_underscores) {
      warn_text <- paste0(
        warn_text, ".\nUse 'x <- make.names(x, unique = TRUE)' to create",
        " unique, syntactically valid names!")
    } else {
      warn_text <- paste0(
        warn_text, ".\nUse 'x <- make.names(x, unique = TRUE, allow_ = FALSE)'",
        " to create unique,\nsyntactically valid names without underscores!")
    }
  }

  # Another early return occurs if 'x' is not a character vector and not NULL.
  if(length(warn_text) > 1L || nchar(warn_text) > 0L) {
    chars_ok <- FALSE
    warning(x = warn_text)
    return(FALSE)
  }

  TRUE
}
