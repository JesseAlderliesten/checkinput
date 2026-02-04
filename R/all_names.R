#' Check that names are syntactically valid and unmodified
#'
#' Check that `x` is a character vector that consists of unique, syntactically
#' valid names that do not consist of only dots or of two dots followed by a
#' number, and do not suggest they were modified or automatically created.
#'
#' @param x Vector of names to test.
#' @param allow_underscores `TRUE` or `FALSE`: allow underscores?
#'
#' @details
#' [Duplicated][duplicated] or syntactically invalid names are not allowed by
#' `all_names()` because \R functions are not guaranteed to handle such names
#' correctly. For example, not all operations on [data frames][data.frame()]
#' will preserve duplicated column names, and operations involving syntactically
#' invalid names might, by definition, give undocumented results.
#'
#' [Syntactically valid][make.names] names only consist of letters, numbers,
#' dots and underscores; start with a letter, or with a dot not followed by a
#' number; and are not [reserved] words such as [for] or any of the [NA]s. The
#' definition of *letter* depends on the current [locale][locales]. A
#' conservative check for names that are syntactically valid on all locales
#' would only allow digits and unaccented Latin letters, but that is *not*
#' enforced by `all_names()`.
#'
#' Names that consist of only dots, or consist of two dots followed by a number,
#' are not allowed by `all_names()` (nor by `vctrs::vec_as_names()`): they are
#' listed as [reserved] words even though they are not recognised as
#' syntactically invalid by [make.names()].
#'
#' Suspicious names are not allowed by `all_names()`. A suspicious name contains
#' a pattern that suggests it originally was syntactically invalid and has been
#' *modified* into a syntactically valid name, or was modified to make names
#' [unique][make.unique()]. Such changes usually occur silently, for example
#' when data is read into \R, which is problematic because it cannot reliably be
#' assumed the original column names are present. The identification of
#' suspicious names is partly based on the assumption that names originally did
#' not contain dots, see the first item in the list below.
#'
#' `all_names()` *tries* to recognise modifications made by [make.names()],
#' which is used by [data.frame()], [read.csv()][utils::read.csv()], and
#' `data.table::fread(x, header = TRUE, check.names = TRUE)`; and modifications
#' made by `vctrs::vec_as_names(x, repair = "universal")`, which is used
#' throughout the [tidyverse](https://tidyverse.org/):
#' - modifications to replace invalid characters (i.e., characters that are not
#'   a letter, number, dot or underscore): `make.names()` and
#'   `vctrs::vec_as_names(x, repair = "universal")` replace such characters with
#'   a dot. Their identification is based on the assumption that names
#'   originally did *not* contain dots, which is good practice (base-\R
#'   unfortunately violates that practice, e.g., in [data.frame()]) preventing
#'   names containing a dot from being confused with [methods][UseMethod] used
#'   on [classed objects][is.object].
#' - modifications to make duplicated names unique: `make.names(x, unique = TRUE)`
#'   appends a dot followed by a number;
#'   `vctrs::vec_as_names(x, repair = "universal")` appends three dots followed
#'   by a number. It is *not* checked if a complete sequence of suspicious names
#'   is present, e.g., `a.2` will be flagged as suspicious even if `a` and `a.1`
#'   are absent.
#' - modifications to make [reserved] words valid: `make.names()` appends a dot;
#'   `vctrs::vec_as_names(x, repair = "universal")` prepends a dot.
#' - modifications to make names that did not start with a letter, nor with a dot
#'   not followed by a number, syntactically valid: `make.names()` prepends `X`;
#'   `vctrs::vec_as_names(x, repair = "universal")` prepends one or more dots.
#' - modifications to name unnamed columns: `data.frame()` and
#'   `read.csv(..., header = FALSE)` use pattern `V1`, `V2`, `V3` if a matrix
#'   without column names is converted to a data.frame or data without column
#'   names is read into \R, respectively; `read.csv(..., header = TRUE)` uses
#'   pattern `X`, `X.1`, `X.2`;
#'
#' Names containing underscores (`_`) *are* by default allowed by `all_names()`:
#' `allow_underscores` by default is `TRUE` because names containing underscores
#' are not syntactically invalid. However, setting `allow_underscores` to `FALSE`
#' is useful to check that names do not contain underscores, for example if
#' several names will be concatenated separated by underscores to create an
#' ID-tag.
#'
#' @returns
#' `TRUE` or `FALSE`, indicating if `x` is a character vector that consists of
#' unique, syntactically valid names that do not consist of only dots or of two
#' dots followed by a number, and do not suggest they were modified or
#' automatically created.
#'
#' @section Programming note:
#' The patterns used to identify suspicious names are created using
#' [regular expressions][base::regex] with the following elements:
#' - require a pattern to start at the beginning of a string (`^`) or reach the
#'   end of a string (`$`);
#' - specify characters that should be present: a dot (`\\.`, `[.]`, or, if
#'   `fixed` is `TRUE`, `.`), an underscore (`_`), any digit (`[0-9]`), digits
#'   one to nine (`[1-9]`), characters `V` or `X`);
#' - indicate presence: present zero or more times (`*`); present one or more
#'   times (`+`).
#'
#' Multiple patterns can be combined using `|`, the normal operator indicating
#' [logical OR][|].
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
#' `janitor::make_clean_names()` to *modify* names, e.g., through adjusting case
#' and transliterating non-ASCII characters; [names()] to get or set the names
#' of an object.
#'
#' The vignettes about [design choices](../doc/design_choices.html) and about
#' [type coercion](../doc/type_coercion.html).
#'
#' @family
#' collections of checks on type and length
#'
#' @examples
#' all_names(x = c("a", "b1a")) # TRUE
#' all_names(x = c("a", "b1a", "a")) # FALSE: duplicated name
#'
#' # Syntactically invalid names: the character '#' makes names invalid, '""' is
#' # an empty name, 'for' is a reserved word, '..' consists of only dots, and
#' # '..23' consists of two dots followed by a number, which is a reserved
#' # pattern.
#' all_names(x = c("a", "ab#cd", "", "for", "..", "..23")) # FALSE
#'
#' # FALSE: suspicious names
#' all_names(x = c("e.2", ".TRUE", "..22c", "a...2",
#'                 "V3", "X.2", "X0...11", "X0.3", "X3"))
#'
#' all_names(x = "abc_def", allow_underscores = FALSE) # FALSE: underscores
#' all_names(x = "abc_def", allow_underscores = TRUE) # TRUE (is the default)
#'
#' # pass names() or colnames() used on an object without (column) names to
#' # all_names()
#' all_names(x = names(1:3)) # FALSE
#'
#' all_names(13) # FALSE: 'x' is not a character vector
#'
#' @export
all_names <- function(x, allow_underscores = TRUE) {
  stopifnot(is_logical(allow_underscores))

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
        "'x' is NULL: did you pass names() or colnames() used on an object",
        "  without (column) names to all_names()?")
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

  # Names that consist of only dots, or consist of two dots followed by only
  # digits (i.e., pattern '..1', '..2', '..3' etc), are reserved words. They are
  # considered invalid by vctrs::vec_as_names(x, repair = "universal") but not
  # by make.names().
  warn_text_dots <- character(0)
  bool_onlydots <- grepl(pattern = "^\\.+$", x = x)
  if(any(bool_onlydots)) {
    warn_text_dots <- paste0(
      "consist of only dots (but make.names() will not fix that): ",
      paste_quoted(x[bool_onlydots]))
    x <- x[!bool_onlydots]
  }

  # The code is taken from vctrs:::two_to_three_dots().
  bool_patterndots <- grepl(pattern = "^[.][.][1-9][0-9]*$", x = x)
  if(any(bool_patterndots)) {
    warn_text_dots <- c(
      warn_text_dots,
      paste0("consist of two dots followed by digits, which is a reserved",
             " pattern (but make.names() will not fix that): ",
             paste_quoted(x[bool_patterndots])))
    x <- x[!bool_patterndots]
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

  # Names are suspicious if they contain a dot, consist of 'V' followed by a
  # number; or start with an X followed by a name that would be syntactically
  # invalid on its own, see 'Details'.
  bool_suspicious <- grepl(pattern = "^V[1-9][0-9]*$", x = x) |
    grepl(pattern = ".", x = x, fixed = TRUE)
  bool_X <- grepl(pattern = "^X", x = x)
  if(any(bool_X)) {
    x_trailing <- substring(text = x, first = 2L)
    bool_X[x_trailing == make.names(x_trailing)] <- FALSE
    bool_suspicious[bool_X] <- TRUE
  }

  if(any(bool_suspicious)) {
    warn_text <- c(warn_text,
                   paste0("are suspicious: ",
                          paste_quoted(x[bool_suspicious])))
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
