#' Check that `x` is a valid path
#'
#' Check that `x` is a valid path, possibly containing a valid filename.
#'
#' @inheritParams is_logical
#'
#' @details
#' `is_path()` is intended to be used to check for valid paths before creating a
#' directory or a file. Therefore it imposes the following restrictions:
#'
#' - `x` should be a [character string][is_character()].
#' - `x` should **not** contain the characters `"`, `*`, `?`, `|`, `<`, `>`, nor
#'   any of the control characters (`ASCII` octal codes 000 through 037 and 177,
#'   see `help("regex")`). Although `:` is allowed by `is_path()` outside a
#'   filename, Windows will only allow colons to indicate volume names like `C:\`.
#' - path components (i.e., parts separated by file separators `/` or `\\`)
#'   should **not** be the Windows-reserved terms `CON`, `PRN`, `AUX`, `NUL`,
#'   `COM<non-zero digit>`, `LPT<non-zero digit>`, case-insensitive variants of
#'   these names, or these names followed by an extension.
#' - path components should **not** end with a space.
#' - path components should **not** end with a dot, with the exception of `"."`
#'   and `".."` that are allowed as first component to indicate the
#'   [working directory][getwd()] and the parent directory, respectively.
#' - If `x` contains a file extension (or compression extension, the current
#'   implementation does not distinguish those from each other), the part after
#'   the last slash is considered the filename, which **should** adhere to the
#'   restrictions listed above (although it **may** contain the Windows-reserved
#'   terms listed in the second point above), and in addition should **not**
#'   contain `:` **nor** start with a space or a hyphen (`-`).
#'
#' These restrictions on `x` consider characters and words that are not allowed
#' in Windows and thus would lead to an error when used to create a directory or
#' file; characters that are silently removed in Windows and thus would lead
#' to a mismatch between the created directory and the returned path when used
#' to create a directory; and characters that might give problems when used in
#' the shell.
#'
#' `is_path()` allows some patterns that will not occur in real (i.e., existing)
#' paths or filenames:
#'
#' - `x` does **not** have to contain a file separator (i.e., `/` or `\\`). This
#'   makes it possible to use `is_path()` to check that input to [fs::path()]
#'   only contains allowed characters.
#' - `x` does **not** have to point to an existing directory (see the previous
#'   point).
#' - `x` might contain repeated file separators (e.g., `//` or `\\\\`): these
#'   will be treated as if they were only a single file separator.
#' - `x` might contain trailing file separators, even though these might be
#'   ignored or removed in some operations (e.g., they are removed by
#'   [file.path()] and [fs::path()])
#'   .
#' @returns
#' `TRUE` or `FALSE` indicating if `x` is a valid path, possibly containing a
#' valid filename.
#'
#' @section Notes on paths:
#' The file separator is a backslash (`\`) on Windows but a forward slash (`/`)
#' on other operating systems ([.Platform$file.sep][.Platform] gives the file
#' separator used on the current platform).
#'
#' Furthermore, the backslash is used
#' as [escape character][regex] in \R, such that backslashes need to be escaped
#' in \R code by doubling them (use `cat(x)` to see how `x` would be printed).
#' Thus, a check on the presence of repeated
#' slashes and backslashes in [string][is_character()] `string` would use
#' `grepl(pattern = "//", x = string, fixed = TRUE)` and
#' `grepl(pattern = "\\\\", x = string, fixed = TRUE)`. The message to point out
#' their presence would be written as `message("Repeated '/' or '\\'")` which
#' would be printed as `Repeated '/' or '\'`. This makes it cumbersome to get
#' the correct type and number of slashes to compare with the path recorded in a
#' message, such that it is more robust to check only for fixed parts of the
#' message (e.g., `"Repeated"`), possibly followed by a check like
#' `tinytest::expect_true(fs::dir_exists(string))`.
#'
#' @section Programming notes:
#' The output of `tempdir()` during R cmd checks on MacOS contains duplicated
#' forward slashes (e.g., `/var/[...]/T//RtmpxC2Fyl/working_dir/RtmpdnqgUR`)
#' which in earlier versions of `is_path()` (then in package `progutils`) led to
#' spurious warnings about duplicated file separators.
#'
#' @section References:
#' - Naming files, paths, and namespaces from
#'   [Microsoft](https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file)
#' - Entries
#'   [Filename](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap03.html#tag_03_146),
#'   [Portable filenames](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap03.html#tag_03_264)
#'   and
#'   [Pathname](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap03.html#tag_03_254)
#'   from the Posix standard
#'   [POSIX.1-2024](https://pubs.opengroup.org/onlinepubs/9799919799/)
#' - Comparison of file systems from
#'   [Wikipedia](https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits)
#'
#' @seealso
#' [fs::path_math()] for various operations on paths; [fs::path_sanitize()] to
#' **remove** invalid characters from potential paths;
#' [utils::file_test()] and references there on file existence and permissions;
#' [progutils::create_file_path()] to create a file path, creating the directory
#' if it does not yet exist; [progutils::create_dir()] to create a directory if
#' it does not yet exist; [progutils::get_file_path()] to check if a file exists
#' and is a unique match to a pattern.
#'
#' Section 'Paths in the shell' in the vignette *Git and GitHub* of package
#' `checkrpkgs`: `vignette("git_github", package = "checkrpkgs")` on paths and
#' file separators in the [shell](https://happygitwithr.com/shell).
#'
#' @family
#' collections of checks on type and length
#'
#' @examples
#' is_path(getwd())
#' is_path(fs::path_wd("abcd"))
#' is_path(fs::path_wd("ab|cd"))
#'
#' is_path(fs::path_wd("abcd.txt"))
#' is_path(fs::path_wd("abcd.txt.gz"))
#' is_path(fs::path_wd("abcd.gz"))
#'
#' # ':' is allowed in paths but not in filenames
#' is_path(fs::path_wd("ab:cd")) # TRUE
#' is_path(fs::path_wd("ab:cd.txt")) # FALSE
#'
#' # Other illegal characters are not allowed in either paths or filenames
#' is_path(fs::path_wd("ab|cd")) # FALSE
#' is_path(fs::path_wd("ab|cd.txt")) # FALSE
#'
#' @export
is_path <- function(x) {
  arg_name <- paste_quoted(deparse1(substitute(x)))

  if(!is_character(x)) {
    warning(arg_name, " should be a non-empty, non-NA_character_ character string:\n", x)
    # Return early for non-character input to prevent spurious errors.
    return(FALSE)
  }

  path_ok <- TRUE

  # Notes:
  # - split = c("/", "\\") does not work because that recycles 'split' along 'x'
  # - fs::path_split() does not work because it tidies the path using
  #   fs::path_tidy() before splitting, which removes repeated slashes
  # - The if-else construct is needed because strsplit() discards empty quotes
  #   in the input.
  path_comp <- unlist(strsplit(x = x, split = "/", fixed = TRUE))
  if(any(!nzchar(path_comp))) {
    path_comp <- c(unlist(strsplit(x = path_comp, split = "\\", fixed = TRUE)), "")
  } else {
    path_comp <- unlist(strsplit(x = path_comp, split = "\\", fixed = TRUE))
  }

  if(grepl(pattern = '["*?|<>]', x = x)) {
    path_ok <- FALSE
    warning(arg_name, " should not contain '\"', '*', '?', '|', '<' or '>':\n",
            x)
  }

  # "[[:cntrl:]]" matches the control characters, see help("regex")
  if(grepl(pattern = "[[:cntrl:]]", x = x)) {
    path_ok <- FALSE
    warning(arg_name, " should not contain control characters:\n", x)
  }

  Windows_reserved <- c("aux", paste0("com", 1:9), "con", paste0("lpt", 1:9),
                        "nul", "prn")
  if(any(Windows_reserved %in% tolower(path_comp))) {
    path_ok <- FALSE
    reserved_comp <- path_comp[tolower(path_comp) %in% Windows_reserved]
    warning("Components of ", arg_name,
            " should not contain Windows-reserved names (",
            paste_quoted(reserved_comp), "):\n", x)
  }

  bool_path_dot <- endsWith(x = path_comp, suffix = ".")
  # "." and ".." are allowed as first path component to denote the working
  # directory and the parent directory, respectively
  if(bool_path_dot[1] && path_comp[1] %in% c(".", "..")) {
    bool_path_dot[1] <- FALSE
  }
  if(any(bool_path_dot | endsWith(x = path_comp, suffix = " "))) {
    path_ok <- FALSE
    warning("Components of ", arg_name,
            " should not end with ' ' or '.' (i.e., a space or a dot):\n", x)
  }

  filename <- basename(x)
  file_ext <- fs::path_ext(path = filename)
  filename_no_ext <- fs::path_ext_remove(path = filename)
  has_file_ext <- (length(file_ext) != 0L && nzchar(file_ext)) ||
    # fs::path() needed because fs::path_ext_remove() tidies the path
    fs::path(filename) != filename_no_ext ||
    # Catch case where filename ends in a dot (e.g., "ff..txt") on Windows with
    # R 4.1.0: modified from fs::path_ext_remove() to only remove a single dot
    grepl(pattern = "\\.([^.]+)$", x = filename, perl = TRUE)

  if(has_file_ext) {
    if(length(filename_no_ext) == 0L || !nzchar(filename_no_ext)) {
      path_ok <- FALSE
      warning("filename without extension (", paste_quoted(filename_no_ext),
              ") should not be empty:\n", x)
    }

    if(startsWith(x = filename, prefix = " ") ||
       startsWith(x = filename, prefix = "-")) {
      path_ok <- FALSE
      warning("The filename (", paste_quoted(filename), ") in ", arg_name,
              " should not start with ' ' (i.e., a space) or '-':\n", x)
    }

    if(grepl(pattern = ":", x = filename, fixed = TRUE)) {
      path_ok <- FALSE
      warning("The filename (", paste_quoted(filename), ") in ", arg_name,
              " should not contain ':':\n", x)
    }

    filename_dot <- !(filename %in% c(".", "..")) &&
      (endsWith(filename_no_ext, suffix = ".") ||
         # To catch case where filename ends in a dot, e.g., "ff..txt": modified
         # from fs::path_ext_remove() to only remove a single dot
         endsWith(sub("\\.([^.]+)$", "", filename, perl = TRUE), suffix = "."))

    if(endsWith(x = filename_no_ext, suffix = " ") ||
       endsWith(x = filename_no_ext, suffix = ".") ||
       filename_dot) {
      path_ok <- FALSE
      warning("The filename (", paste_quoted(filename), ") in ", arg_name,
              " should not end with ' ' or '.' (i.e., a space or a dot):\n", x)
    }
  }

  # An early return (FALSE) occurs for non-character input.
  path_ok
}
