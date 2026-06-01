#' Check that `x` is a valid path
#'
#' Check that `x` is a valid path, possibly containing a valid filename.
#'
#' @param path [character string][is_character()] with the path, possibly
#' containing a valid filename.
#'
#' @details
#' `is_path()` is intended to be used to check for valid paths before creating a
#' directory or a file. Therefore it imposes the following restrictions:
#'
#' - `path` should **not** contain the characters `"`, `*`, `?`, `|`, `<`, `>`,
#'   nor any of the control characters (`ASCII` octal codes 000 through 037 and
#'   177, see `help("regex")`).
#' - `path` components (i.e., parts separated by file separators `/` or `\\`)
#'   should **not** be the Windows-reserved terms `CON`, `PRN`, `AUX`, `NUL`,
#'   `COM<non-zero digit>`, `LPT<non-zero digit>`, case-insensitive variants of
#'   these names, or these names followed by an extension.
#' - `path` components should **not** end with a space.
#' - `path` components should **not** end with a dot, with the exception of
#'   `"."` and `".."` that are allowed as first component to indicate the
#'   working directory and the parent directory, respectively.
#' - `path` should not point to `tempdir()`: a temporary subdirectory should be
#'   used instead (see `progutils::create_tempdir()`).
#' - If `path` contains a file extension (or compression extension, the current
#'   implementation does not distinguish those from each other), the part after
#'   the last slash is considered the filename, which **should** adhere to the
#'   restrictions listed above, and in addition should **not** contain `:`
#'   **nor** start with a space or a hyphen (`-`), while it **might** contain
#'   the Windows-reserved terms given in the second point above.
#'
#' These restrictions `path` consider characters and words that are not allowed
#' in Windows and thus would lead to an error when used to create a directory or
#' file; and characters that are silently removed in Windows and thus would lead
#' to a mismatch between the created directory and the returned path when used
#' to create a directory.
#'
#' `is_path()` allows some patterns that will not occur in real (i.e., existing)
#' paths or filenames:
#'
#' - `path` does **not** have to contain a file separator (i.e., `/` or `\\`).
#'   This makes it possible to use `is_path()` to check that input to
#'   [fs::path()] only contains allowed characters.
#' - `path` does **not** have to point to an existing directory (see the
#'   previous point).
#' - `path` might contain repeated file separators (e.g., `//` or `\\\\`): these
#'   will be treated as if they were only a single file separator.
#' - `path` might contain trailing file separators, even though these might be
#'   ignored or removed in some operations (e.g., they are removed by [file.path()]
#'   and [fs::path()])
#'   .
#' @returns
#' `TRUE` or `FALSE` indicating if `path` is a valid path, possibly containing a
#' valid filename.
#'
#' @section Programming notes:
#' The file separator is a backslash (`\`) on Windows but a forward slash (`/`)
#' on other operating systems ([.Platform$file.sep][.Platform] gives the file
#' separator used on the current platform). Furthermore, the backslash is used
#' as [escape character][regex] in \R, such that backslashes need to be escaped
#' in \R code. Thus, a check on the presence of repeated slashes and backslashes
#' in [string][is_character()] `string` would use
#' `grepl(pattern = "//", x = string, fixed = TRUE)` and
#' `grepl(pattern = "\\\\", x = string, fixed = TRUE)`. The message to point out
#' their presence would be written as `message("Repeated '/' or '\\'")` which
#' would be printed as `Repeated '/' or '\'`. This makes it cumbersome to get
#' the correct type and number of slashes to compare with the path recorded in a
#' message, such that it is more robust to check only for fixed parts of the
#' message (e.g., `"Repeated"`), possibly followed by a check like
#' `tinytest::expect_true(dir.exists(string))`.
#'
#' On MacOS, the output of `tempdir()` is preceded by duplicated forward slashes
#' (e.g., `/var/[...]/T//RtmpxC2Fyl/working_dir/RtmpdnqgUR`) which led to
#' spurious warnings in earlier versions of `is_path()` that warned about
#' duplicated file separators.
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
#' [fs::path_math] for various operations on paths; [fs::path_sanitize()] to
#' **remove** invalid characters from potential paths;
#' [utils::file_test()] and references there on file existence and permissions;
#' `progutils::create_file_path()` to create a file path, creating the directory
#' if it does not yet exist; `progutils::create_dir()` to create a directory if
#' it does not yet exist; `progutils::get_file_path()` to check if a file exists
#' and is a unique match to a pattern.
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
#' is_path(fs::path_wd("ab:cd.txt"))
#' is_path(fs::path_wd("ab|cd.txt"))
#'
#' @export
is_path <- function(path) {
  path_ok <- TRUE
  path_name <- paste0("'path' (", paste_quoted(deparse(substitute(path))), ")")

  if(!is_character(path)) {
    path_ok <- FALSE
  }

  # Notes:
  # - split = c("/", "\\") does not work because that recycles 'split' along 'x'
  # - fs::path_split() does not work because it tidies the path using
  #   fs::path_tidy() before splitting, which removes repeated slashes
  # - The if-else construct is needed because strsplit() discards empty quotes
  #   in the input.
  path_comp <- unlist(strsplit(x = path, split = "/", fixed = TRUE))
  if(any(!nzchar(path_comp))) {
    path_comp <- c(unlist(strsplit(x = path_comp, split = "\\", fixed = TRUE)), "")
  } else {
    path_comp <- unlist(strsplit(x = path_comp, split = "\\", fixed = TRUE))
  }

  if(grepl(pattern = '["*?|<>]', x = path)) {
    path_ok <- FALSE
    warning(path_name, " should not contain '\"', '*', '?', '|', '<' or '>':\n",
            path)
  }

  # "[[:cntrl:]]" matches the control characters, see help("regex")
  if(grepl(pattern = "[[:cntrl:]]", x = path)) {
    path_ok <- FALSE
    warning(path_name, " should not contain control characters:\n", path)
  }

  Windows_reserved <- c("aux", paste0("com", 1:9), "con", paste0("lpt", 1:9),
                        "nul", "prn")
  if(any(Windows_reserved %in% tolower(path_comp))) {
    path_ok <- FALSE
    reserved_comp <- path_comp[tolower(path_comp) %in% Windows_reserved]
    warning("Components of ", path_name,
            " should not contain Windows-reserved names (",
            paste_quoted(reserved_comp), "):\n", path)
  }

  bool_path_dot <- endsWith(x = path_comp, suffix = ".")
  # "." and ".." are allowed as first path component to denote the working
  # directory and the parent directory, respectively
  if(bool_path_dot[1] && path_comp[1] %in% c(".", "..")) {
    bool_path_dot[1] <- FALSE
  }
  if(any(bool_path_dot | endsWith(x = path_comp, suffix = " "))) {
    path_ok <- FALSE
    warning("Components of ", path_name,
            " should not end with ' ' or '.' (i.e., a space or a dot):\n",
            path)
  }

  filename <- basename(path)
  file_ext <- fs::path_ext(path = filename)
  filename_no_ext <- fs::path_ext_remove(path = filename)

  filename_dot <- !(filename %in% c(".", "..")) &&
    # To not get spurious warnings about filenames if it is a path component
    filename != filename_no_ext &&
    (endsWith(filename_no_ext, suffix = ".") ||
       # To catch case where filename ends in a dot, e.g., "ff..txt": modified
       # from fs::path_ext_remove() to only remove a single dot
       endsWith(sub("\\.([^.]+)$", "", filename, perl = TRUE), suffix = "."))

  if(!filename_dot && (length(file_ext) == 0L || !nzchar(file_ext))) {
    to_tempdir <-
      basename(normalizePath(path, winslash = "/", mustWork = FALSE)) ==
      basename(normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
  } else {
    to_tempdir <-
      basename(dirname(normalizePath(path, winslash = "/", mustWork = FALSE))) ==
      basename(normalizePath(tempdir(), winslash = "/", mustWork = FALSE))

    if(length(filename_no_ext) == 0L || !nzchar(filename_no_ext)) {
      path_ok <- FALSE
      warning("filename without extension (", paste_quoted(filename_no_ext),
              ") should not be empty:\n", path)
    }

    if(startsWith(x = filename, prefix = " ") ||
       startsWith(x = filename, prefix = "-")) {
      path_ok <- FALSE
      warning("'filename' should not start with ' ' (i.e., a space) or '-':\n",
              filename)
    }

    if(grepl(pattern = ":", x = filename, fixed = TRUE)) {
      path_ok <- FALSE
      warning("'filename' should not contain ':':\n", filename)
    }

    if(endsWith(x = filename_no_ext, suffix = " ") ||
       endsWith(x = filename_no_ext, suffix = ".") ||
       filename_dot) {
      path_ok <- FALSE
      warning("'filename' should not end with ' ' or '.' (i.e., a space or a",
              " dot):\n", filename)
    }
  }

  if(to_tempdir) {
    path_ok <- FALSE
    warning(paste0(
      "'path' should not point to 'tempdir()': instead, point to a subdirectory",
      " in\ntempdir() through 'fs::path(tempdir(), \"subdir\")', or create such",
      " a subdirectory\nthrough 'progutils::create_tempdir(subdir = \"subdir\")':\n",
      path))
  }

  path_ok
}
