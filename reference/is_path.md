# Check that `x` is a valid path

Check that `x` is a valid path, possibly containing a valid filename.

## Usage

``` r
is_path(path)
```

## Arguments

- path:

  [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.md)
  with the path, possibly containing a valid filename.

## Value

`TRUE` or `FALSE` indicating if `path` is a valid path, possibly
containing a valid filename.

## Details

`is_path()` is intended to be used to check for valid paths before
creating a directory or a file. Therefore it imposes the following
restrictions:

- `path` should **not** contain the characters `"`, `*`, `?`, `|`, `<`,
  `>`, nor any of the control characters (`ASCII` octal codes 000
  through 037 and 177, see
  [`help("regex")`](https://rdrr.io/r/base/regex.html)).

- `path` components (i.e., parts separated by file separators `/` or
  `\\`) should **not** be the Windows-reserved terms `CON`, `PRN`,
  `AUX`, `NUL`, `COM<non-zero digit>`, `LPT<non-zero digit>`,
  case-insensitive variants of these names, or these names followed by
  an extension.

- `path` components should **not** end with a space.

- `path` components should **not** end with a dot, with the exception of
  `"."` and `".."` that are allowed as first component to indicate the
  working directory and the parent directory, respectively.

- `path` should not point to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html): a temporary
  subdirectory should be used instead (see
  `progutils::create_tempdir()`).

- If `path` contains a file extension (or compression extension, the
  current implementation does not distinguish those from each other),
  the part after the last slash is considered the filename, which
  **should** adhere to the restrictions listed above, and in addition
  should **not** contain `:` **nor** start with a space or a hyphen
  (`-`), while it **might** contain the Windows-reserved terms given in
  the second point above.

These restrictions `path` consider characters and words that are not
allowed in Windows and thus would lead to an error when used to create a
directory or file; and characters that are silently removed in Windows
and thus would lead to a mismatch between the created directory and the
returned path when used to create a directory.

`is_path()` allows some patterns that will not occur in real (i.e.,
existing) paths or filenames:

- `path` does **not** have to contain a file separator (i.e., `/` or
  `\\`). This makes it possible to use `is_path()` to check that input
  to [`fs::path()`](https://fs.r-lib.org/reference/path.html) only
  contains allowed characters.

- `path` does **not** have to point to an existing directory (see the
  previous point).

- `path` might contain repeated file separators (e.g., `//` or `\\\\`):
  these will be treated as if they were only a single file separator.

- `path` might contain trailing file separators, even though these might
  be ignored or removed in some operations (e.g., they are removed by
  [`file.path()`](https://rdrr.io/r/base/file.path.html) and
  [`fs::path()`](https://fs.r-lib.org/reference/path.html)) .

## Programming notes

The file separator is a backslash (`\`) on Windows but a forward slash
(`/`) on other operating systems
([.Platform\$file.sep](https://rdrr.io/r/base/Platform.html) gives the
file separator used on the current platform). Furthermore, the backslash
is used as [escape character](https://rdrr.io/r/base/regex.html) in R,
such that backslashes need to be escaped in R code. Thus, a check on the
presence of repeated slashes and backslashes in
[string](https://jessealderliesten.github.io/checkinput/reference/all_characters.md)
`string` would use `grepl(pattern = "//", x = string, fixed = TRUE)` and
`grepl(pattern = "\\\\", x = string, fixed = TRUE)`. The message to
point out their presence would be written as
`message("Repeated '/' or '\\'")` which would be printed as
`Repeated '/' or '\'`. This makes it cumbersome to get the correct type
and number of slashes to compare with the path recorded in a message,
such that it is more robust to check only for fixed parts of the message
(e.g., `"Repeated"`), possibly followed by a check like
`tinytest::expect_true(dir.exists(string))`.

On MacOS, the output of
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) is preceded by
duplicated forward slashes (e.g.,
`/var/[...]/T//RtmpxC2Fyl/working_dir/RtmpdnqgUR`) which led to spurious
warnings in earlier versions of `is_path()` that warned about duplicated
file separators.

## References

- Naming files, paths, and namespaces from
  [Microsoft](https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file)

- Entries
  [Filename](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap03.html#tag_03_146),
  [Portable
  filenames](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap03.html#tag_03_264)
  and
  [Pathname](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap03.html#tag_03_254)
  from the Posix standard
  [POSIX.1-2024](https://pubs.opengroup.org/onlinepubs/9799919799/)

- Comparison of file systems from
  [Wikipedia](https://en.wikipedia.org/wiki/Comparison_of_file_systems#Limits)

## See also

[fs::path_math](https://fs.r-lib.org/reference/path_math.html) for
various operations on paths;
[`fs::path_sanitize()`](https://fs.r-lib.org/reference/path_sanitize.html)
to **remove** invalid characters from potential paths;
[`utils::file_test()`](https://rdrr.io/r/utils/filetest.html) and
references there on file existence and permissions;
`progutils::create_file_path()` to create a file path, creating the
directory if it does not yet exist; `progutils::create_dir()` to create
a directory if it does not yet exist; `progutils::get_file_path()` to
check if a file exists and is a unique match to a pattern.

Other collections of checks on type and length:
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
[`is_zerolength()`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)

## Examples

``` r
is_path(getwd())
#> [1] TRUE
is_path(fs::path_wd("abcd"))
#> [1] TRUE
is_path(fs::path_wd("ab|cd"))
#> Warning: 'fs::path_wd("ab|cd")' should not contain '"', '*', '?', '|', '<' or '>':
#> /home/runner/work/checkinput/checkinput/docs/reference/ab|cd
#> [1] FALSE

is_path(fs::path_wd("abcd.txt"))
#> [1] TRUE
is_path(fs::path_wd("abcd.txt.gz"))
#> [1] TRUE
is_path(fs::path_wd("abcd.gz"))
#> [1] TRUE

is_path(fs::path_wd("ab:cd.txt"))
#> Warning: 'filename' should not contain ':':
#> ab:cd.txt
#> [1] FALSE
is_path(fs::path_wd("ab|cd.txt"))
#> Warning: 'fs::path_wd("ab|cd.txt")' should not contain '"', '*', '?', '|', '<' or '>':
#> /home/runner/work/checkinput/checkinput/docs/reference/ab|cd.txt
#> [1] FALSE
```
