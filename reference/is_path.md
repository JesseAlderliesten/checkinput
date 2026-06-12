# Check that `x` is a valid path

Check that `x` is a valid path, possibly containing a valid filename.

## Usage

``` r
is_path(x, require_sep = TRUE)
```

## Arguments

- x:

  object to check.

- require_sep:

  should `x` include a file separator? Ignored if `x` is `"."` or
  `".."`.

## Value

`TRUE` or `FALSE` indicating if `x` is a valid path, possibly containing
a valid filename.

## Details

`is_path()` is intended to be used to check for valid paths before
creating a directory or a file. Therefore it imposes the following
restrictions:

- `x` should be a non-empty [character
  string](https://jessealderliesten.github.io/checkinput/reference/all_characters.md).

- `x` should **not** contain the characters `"`, `*`, `?`, `|`, `<`,
  `>`, nor any of the control characters (`[:cntrl:]`, with `ASCII`
  octal codes 000 through 037 and 177, see
  [`help("regex")`](https://rdrr.io/r/base/regex.html)). Although colons
  (`:`) outside a filename are allowed by `is_path()`, Windows will only
  allow colons to indicate volume names like `C:\`.

- path components (i.e., parts separated by file separators `/` or `\\`)
  should **not** be the Windows-reserved terms `CON`, `PRN`, `AUX`,
  `NUL`, `COM<non-zero digit>`, `LPT<non-zero digit>`, case-insensitive
  variants of these names, or these names followed by an extension.

- path components should **not** end with a space.

- path components should **not** end with a dot, with the exception of
  `"."` and `".."` that are allowed as first component to indicate the
  [working directory](https://rdrr.io/r/base/getwd.html) and the parent
  directory, respectively.

- If `x` contains a file extension (or compression extension, the
  current implementation does not distinguish those from each other),
  the part after the last slash is considered the filename, which
  **should** adhere to the restrictions listed above (although it
  **may** contain the Windows-reserved terms listed above), and in
  addition should **not** contain a colon (`:`) **nor** start with a
  space or a hyphen (`-`).

These restrictions on `x` consider characters and path components that
are not allowed in Windows and thus would lead to an error when used to
create a directory or file; characters that are silently removed in
Windows and thus would lead to a mismatch between the created directory
and the returned path when used to create a directory; and characters
that might give problems when used in the shell.

`is_path()` is lenient with respect to file separators (i.e., `/` or
`\\`):

- `x` does **not** have to contain any file separator if `require_sep`
  is `FALSE`, such that `is_path(x, require_sep = FALSE)` can be used to
  check that filenames only contain allowed characters (given that `x`
  contains a file extension).

- `x` might contain trailing file separators, although these might be
  ignored or removed in some operations (e.g., they are removed by
  [`file.path()`](https://rdrr.io/r/base/file.path.html) and
  [`fs::path()`](https://fs.r-lib.org/reference/path.html)).

- `x` might contain successive file separators (e.g., `//` or `\\\\`):
  these should be treated by the operating system as if they were only a
  single file separator.

`is_path()` does **not** check that the path in `x` points to an
existing file or folder, **nor** that such a file or folder can be
created.

## Notes on paths

The file separator is a backslash (`\`) on Windows but a forward slash
(`/`) on other operating systems:
[.Platform\$file.sep](https://rdrr.io/r/base/Platform.html) gives the
file separator used on the current platform. Furthermore, the backslash
is used as [escape character](https://rdrr.io/r/base/regex.html) in R,
such that backslashes need to be escaped in R code by doubling them. Use
`cat(x)` to omit the escape-characters to see how `x` would be printed.

## Programming notes

The output of [`tempdir()`](https://rdrr.io/r/base/tempfile.html) during
[R CMD checks](https://r-pkgs.org/R-CMD-check.html) on MacOS contains
successive forward slashes (e.g.,
`/var/[...]/T//RtmpxC2Fyl/working_dir/RtmpdnqgUR`) which in earlier
versions of `is_path()` (then in package
[progutils](https://jessealderliesten.github.io/progutils/)) led to
spurious warnings about duplicated file separators.

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

[`fs::path_math()`](https://fs.r-lib.org/reference/path_math.html) for
various operations on paths;
[`fs::path_sanitize()`](https://fs.r-lib.org/reference/path_sanitize.html)
to **remove** invalid characters from potential paths;
[`utils::file_test()`](https://rdrr.io/r/utils/filetest.html) and
references there on checking file existence and permissions;
[`progutils::create_file_path()`](https://jessealderliesten.github.io/progutils/reference/create_file_path.html)
to create a file path, creating the directory if it does not yet exist;
[`progutils::create_dir()`](https://jessealderliesten.github.io/progutils/reference/create_dir.html)
to create a directory if it does not yet exist;
[`progutils::get_file_path()`](https://jessealderliesten.github.io/progutils/reference/get_file_path.html)
to check if a file exists and is a unique match to a pattern.

Section 'Paths in the shell' in the vignette *Git and GitHub* of package
`checkrpkgs` (`vignette("git_github", package = "checkrpkgs")`) on paths
and file separators in the [shell](https://happygitwithr.com/shell).

Other collections of checks on type and length:
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
[`is_zerolength()`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)

## Examples

``` r
is_path(getwd()) # TRUE
#> [1] TRUE
is_path(fs::path_wd("abcd")) # TRUE
#> [1] TRUE
is_path(fs::path_wd("ab|cd")) # FALSE, warning about '|'
#> Warning: 'fs::path_wd("ab|cd")' should not contain '"', '*', '?', '|', '<' or '>':
#> /home/runner/work/checkinput/checkinput/docs/reference/ab|cd
#> [1] FALSE

is_path(fs::path_wd("abcd.txt")) # TRUE
#> [1] TRUE
is_path(fs::path_wd("abcd.txt.gz")) # TRUE
#> [1] TRUE
is_path(fs::path_wd("abcd.gz")) # TRUE
#> [1] TRUE

# ':' is allowed in paths but not in filenames
is_path(fs::path_wd("ab:cd")) # TRUE
#> [1] TRUE
is_path(fs::path_wd("ab:cd.txt")) # FALSE, warning about ':'
#> Warning: The filename ('ab:cd.txt') in 'fs::path_wd("ab:cd.txt")' should not contain ':':
#> /home/runner/work/checkinput/checkinput/docs/reference/ab:cd.txt
#> [1] FALSE

# Other illegal characters are not allowed in either paths or filenames
is_path(fs::path_wd("ab|cd")) # FALSE, warning about '|'
#> Warning: 'fs::path_wd("ab|cd")' should not contain '"', '*', '?', '|', '<' or '>':
#> /home/runner/work/checkinput/checkinput/docs/reference/ab|cd
#> [1] FALSE
is_path(fs::path_wd("ab|cd.txt")) # FALSE, warning about '|'
#> Warning: 'fs::path_wd("ab|cd.txt")' should not contain '"', '*', '?', '|', '<' or '>':
#> /home/runner/work/checkinput/checkinput/docs/reference/ab|cd.txt
#> [1] FALSE
```
