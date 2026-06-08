# checkinput 1.0.0

### Documentation
- Update `NEWS`.


# checkinput 0.12.0

### Breaking changes
- `is_path()`: add argument `require_sep` to facilitate checking filenames.
- Remove dependency `progutils` from `Suggests` to prevent dependency conflicts
  because `progutils` itself depends on `checkinput`.


# checkinput 0.11.0

### Breaking changes
- Rename argument `allow_zero_length` to `allow_zerolength`.


# checkinput 0.10.0

### Breaking changes
- Add dependency `progutils >= 0.0.4` to `Suggests` because it is used in the
  documentation.

### Bugfixes
- `is_path("C:")` would warn that filename should not contain `:`.


# checkinput 0.9.0

### Breaking changes
- Add dependency `fs` to `Imports` because it is used in `is_path()`.
- `paste_quoted()` now accepts atomic non-vector objects such as the output of
  `fs::path()`.

### Added functions
- `is_path()`: move from package `progutils` to `checkinput`. Rename argument
  `path` to `x`. Warn and return `FALSE` instead of throwing an error if `path`
  is not a valid path. Not allow filenames to start with a hyphen. Not warn
  about duplicated file separators or pointing to `tempdir()`.


# checkinput 0.8.0

### Breaking changes
- `all_names()`: include the used call instead of hardcoded `x` in suggestions
  to make names valid.
- Rename argument `allow_zero` to `allow_zero_length`.


# checkinput 0.7.0

### Breaking changes
- `make_natural()`: change default `all` from `TRUE` to `FALSE` because it will
  be used more often with scalar input.


# checkinput 0.6.0

### Breaking changes
- Use `roxygen2` version 8.0.0.
- Replace the non-exported function `paste_quoted()` by the full function from
  `progutils::paste_quoted()` and export it.


# checkinput 0.5.0

### Breaking changes
- Dependency `tinytest`: declare version `>= 1.4.1` because I use argument
  `strict` in `expect_message()` and `expect_warning()`.
- Remove the dependency on `vctrs` by hardcoding the output of the single
  remaining call to `vctrs::vec_as_names()` in a test for `all_names()`.


# checkinput 0.4.0

### Breaking changes
- Dependency `R >= 4.0.0` increased to `R >= 4.1.0`, which is required to pass
  the R CMD check on Ubuntu-latest: `rmarkdown` > `bslib` > `sass` > `fs` needs
  `R >= 4.1` and `rappdirs` needs `R >= 4.1.0`.
- `all_names()`: wrap text of warnings. Use `deparse1(substitute(x))` to get the
  offending values instead of literal `'x'` in the text of warnings or errors.
- `is_nonnegative()`, `all_nonnegative()`, `is_positive()`: call `is_number()`
  or `all_numbers()` followed by `all(x >= 0, na.rm = TRUE)` or
  `all(x > 0, na.rm = TRUE)` for more succinct and uniform code.


# checkinput 0.3.0

### Breaking changes
- Depend on `R >= 4.0.0` to be able to use `deparse1()`.

### Added functions
- `make_natural()`: use `is_natural()` or `all_natural()` to round values, while
  throwing an error if appropriate. Use `deparse1(substitute(x))` to get the
  offending values instead of literal `'x'` in error messages.


# checkinput 0.2.0

### Breaking changes
- `all_natural()`: `tol` should be smaller than `0.5`. Add argument `allow_zero`
  with default `FALSE`, in contrast to the previous implicit default `TRUE`.

### Added functions
- `is_natural`: as `all_natural()` but test if `x` has a length of at most one.


# checkinput 0.0.6

### Breaking changes
- Add dependency `vctrs` to `Suggests` because it is used in documentation of
  `all_names()`.
- `all_names()`: remove argument `allow_susp` and never allows suspicious names;
  also check for suspicious names that might have been created by `data.frame()`
  or `vctrs::vec_as_names()`; consider the reserved words `...` and `..1`
  invalid; make existing checks are now more specific: check that names which
  apparently have been made valid indeed contain an invalid part, and that the
  first digit of numbers added to duplicated names is non-zero.
- `is_number()` and derived functions gain arguments `allow_zero`, `allow_NA`
  and `allow_NaN` (with default `FALSE`, in contrast to the previously implicit
  `TRUE`), to resolve the contradiction that using `is_number()` returned `TRUE`
  for these values but using them in conditional statements led to `logical(0)`
  and thus to an error.


# checkinput 0.0.4

### Breaking changes
- Functions now allow objects with dimensions as input to `x` but return `FALSE`
  for them, as well as for non-atomic input.
- Simplify `all_names()` by removing checking for non-ASCII characters and never
  allow names that are duplicated or consist only of dots.
