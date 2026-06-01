# Changelog

## checkinput 0.9.0

#### Breaking changes

- Added dependency `fs` that is used in
  [`is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.md).
- [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.md)
  now accepts atomic non-vector objects such as the output of
  [`fs::path()`](https://fs.r-lib.org/reference/path.html).

#### Added functions

- [`is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.md):
  move from package `progutils` to `checkinput`. Rename argument `path`
  to `x`. Warn and return `FALSE` instead of throwing an error if `path`
  is not a valid path. Not allow filenames to start with a hyphen. Not
  warn about duplicated file separators or pointing to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## checkinput 0.8.0

#### Breaking changes

- [`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md)
  now includes the used call instead of hardcoded `x` in suggestions to
  make names valid.
- [`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
  [`is_character()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md)
  [`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
  [`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
  [`all_numbers()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
  [`is_nonnegative()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
  [`all_nonnegative()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
  [`is_positive()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
  [`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
  [`all_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md)
  and
  [`make_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md):
  renamed argument `allow_zero` to `allow_zero_length`.

## checkinput 0.7.0

#### Breaking changes

- [`make_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md):
  change default `all` from `TRUE` to `FALSE` because it will be used
  more often with scalar input.

## checkinput 0.6.5

#### Documentation

- [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.md):
  clearer title and description.

## checkinput 0.6.4

#### Documentation

- Vignette `design_choices`: shorten name to `Design choices`.
- Fix links to vignettes.

## checkinput 0.6.2

#### Documentation

- Vignettes: explain notation. Rely on `pkgdown` to create links to
  functions.

## checkinput 0.6.0

#### Breaking changes

- Use `roxygen2` version 8.0.0.
- Replace the non-exported function
  [`paste_quoted()`](https://jessealderliesten.github.io/checkinput/reference/paste_quoted.md)
  by the full function from `progutils::paste_quoted()` and export it.

## checkinput 0.5.0

#### Breaking changes

- Dependency `tinytest`: declare version `>= 1.4.1` because I use
  argument `strict` in `expect_message()` and `expect_warning()`.
- Remove the dependency on `vctrs` by hardcoding the output of the
  single remaining call to
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  in a test for
  [`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md).

## checkinput 0.4.0

#### Breaking changes

- Dependency `R` \>= 4.0.0 increased to `R` \>= 4.1.0, which is required
  to pass the R CMD check on ubuntu-latest: `rmarkdown` \> `bslib` \>
  `sass` \> `fs` needs `R` \>= 4.1 and `rappdirs` needs `R` \>= 4.1.
- [`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md):
  wrap text of warnings. Use `deparse1(substitute(x))` to get the
  offending values instead of literal `'x'` in the text of warnings or
  errors.
- [`is_nonnegative()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
  [`all_nonnegative()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
  [`is_positive()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md):
  call
  [`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
  or
  [`all_numbers()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
  followed by `all(x >= 0, na.rm = TRUE)` or `all(x > 0, na.rm = TRUE)`
  for more succinct and uniform code.

## checkinput 0.3.0

#### Breaking changes

- Depend on `R` \>= 4.0.0 to be able to use
  [`deparse1()`](https://rdrr.io/r/base/deparse.html).

#### Added functions

- [`make_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md):
  use
  [`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md)
  or
  [`all_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md)
  to round values, while throwing an error if appropriate. Use
  `deparse1(substitute(x))` to get the offending values instead of
  literal `'x'` in error messages.

## checkinput 0.2.0

#### Breaking changes

- [`all_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md):
  `tol` should be smaller than `0.5`. Add argument `allow_zero` with
  default `FALSE`, in contrast to the previous implicit default `TRUE`.

#### Added functions

- `is_natural`: as
  [`all_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md)
  but test if `x` has a length of at most one.

## checkinput 0.1.0

#### Documentation

- Vignettes: add hyperlinks from the vignettes to the help-pages of
  functions. No need to put output in comments because the output is
  automatically shown together with the code. Use `<funcname>` instead
  of `<func>` or `<function>` to align with usage in the vignettes of
  `checkrpkgs`.
- `README`: use `<funcname>` instead of `<func>` or `<function>` to
  align with usage in the vignettes of `checkrpkgs`. Define variables to
  write more succinct code.

## checkinput 0.0.6

#### Breaking changes

- Added `vctrs` as dependency in `Suggests` because it is used in
  documentation of
  [`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md).
- [`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md):
  remove argument `allow_susp` and never allows suspicious names because
  allowing them amounts to only checking for syntactically invalid
  names, for which
  [`make.names()`](https://rdrr.io/r/base/make.names.html) can be used
  directly; do not try to identify which functions might have changed a
  name because that is very ambiguous; also check for suspicious names
  that might have been created by
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) or
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html);
  consider the reserved words `...` and `..1` invalid and, when
  appropriate, note that
  [`make.names()`](https://rdrr.io/r/base/make.names.html) does not
  adjust them; existing checks are now more specific: they check that
  names which apparently have been made valid indeed contain an invalid
  part, and that the first digit of numbers added to duplicated names is
  non-zero.
- [`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
  and derived functions gain arguments `allow_zero`, `allow_NA` and
  `allow_NaN` (with default `FALSE`, in contrast to the previously
  implicit `TRUE`), to resolve the contradiction that using
  [`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
  returned `TRUE` for these values but using them in conditional
  statements led to `logical(0)` and thus to an error.

#### Documentation

- [`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md):
  major updates and restructuring. Also updated and added examples.
- `README`: add a reference to my repository `checkrpkgs`. Clarify that
  functions **do** throw errors about invalid input to arguments other
  than `x`.
- Vignette `design_choices`: clarified that functions **do** throw
  errors about invalid input to arguments other than `x`.

## checkinput 0.0.5

#### Documentation

- `README`: mention vignettes and introduce design choices.
- Remove `Notes` about legacy code.
- Move `To do` points and `Wishlist` to GitHub issues.
- Move information about correct input to the vignette `design_choices`
  that is linked in the `See also` sections of the relevant functions.
  The vignette also contains a section about getting a named boolean
  vector as output. Both sections are also mentioned in the `README`.
- [`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md):
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) also calls
  [`make.names()`](https://rdrr.io/r/base/make.names.html). Move section
  on how to get a boolean vector to the vignette on design choices.
- [`is_zerolength()`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md):
  document that a zero-row data frame is not a zero-length object.

## checkinput 0.0.4

#### Breaking changes

- Functions now allow objects with dimensions as input to `x` but return
  `FALSE` for them, as well as for non-atomic input.
- Simplify
  [`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md)
  by removing checking for non-ASCII characters and never allow names
  that are duplicated or consist only of dots.

## checkinput 0.0.3

`NEWS` for this and earlier versions has not been tracked.
