# devel
- `all_names()`: wrap text of warnings. Use `deparse1(substitute(x))` to get the
  offending values instead of `x` in the text of warnings or errors.
- `is_nonnegative()`, `all_nonnegative()`, `is_positive()`: call `is_number()`
  or `all_numbers()` followed by `all(x >= 0, na.rm = TRUE)` or
  `all(x > 0, na.rm = TRUE)` for more succinct and more uniform code.

### Miscellaneous
- GitHub action `check-standard` now also runs on R 4.0.0, is triggered every
  Saturday on 04:23 UTC, and can be triggered manually (trigger it once manually
  on the main branch to be able to trigger it manually on other branches).


# checkinput 0.3.1
`checkinput` now uses GitHub action `check-standard` on all branches.


# checkinput 0.3.0

### Breaking changes
- Depend on `R` >= 4.0.0 to be able to use `deparse1()`.

### Added functions
- `make_natural`: uses `is_natural` or `all_natural` to round values, while
  throwing an error if appropriate. Uses `deparse1(substitute(x))` to get the
  offending values instead of `x` in error messages.


# checkinput 0.2.0

### Breaking changes
- `all_natural`: `tol` should be smaller than `0.5`. Added argument `allow_zero`
  with default `FALSE`, in contrast to the previous implicit default `TRUE`.

### Added functions
- `is_natural`: as `all_natural` but also tests if `x` has a length of at most
  one.


# checkinput 0.1.0

### Updated documentation
- Vignettes: add hyperlinks from the vignettes to the help-pages of functions.
  No need to put output in comments because the output is automatically shown
  together with the code. Use `<funcname>` instead of `<func>` or `<function>`
  to align with usage in the vignettes of `checkrpkgs`. 
- README: use `<funcname>` instead of `<func>` or `<function>` to align with
  usage in the vignettes of `checkrpkgs`. Define variables to write more
  succinct code.


# checkinput 0.0.6
This update has large changes to `is_number()` and related functions, and to
`all_names()`.

### Breaking changes
- `all_names()`: removed argument `allow_susp` and never allows suspicious names
  because allowing them amounts to only checking for syntactically invalid names,
  for which `make.names()` can be used directly; not trying anymore to identify
  which functions might have changed a name because that is very ambiguous; also
  check for suspicious names that might have been created by `data.frame()` or
  `vctrs::vec_as_names()`; consider the reserved words `...` and `..1` invalid
  and, when appropriate, note that `make.names()` does not adjust them; existing
  checks are now more specific: they check that names which apparently have been
  made valid indeed contain an invalid part, and that the first digit of numbers
  added to duplicated names is non-zero.
- `is_number()` and derived functions gained arguments `allow_zero`, `allow_NA`
  and `allow_NaN` (with default `FALSE` in contrast to the previously implicit
  `TRUE`), to resolve the contradiction that using `is_number()` returned `TRUE`
  for these values but using them in conditional statements led to `logical(0)`
  and thus to an error.

### Updated documentation
- Added `vctrs` as dependency in `Suggests` because it is used in documentation
  of `all_names()`.
- `all_names()`: major updates and restructuring. Also updated and added examples.
- `all_natural()`: added a note about functions named `integerish`.
- `README`: added a reference to my repository `checkrpkgs`. Clarified that
  functions *do* throw errors about invalid input to arguments other than `x`.
- Vignette `design_choices.Rmd`: clarified that functions *do* throw errors about
  invalid input to arguments other than `x`. Added a table of contents. Removed
  obsolete note about `allow_NA` being absent from `is_number()` and derived
  functions.
- Vignette `Type_Coercion.Rmd`: renamed to `type_coercion.Rmd`. Added section
  labels and a table of contents.
- Replaced section title `Note` (created through `@Note`) by section title `Notes`
  (created through `@section Notes:`). Idem for `Programming note`.


# checkinput 0.0.5
This update has many changes on documentation.

### Breaking changes
- `paste_quoted()` (an internal function) now throws an error if `x` has
  dimensions.

### Bug fixes
- `paste_quoted()` (an internal function) now throws an error if `x` has
  dimensions.

### Minor improvements
- Moved code of `is_character.R` to `all_characters.R`, and code of
  `is_positive.R`, `is_nonnegative.R`, etc. to `is.number.R`.

### Updated documentation
- Updated `README` to mention vignettes and introduce design choices.
- Cross-reference on `is_number()` instead of on `all_nonnegative()`.
- Removed `Note`s about legacy code.
- Moved `To do` points and `Wishlist` to GitHub issues.
- Moved information about correct input to the vignette `design_choices` that is
  linked in the `See also` sections of the relevant functions. The vignette also
  contains a section about getting a named boolean vector as output. Both
  sections are also mentioned in the `README`.
- `all_names()`: `data.frame()` also calls `make.names()`. Moved section on how
  to get a boolean vector to the vignette on design choices.
- `is_zerolength()`: document that a zero-row `data.frame` is not a zero-length
  object.


# checkinput 0.0.4

### Breaking changes
- Functions now allow objects with dimensions as input to `x` but return `FALSE`
  for them, as well as for non-atomic input.
- Simplified `all_names()` by removing checking for non-ASCII characters and
  never allow names that are duplicated or consist only of dots.


# checkinput 0.0.3

`NEWS` for this and earlier versions has not been tracked.
