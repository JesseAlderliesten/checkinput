# checkinput (development version)
The development-branch of `checkinput` is *work in progress*: see below for the
released versions.

### To do
- Change `all_names()` (BACK?) to `are_names()`, returning a boolean vector?
  Error on non-character-vector input.
- Link from vignettes to help-pages? See
  https://github.com/dmurdoch/rgl/commit/bbc84447c2a6efed42907fbac176e9569b868d8f
  and https://stackoverflow.com/questions/34946219/linking-r-package-vignettes.

### Breaking changes
- None.

### Bug fixes
- None.

### Added functions
- None.

### Minor improvements
- None.

### Updated documentation
- None.

### Updated tests
- None.


# checkinput 0.0.5
This update has many changes on documentation.

### Breaking changes
- `paste_quoted()` (an internal function) now throws an error if `x` has
  dimensions.

### Bug fixes
- `paste_quoted()` (an internal function) now throws an error if `x` has
  dimensions.

### Added functions
- None.

### Minor improvements
- Put code of `is_character.R` in `all_characters.R`, and put code of
  `is_positive.R`, `is_nonnegative.R`, etc. in `is.number.R`. That is, copy code
  from files that used `@rdname <func>` to copy the documentation of `<func>` to
  the relevant files.

### Updated documentation
- Updated `README` to mention vignettes and introduce design choices.
- Cross-reference on `is_number()` instead of on `all_nonnegative()`.
- Removed `Note`s about legacy-code.
- Moved `To do` points and `Wishlist` to GitHub issues.
- Moved information about correct input to the vignette `design_choices` that is
  linked in the `See also` sections of the relevant functions. The vignette also
  contains a section about getting a named boolean vector as output. Both
  sections are also mentioned in the `README`.
- all_names(): `data.frame()` also calls `make.names()`. Moved section on how to
  get a boolean vector to the vignette on design choices.
- is_zerolength(): document that a zero-row data.frame is not a zero-length
  object.

### Updated tests
- Added tests for normal and zero-row/zero-column matrices and dataframes.
- Clean up after tests.
- all_names(): changed warning about missing tests to a comment and created a
  GitHub issue.
- is_zerolength(): test that zero-row data.frame is not a zero-length object.
- paste_quoted(): also test factor input and `x` with dimensions.


# checkinput 0.0.4

### Breaking changes
- Functions allow objects with dimensions as input to `x` but return `FALSE` for
  them, as well as for non-atomic input.
- Simplified `all_names()` by removing checking for non-ASCII characters and
  never allow names that are duplicated or consist only of dots.


# checkinput 0.0.3

NEWS for this and earlier versions has not been tracked.
