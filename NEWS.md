# checkinput-devel
Changes to the development-branch of `checkinput`: this branch is *Work in progress*.

## To do
- Standardize the `Note` about getting a named boolean vector as output across
  functions, and add it to the `README`: "To get a named boolean vector
  indicating for each element of `x` if it is a valid name, use
  `vapply(X = x, FUN.VALUE = logical(1), FUN = all_names, ...)` instead of
  `all_names(x, ...)`." See section `Note` in `all_names()` for good text.
- Put code of `is_positive()`, `is_nonnegative()`, etc. in a single file.
- Cross-reference on `all_numbers()` or `is_number()` instead of on `all_nonnegative()`.
- Change `all_names()` (BACK?) to `are_names()`, returning a boolean vector?
  Error on non-character-vector input.

## Breaking changes
- `paste_quoted()` (an internal function) now throws an error if `x` has dimensions.

## Bug fixes
- `paste_quoted()` (an internal function) now throws an error if `x` has dimensions.

## Added functions
- None.

## Minor improvements
- None.

## Updated documentation
- Updated `README` to mention vignettes and introduce design choices.
- Removed `Note` about legacy-code.
- Moved `To do` points and `Wishlist` to GitHub issues.
- Moved information about correct input to a vignette that is mentioned in the
  `See also` sections of the relevant functions.
- all_names(): `data.frame()` also calls `make.names()`.
- is_zerolength(): document that zero-row data.frame is not a zero-length object.

## Updated tests
- Added tests for normal and zero-row/zero-column matrices and dataframes.
- Clean up after tests.
- all_names(): changed warning about missing tests to a comment and created a GitHub issue.
- is_zerolength(): test that zero-row data.frame is not a zero-length object.
- paste_quoted(): also test factor input and `x` with dimensions.


# checkinput 0.0.4

## Breaking changes
- Functions allow objects with dimensions as input to `x` but return `FALSE` for
  them, as well as for non-atomic input.
- Simplified `all_names()` by removing checking for non-ASCII characters and
  never allow names that are duplicated or consist only of dots.


# checkinput 0.0.3

NEWS for this and earlier versions has not been tracked.
