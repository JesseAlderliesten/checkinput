# checkinput-devel
Changes to the development-branch of `checkinput`.

## Breaking changes
- None.

## Bug fixes
- None.

## Added functions
- None.

## Minor improvements
- None.

## Updated documentation
- all_names: `data.frame()` also calls `make.names()`. Added `To do` point:
  Add check for automated column names created by `data.frame()` for unnamed
  columns if `fix.empty.names` is `TRUE`.


# checkinput 0.0.4

## Breaking changes
- Functions allow objects with dimensions as input to 'x' but return `FALSE` for
  them, as well as for non-atomic input.
- Simplified `all_names()` by removing checking for non-ASCII characters and
  never allow names that are duplicated or consist only of dots.


# checkinput 0.0.3

NEWS for this and earlier versions has not been tracked.
