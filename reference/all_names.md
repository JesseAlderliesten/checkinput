# Check that `x` contains syntactically valid, unadjusted names

Check that `x` is a character vector with unique, syntactically valid
names that do not suggest they were adjusted or automatically created.

## Usage

``` r
all_names(x, allow_underscores = TRUE)
```

## Arguments

- x:

  object to check.

- allow_underscores:

  `TRUE` or `FALSE`: allow underscores?

## Value

`TRUE` or `FALSE`, indicating if `x` is a character vector that consists
of unique, syntactically valid names that do **not** consist of only
dots or of two dots followed by a number, and do **not** suggest they
were adjusted or automatically created.

## Details

[Duplicated](https://rdrr.io/r/base/duplicated.html) or syntactically
invalid names are not allowed by `all_names()` because R functions are
not guaranteed to handle such names correctly. For example, not all
operations on [data frames](https://rdrr.io/r/base/data.frame.html) will
preserve duplicated column names, and operations involving syntactically
invalid names might, by definition, give undocumented results.

[Syntactically valid](https://rdrr.io/r/base/make.names.html) names only
consist of letters, numbers, dots and underscores; start with a letter,
or with a dot not followed by a number; and are not
[reserved](https://rdrr.io/r/base/Reserved.html) words such as
[for](https://rdrr.io/r/base/Control.html) or any of the
[NA](https://rdrr.io/r/base/NA.html)s. The definition of **letter**
depends on the current [locale](https://rdrr.io/r/base/locales.html). A
conservative check for names that are syntactically valid on all locales
would only allow digits and unaccented Latin letters, but that is
**not** enforced by `all_names()`.

Names that consist of only dots, or consist of two dots followed by a
number, are not allowed by `all_names()` (nor by
[`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html))
even though they are not adjusted by
[`make.names()`](https://rdrr.io/r/base/make.names.html): they are
listed as [reserved](https://rdrr.io/r/base/Reserved.html) words.

Suspicious names are not allowed by `all_names()`. A suspicious name
contains a pattern suggesting it originally was syntactically invalid
and has been **adjusted** into a syntactically valid name, or has been
adjusted to make names
[unique](https://rdrr.io/r/base/make.unique.html). Such adjustments
usually occur silently, for example when data is read into R, such that
it should **not** be assumed that column names after reading data into R
are the same as the column names before reading data into R. The
identification of suspicious names is partly based on the assumption
that names originally did not contain dots, see the first item in the
list below.

`all_names()` **tries** to recognise adjustments made by
[`make.names()`](https://rdrr.io/r/base/make.names.html), which is used
by [`data.frame()`](https://rdrr.io/r/base/data.frame.html),
[`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html), and
`data.table::fread(x, header = TRUE, check.names = TRUE)`; and
adjustments made by `vctrs::vec_as_names(x, repair = "universal")`,
which is used throughout the [tidyverse](https://tidyverse.org/):

- adjustments to replace invalid characters (i.e., characters that are
  not a letter, number, dot or underscore):
  [`make.names()`](https://rdrr.io/r/base/make.names.html) and
  `vctrs::vec_as_names(x, repair = "universal")` replace such characters
  with a dot. Their identification is based on the assumption that names
  originally did **not** contain dots, which is good practice preventing
  names containing a dot from being confused with
  [methods](https://rdrr.io/r/base/UseMethod.html) used on [classed
  objects](https://rdrr.io/r/base/is.object.html), even though that
  advice is not strictly followed in base-R, e.g., in the function name
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- adjustments to make duplicated names unique:
  `make.names(x, unique = TRUE)` appends a dot followed by a number;
  `vctrs::vec_as_names(x, repair = "universal")` appends three dots
  followed by a number.
  [`make.names()`](https://rdrr.io/r/base/make.names.html) does **not**
  adjust the first instance of a duplicate, whereas
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  **does** adjust it: `make.names(c("a", "a"), unique = TRUE)` returns
  `c("a", "a.1")`, whereas
  `vctrs::vec_as_names(c("a", "a"), repair = "universal")` returns
  `c("a...1", "a...2")`.

- adjustments to make [reserved](https://rdrr.io/r/base/Reserved.html)
  words valid: [`make.names()`](https://rdrr.io/r/base/make.names.html)
  appends a dot; `vctrs::vec_as_names(x, repair = "universal")` prepends
  a dot.

- adjustments to make names that did not start with a letter, nor with a
  dot not followed by a number, syntactically valid:
  [`make.names()`](https://rdrr.io/r/base/make.names.html) prepends `X`;
  `vctrs::vec_as_names(x, repair = "universal")` prepends one or more
  dots.

- adjustments to name unnamed columns:
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) uses pattern
  `X1`, `X2`, `X3`;
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  `read.csv(..., header = FALSE)` use pattern `V1`, `V2`, `V3`;
  `read.csv(..., header = TRUE)` uses pattern `X`, `X.1`, `X.2`. It is
  **not** checked if a complete sequence of suspicious names is present,
  e.g., `V3` will be flagged as suspicious even if `V1` and `V2` are
  absent.

Names containing underscores (`_`) are by default **allowed** by
`all_names()` because names containing underscores are not syntactically
invalid. However, setting `allow_underscores` to `FALSE` to **not**
allow such names is useful to check that names do not contain
underscores, for example if several names will be concatenated to create
an ID-tag, separating the parts by underscores.

## Programming notes

The patterns used to identify suspicious names are created using
[regular expressions](https://rdrr.io/r/base/regex.html) with the
following elements:

- require a pattern to start at the beginning of a string (`^`) or reach
  the end of a string (`$`);

- specify characters that should be present: a dot (`\\.` or, if `fixed`
  is `TRUE`, `.`), an underscore (`_`), any digit (`[0-9]`), digits one
  to nine (`[1-9]`), characters `V` or `X`);

- indicate presence: present zero or more times (`*`); present one or
  more times (`+`).

Multiple patterns can be combined using `|`, the normal operator
indicating [logical OR](https://rdrr.io/r/base/Logic.html).

## See also

Section `Details` of
[`make.names()`](https://rdrr.io/r/base/make.names.html), section
`Names and Identifiers` of [Quotes](https://rdrr.io/r/base/Quotes.html),
and the [R FAQ about valid
names](https://CRAN.R-project.org/doc/manuals/R-FAQ.html#What-are-valid-names_003f)
on the syntactical validity of names.

[`names()`](https://rdrr.io/r/base/names.html) to get or set object
names; `janitor::make_clean_names()` to adjust names, e.g., through
adjusting case and transliterating non-ASCII characters.

Other collections of checks on type and length:
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
[`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
[`is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.md),
[`is_zerolength()`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)

## Examples

``` r
all_names(x = c("a", "b2a")) # TRUE
#> [1] TRUE
all_names(x = c("a", "b2a", "a")) # FALSE: duplicated name
#> Warning: Names are duplicated: 'a'.
#> Use 'make.names(c("a", "b2a", "a"), unique = TRUE)' to create unique, syntactically valid names!
#> [1] FALSE

invalid_names <- c("a", "ab#cd", "", "for", "..", "..23")
# Syntactically invalid names: the character '#' makes names invalid,
# '""' is an empty name, 'for', '..', and '..23' are reserved words.
all_names(x = invalid_names) # FALSE
#> Warning: Names are syntactically invalid: 'ab#cd', 'for', '""' (i.e., an empty string); and consist of only dots, which is a reserved word: '..'; and consist of two dots followed by digits, which is a reserved word: '..23'.
#> Use 'invalid_names <- make.names(invalid_names, unique = TRUE)' to create unique, syntactically valid names
#> (it does not adjust names that consist of only dots, or two dots followed by digits)!
#> [1] FALSE

# Names that have been made valid are suspicious
# (but make.names() does not adjust ".." or "..23"):
all_names(x = make.names(invalid_names)) # FALSE
#> Warning: Names consist of only dots, which is a reserved word: '..'; and consist of two dots followed by digits, which is a reserved word: '..23'; and are suspicious: 'ab.cd', 'X', 'for.'
#> [1] FALSE

# FALSE: suspicious names
all_names(x = c("e.2", "a.1b", ".TRUE", "..22c", "a...2",
                "V3", "X.2", "X0...11", "X0.3", "X3"))
#> Warning: Names are suspicious: 'e.2', 'a.1b', '.TRUE', '..22c', 'a...2', 'V3', 'X.2', 'X0...11', 'X0.3', 'X3'
#> [1] FALSE

all_names(x = "abc_def", allow_underscores = FALSE) # FALSE: underscores
#> Warning: Names contain underscores (which are not allowed if 'allow_underscores' is FALSE):
#> 'abc_def'.
#> Use '"abc_def" <- make.names("abc_def", unique = TRUE, allow_ = FALSE)' to create unique,
#> syntactically valid names without underscores!
#> [1] FALSE
all_names(x = "abc_def", allow_underscores = TRUE) # TRUE
#> [1] TRUE

# pass names() or colnames() used on an object
# without (column) names to all_names():
all_names(x = names(1:3)) # FALSE
#> Warning: 'x' (names(1:3)) is NULL: did you use names() or colnames() on an object without
#> (column) names and passed the result to all_names()?
#> [1] FALSE

all_names(13) # FALSE: 'x' is not a character vector
#> Warning: Input to 'x' is not a character vector: 13
#> [1] FALSE
```
