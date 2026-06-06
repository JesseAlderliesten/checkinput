# Check that `x` contains numbers

Check that `x` is a vector of the correct length with numbers of the
correct sign.

## Usage

``` r
is_number(x, allow_zerolength = FALSE, allow_NA = FALSE, allow_NaN = FALSE)

all_numbers(x, allow_zerolength = FALSE, allow_NA = FALSE, allow_NaN = FALSE)

is_nonnegative(
  x,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  allow_NaN = FALSE
)

all_nonnegative(
  x,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  allow_NaN = FALSE
)

is_positive(x, allow_zerolength = FALSE, allow_NA = FALSE, allow_NaN = FALSE)
```

## Arguments

- x:

  object to check.

- allow_zerolength:

  `TRUE` or `FALSE`: allow
  [zerolength](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
  `x` of the correct type?

- allow_NA:

  `TRUE` or `FALSE`: allow [NA](https://rdrr.io/r/base/NA.html)s of the
  correct type in `x`?

- allow_NaN:

  `TRUE` or `FALSE`: allow
  [NaN](https://rdrr.io/r/base/is.finite.html)s?

## Value

`TRUE` or `FALSE` indicating if `x` is a vector of the correct length
only containing allowed numbers.

## Details

`is_number()`, `all_numbers()`, `all_nonnegative()` and
`is_nonnegative()` return `TRUE` for zero, whereas `is_positive()`
returns `FALSE` for zero.

`is_number()`, `is_nonnegative()`, and `is_positive()` return `TRUE` for
`x` with length one. `all_numbers()` and `all_nonnegative()` return
`TRUE` for `x` with length larger than zero. All these functions return
`TRUE` for numeric-type
[zero-length](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
`x` if `allow_zerolength` is `TRUE`.

All these functions return `TRUE` for `-Inf` and `Inf` if it has the
correct sign, for `NA_integer_` and `NA_real_` if `allow_NA` is `TRUE`
(even then they return `FALSE` for `NA_complex_` because its mode is
`complex` instead of `numeric`), and for
[NaN](https://rdrr.io/r/base/is.finite.html) (which has
[mode](https://rdrr.io/r/base/mode.html) `numeric`, despite meaning 'not
a number') if `allow_NaN` is `TRUE`.

## Programming notes

[`is.numeric()`](https://rdrr.io/r/base/numeric.html) checks the
[`mode()`](https://rdrr.io/r/base/mode.html) of `x`, which is `numeric`
for floating-point numbers such as `3.2` and integers such as `3L`. In
contrast, `class(x) == "numeric"` (or, more robust,
`inherits(x = x, what = "numeric")`) would check the
[`class()`](https://rdrr.io/r/base/class.html) of `x` which is `numeric`
for floating-point numbers but `integer` for integers (see the
`Note on names` in
[`is.numeric()`](https://rdrr.io/r/base/numeric.html)).

## See also

Other collections of checks on type and length:
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
[`is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.md),
[`is_zerolength()`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)

## Examples

``` r
is_number(x = 1) # TRUE
#> [1] TRUE
is_number(x = 3.14) # TRUE
#> [1] TRUE
is_number(x = c(1, 2)) # FALSE: incorrect length
#> [1] FALSE
all_numbers(x = c(1, 2)) # TRUE
#> [1] TRUE
is_number(x = "a") # FALSE: incorrect type
#> [1] FALSE
is_number(x = numeric(0)) # FALSE: incorrect length
#> [1] FALSE
is_number(x = numeric(0), allow_zerolength = TRUE) # TRUE
#> [1] TRUE
is_number(x = NA_real_) # FALSE
#> [1] FALSE
is_number(x = NA_real_, allow_NA = TRUE) # TRUE
#> [1] TRUE
is_number(x = NA_character_, allow_NA = TRUE) # FALSE: incorrect type
#> [1] FALSE
is_number(x = NaN, allow_NA = TRUE) # FALSE, need allow_NaN = TRUE to allow NaN
#> [1] FALSE
is_number(x = NaN, allow_NaN = TRUE) # TRUE
#> [1] TRUE
is_number(x = Inf) # TRUE
#> [1] TRUE
is_nonnegative(x = 3) # TRUE
#> [1] TRUE
is_nonnegative(x = 0) # TRUE
#> [1] TRUE
all_nonnegative(x = c(3, 0)) # TRUE
#> [1] TRUE
all_nonnegative(x = numeric(0), allow_zerolength = TRUE) # TRUE
#> [1] TRUE
is_positive(x = 3) # TRUE
#> [1] TRUE
is_positive(x = 0) # FALSE
#> [1] FALSE
```
