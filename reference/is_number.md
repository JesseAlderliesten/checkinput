# Check that `x` is numeric

Check that `x` is a numeric vector of the correct length with numbers of
the correct sign.

## Usage

``` r
is_number(x, allow_zero = FALSE, allow_NA = FALSE, allow_NaN = FALSE)

all_numbers(x, allow_zero = FALSE, allow_NA = FALSE, allow_NaN = FALSE)

is_nonnegative(x, allow_zero = FALSE, allow_NA = FALSE, allow_NaN = FALSE)

all_nonnegative(x, allow_zero = FALSE, allow_NA = FALSE, allow_NaN = FALSE)

is_positive(x, allow_zero = FALSE, allow_NA = FALSE, allow_NaN = FALSE)
```

## Arguments

- x:

  object to test.

- allow_zero:

  `TRUE` or `FALSE`: allow zero-length `x` of the correct type?

- allow_NA:

  `TRUE` or `FALSE`: allow numeric [NA](https://rdrr.io/r/base/NA.html)s
  (i.e., `NA_integer_` and `NA_real_`)?

- allow_NaN:

  `TRUE` or `FALSE`: allow
  [NaN](https://rdrr.io/r/base/is.finite.html)s?

## Value

`TRUE` or `FALSE` indicating if `x` is a numeric vector of the correct
length only containing allowed numbers.

## Details

The correct length of `x` is one for `is_...()` and larger than zero for
`all_...()`, unless `allow_zero` is `TRUE`: then numeric-type
zero-length `x` is also allowed for both types of functions.

`all_nonnegative()` and `is_nonnegative()` return `TRUE` for `0`,
whereas `is_positive()` returns `FALSE` for `0`.

All functions return `TRUE` for `-Inf` and `Inf` if it has the correct
sign; return `TRUE` for [NaN](https://rdrr.io/r/base/is.finite.html)
(which has [mode](https://rdrr.io/r/base/mode.html) `numeric`, despite
meaning 'not a number') if `allow_NaN` is `TRUE`; and return `FALSE` for
`NA_complex_` (even if `allow_NA` is `TRUE`) because its mode is
`complex` instead of `numeric`.

## Programming notes

[`is.numeric()`](https://rdrr.io/r/base/numeric.html) tests the
[`mode()`](https://rdrr.io/r/base/mode.html) of `x`, which is `numeric`
for floating-point numbers such as 3.2 and integers such as 3L. In
contrast, `class(x) == "numeric"` (or, more robust,
`inherits(x = x, what = "numeric")`) would test the
[`class()`](https://rdrr.io/r/base/class.html) of `x` which is `numeric`
for floating-point numbers but `integer` for integers (see the
`Note on names` in
[`is.numeric()`](https://rdrr.io/r/base/numeric.html)).

The functions duplicate code instead of calling `is_number()` or
`all_numbers()`, to prevent performing checks twice.

## See also

The vignettes *Design choices regarding function input*:
[`vignette("design_choices", package = "checkinput")`](https://jessealderliesten.github.io/checkinput/articles/design_choices.md)
and *Type coercion in vectors*:
[`vignette("type_coercion", package = "checkinput")`](https://jessealderliesten.github.io/checkinput/articles/type_coercion.md).

Other collections of checks on type and length:
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
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
is_number(x = numeric(0), allow_zero = TRUE) # TRUE
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
all_nonnegative(x = numeric(0), allow_zero = TRUE) # TRUE
#> [1] TRUE
is_positive(x = 3) # TRUE
#> [1] TRUE
is_positive(x = 0) # FALSE
#> [1] FALSE
```
