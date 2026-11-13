# Check that x is Logical

Check that `x` is a length-one [logical
vector](https://rdrr.io/r/base/logical.html) with only allowed values.

## Usage

``` r
is_logical(x, allow_zerolength = FALSE, allow_NA = FALSE)
```

## Arguments

- x:

  object to check.

- allow_zerolength:

  `TRUE` or `FALSE`: allow
  [zero-length](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
  `x` of the correct [type](https://rdrr.io/r/base/typeof.html)?

- allow_NA:

  `TRUE` or `FALSE`: allow [`NA`](https://rdrr.io/r/base/NA.html) of the
  correct type in `x`?

## Value

`TRUE` or `FALSE`

## Details

`is_logical()` returns `TRUE` if `x` is

- `TRUE`

- `FALSE`

- [`logical(0)`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
  and `allow_zerolength` is `TRUE`

- logical-type [`NA`](https://rdrr.io/r/base/NA.html) and `allow_NA` is
  `TRUE`

## See also

Other collections of checks on type and length:
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
[`is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.md),
[`is_zerolength()`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)

## Examples

``` r
is_logical(TRUE) # TRUE
#> [1] TRUE
is_logical(c(TRUE, TRUE)) # FALSE: incorrect length
#> [1] FALSE
is_logical(1) # FALSE: incorrect type
#> [1] FALSE
is_logical(NA) # FALSE: default 'allow_NA' is FALSE
#> [1] FALSE
is_logical(NA, allow_NA = TRUE) # TRUE
#> [1] TRUE
is_logical(NA_character_, allow_NA = TRUE) # FALSE: incorrect type
#> [1] FALSE
is_logical(matrix(TRUE)) # FALSE: has non-NULL dimensions
#> [1] FALSE
is_logical(data.frame(TRUE)) # FALSE: has non-NULL dimensions
#> [1] FALSE
```
