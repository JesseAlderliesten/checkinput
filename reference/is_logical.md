# Check that `x` is logical

Check that `x` is a length-one logical vector with only allowed values.

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
  `x` of the correct type?

- allow_NA:

  `TRUE` or `FALSE`: allow [NA](https://rdrr.io/r/base/NA.html) of the
  correct type in `x`?

## Value

`TRUE` or `FALSE` indicating if `x` is a logical vector of the correct
length with only allowed values.

## Details

`is_logical()` returns `TRUE` for logical-type `x` of length one, for
logical-type
[zero-length](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
`x` if `allow_zerolength` is `TRUE`, and for logical-type `NA` if
`allow_NA` is `TRUE`.

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
```
