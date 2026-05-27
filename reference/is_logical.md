# Check that `x` is logical

Check that `x` is a length-one logical vector with only allowed values.

## Usage

``` r
is_logical(x, allow_zero_length = FALSE, allow_NA = FALSE)
```

## Arguments

- x:

  object to test.

- allow_zero_length:

  `TRUE` or `FALSE`: allow zero-length `x` of the correct type?

- allow_NA:

  `TRUE` or `FALSE`: allow `NA`s of the correct type in `x`?

## Value

`TRUE` or `FALSE` indicating if `x` is a length-one logical vector only
containing allowed values.

## See also

The vignette *Design choices regarding function input*:
[`vignette("design_choices", package = "checkinput")`](https://jessealderliesten.github.io/checkinput/articles/design_choices.md).

Other collections of checks on type and length:
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
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
