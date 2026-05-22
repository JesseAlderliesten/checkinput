# Check that `x` is character

Check that `x` is a character vector of the correct length with only
allowed values.

## Usage

``` r
all_characters(x, allow_empty = FALSE, allow_zero = FALSE, allow_NA = FALSE)

is_character(x, allow_empty = FALSE, allow_zero = FALSE, allow_NA = FALSE)
```

## Arguments

- x:

  object to test.

- allow_empty:

  `TRUE` or `FALSE`: allow empty strings (`""`) in `x`?

- allow_zero:

  `TRUE` or `FALSE`: allow zero-length `x` of the correct type?

- allow_NA:

  `TRUE` or `FALSE`: allow `NA`s of the correct type in `x`?

## Value

`TRUE` or `FALSE` indicating if `x` is a character vector of the correct
length with only allowed values.

## Notes

`all_characters()` and `is_character()` by default return `FALSE` for
empty strings.

## See also

The vignettes *Design choices regarding function input*:
[`vignette("design_choices", package = "checkinput")`](https://jessealderliesten.github.io/checkinput/articles/design_choices.md)
and *Type coercion in vectors*:
[`vignette("type_coercion", package = "checkinput")`](https://jessealderliesten.github.io/checkinput/articles/type_coercion.md).

Other collections of checks on type and length:
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
[`is_zerolength()`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)

## Examples

``` r
is_character("a") # TRUE
#> [1] TRUE
is_character(c("a", "b")) # FALSE: incorrect length
#> [1] FALSE
all_characters(c("a", "b")) # TRUE
#> [1] TRUE
is_character(1) # FALSE: incorrect type
#> [1] FALSE
is_character(NA_character_) # FALSE: default 'allow_NA' is FALSE
#> [1] FALSE
is_character(NA_character_, allow_NA = TRUE) # TRUE
#> [1] TRUE
is_character(NA, allow_NA = TRUE) # FALSE: incorrect type
#> [1] FALSE
```
