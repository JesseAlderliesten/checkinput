# Check that `x` is character

Check that `x` is a character vector of the correct length with only
allowed values.

## Usage

``` r
all_characters(
  x,
  allow_empty = FALSE,
  allow_zerolength = FALSE,
  allow_NA = FALSE
)

is_character(
  x,
  allow_empty = FALSE,
  allow_zerolength = FALSE,
  allow_NA = FALSE
)
```

## Arguments

- x:

  object to check.

- allow_empty:

  `TRUE` or `FALSE`: allow empty strings (`""`) in `x`?

- allow_zerolength:

  `TRUE` or `FALSE`: allow
  [zero-length](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
  `x` of the correct type?

- allow_NA:

  `TRUE` or `FALSE`: allow [NA](https://rdrr.io/r/base/NA.html)s of the
  correct type in `x`?

## Value

`TRUE` or `FALSE` indicating if `x` is a character vector of the correct
length with only allowed values.

## Details

`is_character()` and `all_characters()` return `TRUE` for empty strings
(`""`) if `allow_empty` is `TRUE`, and for `NA_character_` if `allow_NA`
is `TRUE`.

`is_character()` returns `TRUE` for `x` with length one and
`all_characters()` returns `TRUE` for `x` with length larger than zero.
Both functions return `TRUE` for character-type
[zero-length](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
`x` if `allow_zerolength` is `TRUE`.

## See also

Other collections of checks on type and length:
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
[`is_natural()`](https://jessealderliesten.github.io/checkinput/reference/is_natural.md),
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
[`is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.md),
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
