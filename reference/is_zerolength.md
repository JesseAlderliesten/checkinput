# Check that x is zero-length

Check that x is zero-length

## Usage

``` r
is_zerolength(x)
```

## Arguments

- x:

  object to test.

## Value

`TRUE` or `FALSE` indicating if `x` is a zero-length object.

## Details

No check is performed on dimensions, such that a
[`matrix()`](https://rdrr.io/r/base/matrix.html) with zero rows is a
zero-length object whereas a
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) with zero rows
is **not** (see the `Examples`).

## Notes

[Zero-length](https://rdrr.io/r/base/length.html) objects can have
different [types](https://rdrr.io/r/base/typeof.html): NULL
([NULL](https://rdrr.io/r/base/NULL.html)), logical (`logical(0)`),
integer (`integer(0)`), double (`numeric(0)`), complex (`complex(0)`),
character (`character(0)`), and list
([`list()`](https://rdrr.io/r/base/list.html) and
[`data.frame()`](https://rdrr.io/r/base/data.frame.html)). `""` is not a
zero-length object: it has a `length` of one despite its
[width](https://rdrr.io/r/base/nchar.html) of zero characters. A
dataframe with zero rows is *not* a zero-length object: it has `length`
equal to the number of columns.

[`is.null()`](https://rdrr.io/r/base/NULL.html) should be used to check
that an object is `NULL` and, more generally,
`isTRUE(all.equal(x, <zero-length object>))` should be used to check
equality to a zero-length object. Testing equality should **not** be
done by using [==](https://rdrr.io/r/base/Comparison.html) because that
leads to `logical(0)` if any of any sides contains a zero-length object,
which gives an error when used as complete [conditional
statement](https://rdrr.io/r/base/Control.html).

`all(logical(0))` returns `TRUE`, see the `Note` in
[`all()`](https://rdrr.io/r/base/all.html). This is also the case for
`all(numeric(0))` and `all(character(0))` that get coerced to type
`logical`.

Although zero-length objects are discarded when combined into a vector
with other values, their types are taken into account for type coercion,
see the vignette mentioned in the `See also` section. For example,
numeric `314` will be coerced to character `"314"` when it is combined
into a vector with zero-length `character(0)`, such that
`c(314, character(0))` results in the character string `"314"`, not in
the numeric value `314`. If zero-length objects are combined into a
vector with only zero-length values, the type of the vector is the
highest type of the components in the hierarchy `NULL` \< `raw` \<
`logical` \< `integer` \< `double` \< `complex` \< `character` \< `list`
\< `expression`, see the section `Details` in
[`help(c)`](https://rdrr.io/r/base/c.html).

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
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md)

## Examples

``` r
is_zerolength(x = character(0)) # TRUE
#> [1] TRUE
is_zerolength(x = 0) # FALSE
#> [1] FALSE
# A matrix with zero rows *is* a zero-length object ...
is_zerolength(x = as.matrix(data.frame(a = 314))[numeric(0), , drop = FALSE])
#> [1] TRUE
# ... whereas a dataframe with zero rows is *not* a zero-length object.
is_zerolength(x = data.frame(a = 314)[numeric(0), , drop = FALSE])
#> [1] FALSE

# Zero-length objects affect type coercion.
num <- 314
str(num) # num 314
#>  num 314
zerochar <- character(0)
str(zerochar) # chr(0)
#>  chr(0) 
str(c(num, zerochar)) # chr "314", not num 314
#>  chr "314"
```
