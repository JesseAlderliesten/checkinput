# Quote and concatenate x to a string

Quote elements of a dimensionless atomic object and concatenate the
result to a single character string.

## Usage

``` r
paste_quoted(x)
```

## Arguments

- x:

  Dimensionless atomic object to be converted to a single character
  string.

## Value

A character string consisting of the elements of `x` surrounded by
single quotes, separated by commas. See `Details` on the handling of
some special values.

## Details

`paste_quoted()` returns `NULL` as `"'NULL'"`, other zero-length objects
as `"'<class>(0)'"` (e.g., `"'logical(0)'"`), `""` as `'""'`, and
non-logical `NA`s as `"'NA_<class>_'"` (e.g., `"'NA_real_'"`; for
[factors](https://rdrr.io/r/base/factor.html) this is
`"'NA_character_'"`).

## Notes

An error occurs if multiple arguments are provided because then `x`
probably was accidentally not [combined](https://rdrr.io/r/base/c.html).
For example, the call `paste_quoted("a", "b")` will return the error
`unused argument ("b")`. The probably intended call is
`paste_quoted(c("a", "b"))`, returning `"'a', 'b'"`.

`paste_quoted()` drops [names](https://rdrr.io/r/base/names.html) of
`x`, which is pointed out in a
[warning](https://rdrr.io/r/base/warning.html) if `x` has names.

## See also

[`toString()`](https://rdrr.io/r/base/toString.html) which can be used
instead of `paste(x, collapse = ", ")`,
[`sQuote()`](https://rdrr.io/r/base/sQuote.html) to use fancy quotes,
[`paste0()`](https://rdrr.io/r/base/paste.html),
[`progutils::unpaste_unquote()`](https://jessealderliesten.github.io/progutils/reference/unpaste_unquote.html)
for the approximate opposite of `paste_quoted()`.

## Examples

``` r
paste_quoted(c(3, 4)) # "'3', '4'"
#> [1] "'3', '4'"
paste_quoted(NULL) # "'NULL'"
#> [1] "'NULL'"
paste_quoted(c(a = 3, b = 4)) # "'3', '4'" # Warns about dropping names.
#> Warning: 'x' has names, these will be discarded.
#> Use progutils::vect_to_char() instead of paste_quoted() to preserve names of numeric 'x'.
#> [1] "'3', '4'"
```
