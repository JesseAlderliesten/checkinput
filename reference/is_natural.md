# Check that x is Nearly Equal to Natural Numbers

Check element-wise near-equality to natural
[numbers](https://rdrr.io/r/base/numeric.html) while allowing for small
numeric errors.

## Usage

``` r
is_natural(
  x,
  include_zero = FALSE,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  allow_NaN = FALSE,
  tol = .Machine$double.eps^0.5
)

all_natural(
  x,
  include_zero = FALSE,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  allow_NaN = FALSE,
  tol = .Machine$double.eps^0.5
)

make_natural(
  x,
  include_zero = FALSE,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  allow_NaN = FALSE,
  tol = .Machine$double.eps^0.5,
  all = FALSE
)
```

## Arguments

- x:

  object to check.

- include_zero:

  include zero among the natural numbers?

- allow_zerolength:

  `TRUE` or `FALSE`: allow
  [zero-length](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
  `x` of the correct [type](https://rdrr.io/r/base/typeof.html)?

- allow_NA:

  `TRUE` or `FALSE`: allow
  [`NA_integer_`](https://rdrr.io/r/base/NA.html) and
  [`NA_real_`](https://rdrr.io/r/base/NA.html) in `x`?

- allow_NaN:

  `TRUE` or `FALSE`: allow
  [`NaN`](https://rdrr.io/r/base/is.finite.html) in `x`?

- tol:

  a
  [positive](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
  number smaller than `0.5`. Numbers that differ less than `tol` are
  considered equal.

- all:

  `TRUE` or `FALSE`: use `all_natural()` instead of `is_natural()`?

## Value

`is_natural()` and `all_natural()` return `TRUE` or `FALSE` indicating
if `x` is a vector of the appropriate length with only natural numbers
according to the values of their other arguments.

`make_natural(x, ..., all = FALSE)` and
`make_natural(x, ..., all = TRUE)` throw an
[error](https://rdrr.io/r/base/stop.html) if `x` contains values that
are not natural according to `is_natural(x, ...)` or
`all_natural(x, ...)`, respectively. If no error is thrown,
`make_natural(x, ...)` [rounds](https://rdrr.io/r/base/Round.html) `x`
to a whole number and coerces it to
[`type`](https://rdrr.io/r/base/typeof.html)
[`integer`](https://rdrr.io/r/base/integer.html), which might lead to
the return containing `integer(0)` or `NA_integer_`, see 'Details'.

## Details

Natural numbers are the positive integers (`1`, `2`, `3`, etc.). The
functions `*_natural()` allow for small numeric errors when comparing
numbers to the natural numbers, which is important because rounding
errors and representation errors during normal calculations can lead to
such small numeric errors, see the first `Examples`. As the `Note` at
[`help("==")`](https://rdrr.io/r/base/Comparison.html) warns, using
`x == round(x)` does **not** allow for such errors but checks exact
equality, see the first `Examples`.

Zero is considered a natural number if argument `include_zero` is
`TRUE`. `make_natural()` then returns `0L` for it. Even then small
negative numbers are **not** considered natural numbers.

`integer(0)` and `numeric(0)` are considered natural numbers if argument
`allow_zerolength` is `TRUE`. `make_natural()` then returns `integer(0)`
for both.

[`NA_integer_`](https://rdrr.io/r/base/NA.html) and `NA_real_` are
considered natural numbers if `allow_NA` is `TRUE`. `make_natural()`
then returns `NA_integer_` for both. Even then `NA_complex_` is **not**
considered a natural number because it is
[`complex`](https://rdrr.io/r/base/complex.html) instead of
[`numeric`](https://rdrr.io/r/base/numeric.html).

[`NaN`](https://rdrr.io/r/base/is.finite.html) is considered a natural
number if `allow_NaN` is `TRUE`. `make_natural()` then returns
`NA_integer_` for `NaN`.

[`NULL`](https://rdrr.io/r/base/NULL.html), negative numbers,
[`Inf`](https://rdrr.io/r/base/is.finite.html), and numbers larger than
`.Machine$integer.max` are **not** considered natural numbers.
`make_natural()` throws an [error](https://rdrr.io/r/base/stop.html) for
them.

## Warning

`TRUE` should be used **cautiously** for arguments `include_zero`,
`allow_zerolength`, `allow_NA`, and `allow_NaN` because `is_natural()`
and `all_natural()` then return `TRUE` and `make_natural()` then returns
`0L` for `0L` and `0`; `integer(0)` for `integer(0)` and `numeric(0)`;
and `NA_integer_` for `NA_integer_`, `NA_real_`, or `NaN` in `x`.

Such returns make it **impossible** to use the `*_natural()` functions
to confirm that `x` can be used in places where whole numbers are
expected, for example when using `x` to create a range through
[`seq()`](https://rdrr.io/r/base/seq.html) or
[`:`](https://rdrr.io/r/base/Colon.html) to create a counter or to
[Extract](https://rdrr.io/r/base/Extract.html) parts of an object.

Argument `include_zero` is included because 0 is considered a natural
number in some definitions of a natural number. Arguments
`allow_zerolength`, `allow_NA` and `allow_NaN` are provided for
consistency with functions like
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md).

## Programming Notes

`make_natural(x, ..., all = FALSE)` and
`make_natural(x, ..., all = TRUE)` throw an error if `x` is not natural
according to `is_natural(x, ...)` or `all_natural(x, ...)`,
respectively. Therefore, their result can be assigned to `x` without the
need to use [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html).

[`is.integer()`](https://rdrr.io/r/base/integer.html) does **not** check
that `x` is a natural number (nor that `x` is a whole number) but rather
that `x` is of [type](https://rdrr.io/r/base/typeof.html) integer, see
the section 'Note' in
[`help("is.integer")`](https://rdrr.io/r/base/integer.html).

The check for near-equality to the natural numbers in the last line of
code of `all_natural()` is partly based on function `is.wholenumber()`
from the `Examples` of
[`is.integer()`](https://rdrr.io/r/base/integer.html).

Numbers larger than
[`.Machine$integer.max`](https://rdrr.io/r/base/zMachine.html) (about 2
\* 10^9) are too large to be represented as 32-bit
[integers](https://rdrr.io/r/base/integer.html) used by R. Such large
numbers are **not** considered natural by `is_natural()` and
`all_natural()`, such that `make_natural()` throws an error for them.
This prevents an inconsistency between `is_natural()` and
`all_natural()` returning `TRUE` for such numbers if they differ less
than `tol` from the natural numbers while `make_natural()` would return
`NA_integer_` for them because they would be coerced to `type`
`integer`, with the warning '`NAs produced by integer overflow`'.
Letting `make_natural()` convert such large numbers to `type` `double`
to extend the range of whole numbers that can be handled by the
`*_natural` functions, the approach used in **checkinput** 1.7.0 if
argument `large_as_double` was `TRUE`, would create the problem that the
numbers are **not** exact anymore, while creating exact numbers is the
point of the `*_natural` functions.

## See also

[`progutils::are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.html)
to check for element-wise near-equality of numbers;
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) to check more
generally for near-equality;
[`identical()`](https://rdrr.io/r/base/identical.html) to check for
exact equality and
[`Comparison`](https://rdrr.io/r/base/Comparison.html) to do so using
binary operators; [`match()`](https://rdrr.io/r/base/match.html) and
[`progutils::not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.html)
to compare character vectors;
[`help(Arithmetic)`](https://rdrr.io/r/base/Arithmetic.html) on using
`%/%` for integer division; section 'Warning' in
[`round()`](https://rdrr.io/r/base/Round.html) and [R FAQ
7.31](https://CRAN.R-project.org/doc/manuals/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
for background on numerical equality and its pitfalls.

Other collections of checks on type and length:
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
[`is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md),
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
[`is_path()`](https://jessealderliesten.github.io/checkinput/reference/is_path.md),
[`is_zerolength()`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)

## Examples

``` r
# Allowing for small numeric errors is important
x <- sqrt(3)^2
x == 3        # FALSE!
#> [1] FALSE
x - 3         # about -4.44e-16
#> [1] -4.440892e-16
is_natural(x) # TRUE
#> [1] TRUE
seq_len(x)    # 1:2 (incorrect)
#> [1] 1 2
seq_len(make_natural(x)) # 1:3 (correct)
#> [1] 1 2 3

# Normal behaviour
is_natural(5 + 1e-9, tol = 1e-8) # TRUE
#> [1] TRUE
is_natural(5 + 1e-7, tol = 1e-8) # FALSE
#> [1] FALSE
is_natural(1:2)  # FALSE: wrong length
#> [1] FALSE
all_natural(1:2) # TRUE
#> [1] TRUE
is_natural("a")  # FALSE: wrong type
#> [1] FALSE

# Zero is considered a natural number if 'include_zero' is TRUE:
is_natural(1e-9, include_zero = FALSE) # FALSE
#> [1] FALSE
is_natural(1e-9, include_zero = TRUE)  # TRUE
#> [1] TRUE
is_natural(-1e-9, include_zero = TRUE) # FALSE: wrong sign
#> [1] FALSE

# Zero-length input
is_natural(integer(0), allow_zerolength = FALSE) # FALSE
#> [1] FALSE
is_natural(integer(0), allow_zerolength = TRUE)  # TRUE
#> [1] TRUE
is_natural(numeric(0), allow_zerolength = FALSE) # FALSE
#> [1] FALSE
is_natural(numeric(0), allow_zerolength = TRUE)  # TRUE
#> [1] TRUE
is_natural(logical(0), allow_zerolength = TRUE)  # FALSE: wrong type
#> [1] FALSE
is_natural(NULL, allow_zerolength = TRUE)        # FALSE: wrong type
#> [1] FALSE

# make_natural()
make_natural(5 + 1e-9) # 5L
#> [1] 5
try(make_natural(5 + 1e-7)) # error: checkinput::is_natural(5 + 1e-07) is not TRUE
#> Error in make_natural(5 + 1e-07) : 
#>   checkinput::is_natural(5 + 1e-07) is not TRUE
try(make_natural(c(3, 5 + 1e-9), all = FALSE)) # error: wrong length
#> Error in make_natural(c(3, 5 + 1e-09), all = FALSE) : 
#>   checkinput::is_natural(c(3, 5 + 1e-09)) is not TRUE
make_natural(c(3, 5 + 1e-9), all = TRUE) # c(3L, 5L)
#> [1] 3 5

# Handling of large numbers
large_double <- .Machine$integer.max + 1
is_natural(large_double) # FALSE
#> [1] FALSE
try(make_natural(c(4, large_double), all = TRUE))
#> Error in make_natural(c(4, large_double), all = TRUE) : 
#>   checkinput::all_natural(c(4, large_double)) is not TRUE
# error: checkinput::all_natural(c(4, large_double)) is not TRUE
is_natural(Inf) # FALSE
#> [1] FALSE
try(make_natural(Inf))
#> Error in make_natural(Inf) : checkinput::is_natural(Inf) is not TRUE
# Error: checkinput::is_natural(Inf) is not TRUE
```
