# Check that x is Nearly Equal to Natural Numbers

Check element-wise near-equality to natural
[numbers](https://rdrr.io/r/base/numeric.html) while allowing for small
numeric errors.

## Usage

``` r
is_natural(
  x,
  strict = TRUE,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  allow_NaN = FALSE,
  allow_Inf = FALSE,
  tol = .Machine$double.eps^0.5
)

all_natural(
  x,
  strict = TRUE,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  allow_NaN = FALSE,
  allow_Inf = FALSE,
  tol = .Machine$double.eps^0.5
)

make_natural(
  x,
  strict = TRUE,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  allow_NaN = FALSE,
  allow_Inf = FALSE,
  tol = .Machine$double.eps^0.5,
  large_as_double = TRUE,
  all = FALSE
)
```

## Arguments

- x:

  object to check.

- strict:

  exclude zero from the natural numbers?

- allow_zerolength:

  `TRUE` or `FALSE`: allow
  [zero-length](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
  `x` of the correct [type](https://rdrr.io/r/base/typeof.html)?

- allow_NA:

  `TRUE` or `FALSE`: allow [`NA`](https://rdrr.io/r/base/NA.html) of the
  correct type in `x`? See the section 'Large Numbers' below on the
  disadvantage of using `TRUE` for `allow_NA` with large numbers.

- allow_NaN:

  `TRUE` or `FALSE`: allow
  [`NaN`](https://rdrr.io/r/base/is.finite.html)?

- allow_Inf:

  `TRUE` or `FALSE`: allow positive
  [`Inf`](https://rdrr.io/r/base/is.finite.html) in `x`? See the section
  'Large Numbers' below.

- tol:

  a small
  [positive](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
  number. Numbers that differ less than `tol` are considered equal.

- large_as_double:

  `TRUE` or `FALSE`: return an object of
  [type](https://rdrr.io/r/base/typeof.html) `double` if `x` contains
  values that are too large to be represented as
  [integer](https://rdrr.io/r/base/integer.html)? See the section 'Large
  Numbers' below.

- all:

  `TRUE` or `FALSE`: use `all_natural()` instead of `is_natural()`?

## Value

`is_natural()` and `all_natural()` return `TRUE` or `FALSE` indicating
if `x` is a vector of the appropriate length with only natural numbers
according to the values of their other arguments.

`make_natural()` throws an [error](https://rdrr.io/r/base/stop.html) if
`x` contains values that are not natural according to `is_natural()` or
`all_natural()`. Otherwise, `make_natural()` returns `x`,
[rounded](https://rdrr.io/r/base/Round.html) to a whole number. The
returned value is coerced to
[`type`](https://rdrr.io/r/base/typeof.html)
[`integer`](https://rdrr.io/r/base/integer.html) if `large_as_double` is
`FALSE` or `x` does not contain
[`Inf`](https://rdrr.io/r/base/is.finite.html) nor values larger than
[`.Machine$integer.max`](https://rdrr.io/r/base/zMachine.html) (about 2
\* 10^9), otherwise it is kept as
[`type`](https://rdrr.io/r/base/typeof.html)
[`double`](https://rdrr.io/r/base/double.html), see the section 'Large
Numbers' below.

## Details

Natural numbers are the positive integers (`1`, `2`, `3`, etc.). The
functions `*_natural()` allow for small numeric errors when comparing
numbers to the natural numbers, which is important because rounding
errors and representation errors during normal calculations can lead to
such small numeric errors. As the `Note` at
[`help("==")`](https://rdrr.io/r/base/Comparison.html) warns, using
`x == round(x)` does **not** allow for such errors but checks exact
equality, see the first `Examples`.

Zero is considered a natural number if argument `strict` is `FALSE`, but
even then small negative numbers are **not** considered natural numbers.

Although using `TRUE` for arguments `allow_zerolength`, `allow_NA`,
`allow_NaN`, or `allow_Inf` might be useful during specific programming
checks and are provided for consistency with
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md),
using `TRUE` for those arguments is **not** useful when testing that `x`
is a natural number before using it as an index.

`integer(0)` and `numeric(0)` are considered natural numbers if argument
`allow_zerolength` is `TRUE`. `make_natural()` then returns `integer(0)`
for both.

[`NA_integer_`](https://rdrr.io/r/base/NA.html) and `NA_real_` are
considered natural numbers if `allow_NA` is `TRUE`. `make_natural()`
then returns `NA_integer_` for both. Even then `NA_complex_` is **not**
considered a natural number because it is
[`complex`](https://rdrr.io/r/base/complex.html) instead of
[`numeric`](https://rdrr.io/r/base/numeric.html).

[`NaN`](https://rdrr.io/r/base/is.finite.html) (which has
[`mode`](https://rdrr.io/r/base/mode.html) `numeric`, despite meaning
'not a number') is considered a natural number if `allow_NaN` is `TRUE`.
`make_natural()` then returns `NA_integer_` for `NaN`.

Positive [`Inf`](https://rdrr.io/r/base/is.finite.html) is considered a
natural number if `allow_Inf` is `TRUE`. `make_natural()` then returns
positive `Inf` unchanged if `large_as_double` is `TRUE`, and
`NA_integer_` for positive `Inf` if `large_as_double` is `FALSE`.

[`NULL`](https://rdrr.io/r/base/NULL.html) and negative numbers are
**not** considered natural numbers and `make_natural()` throws an
[error](https://rdrr.io/r/base/stop.html) for them.

See the section `Large Numbers` below on the treatment of numbers that
are too large to be represented as integers.

## Large Numbers

Numbers larger than
[`.Machine$integer.max`](https://rdrr.io/r/base/zMachine.html) (about 2
\* 10^9) are too large to be represented as 32-bit
[integers](https://rdrr.io/r/base/integer.html) used by R. When such
large numbers are produced in [arithmetic
operations](https://rdrr.io/r/base/Arithmetic.html) that should result
in integers, they are coerced to
[`NA_integer_`](https://rdrr.io/r/base/NA.html) with the warning
'`NAs produced by integer overflow`'.

If argument `large_as_double` is `FALSE`, that approach is also followed
by `make_natural()`: then it coerces `Inf` and numbers that are too
large to be represented as `integer` to `NA_integer_`. If argument
`large_as_double` is `TRUE`, `make_natural()` returns the actual values
as [`type`](https://rdrr.io/r/base/typeof.html)
[`double`](https://rdrr.io/r/base/double.html) instead of `integer`,
with the warning '`The return of 'make_natural()' has type 'double'`'
that also indicates which values are too large.

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
to compare character vectors; help(`%/%`) on integer division; [R FAQ
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

# Zero is not considered a natural number if 'strict' is TRUE:
is_natural(1e-9, strict = TRUE)   # FALSE
#> [1] FALSE
is_natural(1e-9, strict = FALSE)  # TRUE
#> [1] TRUE
is_natural(-1e-9, strict = FALSE) # FALSE: wrong sign
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

# Handling of large numbers by make_natural()
large_double <- .Machine$integer.max + 1
make_natural(c(4, large_double), all = TRUE, large_as_double = TRUE)
#> Warning: The return of 'make_natural()' has type 'double' because 'large_as_double'
#> is 'TRUE' and a return of type 'integer' would produce 'NA_integer_'
#> through integer overflow for the following rounded values of 'x':
#> '2147483648'
#> [1]          4 2147483648
# c(4, 2147483648) with a warning that the return is of type 'double'
make_natural(c(4, large_double), all = TRUE, large_as_double = FALSE)
#> Warning: NAs introduced by coercion to integer range
#> [1]  4 NA
# c(4L, NA_integer_) with a warning that NAs have been introduced

is_natural(Inf, allow_Inf = FALSE) # FALSE
#> [1] FALSE
try(make_natural(Inf, allow_Inf = FALSE)) # Error
#> Error in make_natural(Inf, allow_Inf = FALSE) : 
#>   checkinput::is_natural(Inf) is not TRUE
is_natural(Inf, allow_Inf = TRUE) # TRUE
#> [1] TRUE
make_natural(Inf, allow_Inf = TRUE, large_as_double = TRUE)
#> Warning: The return of 'make_natural()' has type 'double' because 'large_as_double'
#> is 'TRUE' and a return of type 'integer' would produce 'NA_integer_'
#> through integer overflow for the following rounded values of 'x':
#> 'Inf'
#> [1] Inf
# Inf, with a warning that the return is of type 'double'
make_natural(Inf, allow_Inf = TRUE, large_as_double = FALSE)
#> Warning: NAs introduced by coercion to integer range
#> [1] NA
# NA_integer_, with a warning that NAs have been introduced
```
