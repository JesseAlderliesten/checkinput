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
  tol = .Machine$double.eps^0.5
)

all_natural(
  x,
  strict = TRUE,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  tol = .Machine$double.eps^0.5
)

make_natural(
  x,
  strict = TRUE,
  allow_zerolength = FALSE,
  allow_NA = FALSE,
  all = FALSE,
  tol = .Machine$double.eps^0.5,
  large_as_double = TRUE
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
  [`NaN`](https://rdrr.io/r/base/is.finite.html) is never allowed in
  `x`.

- tol:

  a small
  [positive](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
  number. Numbers that differ less than `tol` are considered equal.

- all:

  `TRUE` or `FALSE`: use `all_natural()` instead of `is_natural()`?

- large_as_double:

  `TRUE` or `FALSE`: return an object of
  [type](https://rdrr.io/r/base/typeof.html) `double` if `x` contains
  values that are too large to be represented as
  [integer](https://rdrr.io/r/base/integer.html)? See the section 'Large
  Numbers' below.

## Value

For `is_natural()` and `all_natural()`: `TRUE` or `FALSE` indicating if
`x` is a vector of the appropriate length with only natural numbers.

For `make_natural()`: `x`, [rounded](https://rdrr.io/r/base/Round.html)
to a whole number. The return has
[`type`](https://rdrr.io/r/base/typeof.html)
[`integer`](https://rdrr.io/r/base/integer.html), unless argument
`large_as_double` is `TRUE` and `x` contains values larger than
[`.Machine$integer.max`](https://rdrr.io/r/base/zMachine.html) (about 2
\* 10^9): then the return has type `double`, see the section 'Large
Numbers' below.

`make_natural()` **throws an error** if `x` contains values that differ
more than `tol` from the natural numbers.

## Details

Natural numbers are the positive integers (`1`, `2`, `3`, etc.). The
functions `*_natural()` allow for small numeric errors when comparing
numbers to the natural numbers, which is important because rounding
errors and representation errors during normal calculations can lead to
small numeric errors. As the `Note` at
[`help("==")`](https://rdrr.io/r/base/Comparison.html) warns, using
`x == round(x)` does **not** allow for such errors but checks exact
equality, incorrectly treating numbers as non-equal.

Zero is considered a natural number if argument `strict` is `FALSE`, but
even then small negative numbers are **not** considered natural numbers.

`integer(0)` and `numeric(0)` are considered natural numbers if argument
`allow_zerolength` is `TRUE`.

[`NA_integer_`](https://rdrr.io/r/base/NA.html) and `NA_real_` are
considered natural numbers if `allow_NA` is `TRUE`. Even then
`NA_complex_` is **not** considered a natural number because it is
[`complex`](https://rdrr.io/r/base/complex.html) instead of
[`numeric`](https://rdrr.io/r/base/numeric.html).

[`NULL`](https://rdrr.io/r/base/NULL.html),
[`NaN`](https://rdrr.io/r/base/is.finite.html), negative numbers, and
[`Inf`](https://rdrr.io/r/base/is.finite.html) are **not** considered
natural numbers.

See section `Large Numbers` below on the treatment of numbers that are
too large to be represented as integers.

## Large Numbers

Numbers larger than
[`.Machine$integer.max`](https://rdrr.io/r/base/zMachine.html) (about 2
\* 10^9) are too large to be represented as
[`integer`](https://rdrr.io/r/base/integer.html), see section 'Details'
in [`help("integer")`](https://rdrr.io/r/base/integer.html). When such
large numbers are produced in [arithmetic
operations](https://rdrr.io/r/base/Arithmetic.html) that should result
in integers, they are coerced to
[`NA_integer_`](https://rdrr.io/r/base/NA.html) with the warning
'`NAs produced by integer overflow`'. `NA_integer_` itself is considered
natural if `allow_NA = TRUE`, which is **not** useful when e.g., testing
that `x` is a natural number before using it as an index.

`make_natural()` also coerces numbers that are too large to be
represented as `integer` to `NA_integer_` if argument `large_as_double`
is `FALSE`. If argument `large_as_double` is `TRUE`, `make_natural()`
returns the actual values as
[`type`](https://rdrr.io/r/base/typeof.html)
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
to compare character vectors; [R FAQ
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
is_natural(x = 5 + 1e-10) # TRUE
#> [1] TRUE
# Zero is not considered a natural number if 'strict' is TRUE:
is_natural(x = 1e-10, strict = TRUE) # FALSE
#> [1] FALSE
try(make_natural(x = 1e-10, strict = TRUE)) # Error
#> Error in make_natural(x = 1e-10, strict = TRUE) : 
#>   checkinput::is_natural(1e-10) is not TRUE
is_natural(x = 1e-10, strict = FALSE) # TRUE
#> [1] TRUE
make_natural(x = 1e-10, strict = FALSE) # 0
#> [1] 0
is_natural(x = -1e-10, strict = FALSE) # FALSE: wrong sign
#> [1] FALSE
is_natural(x = Inf, strict = FALSE) # FALSE
#> [1] FALSE
is_natural(x = "a") # FALSE
#> [1] FALSE
is_natural(x = 1:2) # FALSE: wrong length
#> [1] FALSE

# Allowing for small numeric errors is important
x <- sqrt(2)^2
is_natural(x = x) # TRUE
#> [1] TRUE
x == 2 # FALSE!
#> [1] FALSE
x - 2 # about 4.44e-16
#> [1] 4.440892e-16

all_natural(x = c(3, 5 + 1e-10)) # TRUE
#> [1] TRUE
try(make_natural(x = c(3, 5 + 1e-10))) # error: wrong length
#> Error in make_natural(x = c(3, 5 + 1e-10)) : 
#>   checkinput::is_natural(c(3, 5 + 1e-10)) is not TRUE
make_natural(x = c(3, 5 + 1e-10), all = TRUE) # c(3L, 5L)
#> [1] 3 5
# Zero is not considered a natural number if 'strict' is TRUE:
all_natural(x = c(1e-10, 3, 5), strict = TRUE) # FALSE
#> [1] FALSE
all_natural(x = c(1e-10, 3, 5), strict = FALSE) # TRUE
#> [1] TRUE
# FALSE because of wrong sign:
all_natural(x = c(-1e-10, 3, 5), strict = FALSE)
#> [1] FALSE
all_natural(x = c(3, 5, Inf), strict = FALSE) # FALSE
#> [1] FALSE
all_natural(x = "a") # FALSE
#> [1] FALSE
all_natural(x = 1:2) # TRUE
#> [1] TRUE

# Handling of large numbers by make_natural(), using a larger
# tolerance to prevent the small value that is added to
# '.Machine$integer.max + 1' from vanishing.
large_tol <- .Machine$double.eps^0.25
large_double <- .Machine$integer.max + 1 + 0.01 * large_tol

# c(4, 2147483648) with a warning that the return is of type 'double':
make_natural(x = c(4, large_double), all = TRUE,
             tol = large_tol, large_as_double = TRUE)
#> Warning: The return of 'make_natural()' has type 'double' because 'large_as_double'
#> is 'TRUE' and a return of type 'integer' would produce 'NA_integer_'
#> through integer overflow for the following rounded values of 'x':
#> '2147483648'
#> [1]          4 2147483648
# c(4L, NA_integer_) (printed as c(4, NA)) with a warning that
# NAs have been introduced by coercion to integer range:
make_natural(x = c(4, large_double), all = TRUE,
             tol = large_tol, large_as_double = FALSE)
#> Warning: NAs introduced by coercion to integer range
#> [1]  4 NA

# Illustrate the need to follow the use of is_natural(x) or
# all_natural(x) by assigning the rounded value to the argument
toy_fun_erroneous <- function(x) {
  stopifnot(is_natural(x))
  seq_len(x)
}

toy_fun_correct <- function(x) {
  stopifnot(is_natural(x))
  x <- round(x)
  seq_len(x)
}

toy_fun_safe <- function(x, all = TRUE) {
  x <- make_natural(x, all = all)
  seq_len(x)
}

toy_fun_erroneous(x = 5 - 1e-8) # 1:4
#> [1] 1 2 3 4
toy_fun_correct(x = 5 - 1e-8) # 1:5
#> [1] 1 2 3 4 5
toy_fun_safe(x = 5 - 1e-8) # 1:5
#> [1] 1 2 3 4 5

# Error: is_natural(x) is not TRUE
try(toy_fun_erroneous(x = 5.1))
#> Error in toy_fun_erroneous(x = 5.1) : is_natural(x) is not TRUE
try(toy_fun_correct(x = 5.1))
#> Error in toy_fun_correct(x = 5.1) : is_natural(x) is not TRUE
try(toy_fun_safe(x = 5.1, all = FALSE))
#> Error in make_natural(x, all = all) : 
#>   checkinput::is_natural(x) is not TRUE

# Error: all_natural(x) is not TRUE
try(toy_fun_safe(x = 5.1, all = TRUE))
#> Error in make_natural(x, all = all) : 
#>   checkinput::all_natural(x) is not TRUE
```
