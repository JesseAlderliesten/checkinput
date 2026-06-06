# Check that `x` is nearly equal to natural numbers

Check element-wise near-equality to natural numbers while allowing for
small numeric errors.

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
  tol = .Machine$double.eps^0.5
)
```

## Arguments

- x:

  object to check.

- strict:

  Exclude zero from the natural numbers?

- allow_zerolength:

  `TRUE` or `FALSE`: allow
  [zero-length](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
  `x` of the correct type?

- allow_NA:

  `TRUE` or `FALSE`: allow [NA](https://rdrr.io/r/base/NA.html)s of the
  correct type in `x`?

- tol:

  A small
  [positive](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
  number. Numbers that differ less than `tol` are considered equal.

- all:

  `TRUE` or `FALSE`: use `all_natural()` instead of `is_natural()`?

## Value

For `is_natural()` and `all_natural()`: `TRUE` or `FALSE` indicating if
`x` is a vector of the appropriate length with only natural numbers. For
`make_natural()`: `x`, [rounded](https://rdrr.io/r/base/Round.html) to a
whole number and coerced to
[integer](https://rdrr.io/r/base/integer.html) type.

## Details

Natural numbers are the positive integers (`1`, `2`, `3`, etc.).
`is_natural()` and `all_natural()` allow for small numeric errors when
comparing numbers to the natural numbers. Such numeric errors can arise
because of rounding or representation error. As the `Note` at
[`==`](https://rdrr.io/r/base/Comparison.html) warns, `x == round(x)`
does **not** allow for such errors but checks exact equality.

Zero is considered a natural number if argument `strict` is `FALSE`, but
even then small negative numbers are **not** considered natural numbers.

`integer(0)` and `numeric(0)` are considered natural numbers if argument
`allow_zerolength` is `TRUE`, and `NA_integer_` and `NA_real_` are
considered natural numbers if `allow_NA` is `TRUE` (even then
`NA_complex_` is **not** considered a natural number because its mode is
`complex` instead of `numeric`).

[NULL](https://rdrr.io/r/base/NULL.html),
[NaN](https://rdrr.io/r/base/is.finite.html), negative numbers, `Inf`,
and numbers that are [too large](https://rdrr.io/r/base/zMachine.html)
to be represented as [integers](https://rdrr.io/r/base/integer.html) are
**never** considered natural numbers.

## Notes

`make_natural(x, all = FALSE)` and `make_natural(x, all = TRUE)` throw
an error if `x` is not natural according to `is_natural(x)` or
`all_natural(x)`, respectively. Therefore, their result can be assigned
to `x` without the need to use
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html).

Alternatively, use `is_natural(x)` or `all_natural(x)` inside
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html), followed by
assigning the rounded value to `x` after converting it to type
`integer`: `x <- as.integer(round(x))`.

## Programming notes

The code of `is_natural()` and `all_natural()` is partly based on the
example `is.wholenumber()` in
[`is.integer()`](https://rdrr.io/r/base/integer.html).

[`is.integer()`](https://rdrr.io/r/base/integer.html) does **not** check
that `x` is a natural number (nor if `x` is a whole number) but rather
that `x` is of [type](https://rdrr.io/r/base/typeof.html) integer, see
the `Note` in [`is.integer()`](https://rdrr.io/r/base/integer.html).

## See also

[`progutils::are_equal()`](https://jessealderliesten.github.io/progutils/reference/are_equal.html)
to check for element-wise near-equality of numbers;
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) to check more
generally for near-equality;
[`identical()`](https://rdrr.io/r/base/identical.html) to check for
exact equality and [Comparison](https://rdrr.io/r/base/Comparison.html)
to do so using binary operators;
[`match()`](https://rdrr.io/r/base/match.html) and
[`progutils::not_in()`](https://jessealderliesten.github.io/progutils/reference/not_in.html)
to compare character vectors; [R FAQ
7.31](https://CRAN.R-project.org/doc/manuals/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f)
for background on numerical equality.

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
try(make_natural(x = c(3, 5 + 1e-10))) # c(3L, 5L)
#> Error in make_natural(x = c(3, 5 + 1e-10)) : 
#>   checkinput::is_natural(c(3, 5 + 1e-10)) is not TRUE
# Zero is not considered a natural number if 'strict' is TRUE:
all_natural(x = c(1e-10, 3, 5), strict = TRUE) # FALSE
#> [1] FALSE
all_natural(x = c(1e-10, 3, 5), strict = FALSE) # TRUE
#> [1] TRUE
all_natural(x = c(-1e-10, 3, 5), strict = FALSE) # FALSE: wrong sign
#> [1] FALSE
all_natural(x = c(3, 5, Inf), strict = FALSE) # FALSE
#> [1] FALSE
all_natural(x = "a") # FALSE
#> [1] FALSE
all_natural(x = 1:2) # TRUE
#> [1] TRUE

# Illustrate the need to follow use of is_natural(x) or all_natural(x) by
# assigning the rounded value to the argument
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

try(toy_fun_erroneous(x = 5.1)) # Error: is_natural(x) is not TRUE
#> Error in toy_fun_erroneous(x = 5.1) : is_natural(x) is not TRUE
try(toy_fun_correct(x = 5.1)) # Error: is_natural(x) is not TRUE
#> Error in toy_fun_correct(x = 5.1) : is_natural(x) is not TRUE
try(toy_fun_safe(x = 5.1, all = FALSE)) # Error: is_natural(x) is not TRUE
#> Error in make_natural(x, all = all) : 
#>   checkinput::is_natural(x) is not TRUE
try(toy_fun_safe(x = 5.1, all = TRUE)) # Error: all_natural(x) is not TRUE
#> Error in make_natural(x, all = all) : 
#>   checkinput::all_natural(x) is not TRUE
```
