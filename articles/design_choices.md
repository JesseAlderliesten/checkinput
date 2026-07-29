# Design choices

## Introduction and notation

This vignette explains the design choices of `checkinput`. It also shows
how to get a named boolean vector indicating for each element of `x` if
it is `TRUE` or `FALSE` according to the check functions.

## Type of x

The check functions of `checkinput` return either `TRUE` or `FALSE` and
do **not** throw errors for any input to `x`: throwing an error about
`x` is deferred to
[`base::stopifnot()`](https://rdrr.io/r/base/stopifnot.html) in which
calls to functions from `checkinput` are typically wrapped when they are
used inside other functions. Errors **are** thrown about invalid input
to arguments other than `x`, e.g., when values other than `TRUE` or
`FALSE` are used for `allow_NA`.

The default arguments make functions of `checkinput` more restrictive
than the equivalent functions in base R because the functions of
`checkinput` are intended for argument checking. For example,
[`checkinput::is_logical()`](https://jessealderliesten.github.io/checkinput/reference/is_logical.md)
by default returns `FALSE` for `logical(0)` because zero-length `x` is
usually unwanted in function arguments, whereas
[`base::is.logical()`](https://rdrr.io/r/base/logical.html) returns
`TRUE` for `logical(0)` because it has `type` logical.

## Length of x

By default, the check functions starting with `is_` only return `TRUE`
for `x` of length one (with the obvious exception of `is_zerolength(x)`)
and the check functions starting with `all_` return `TRUE` for `x` of
length larger than zero. Set argument `allow_zero` to `TRUE` to let them
also return `TRUE` for zero-length `x` of the correct type. See
[`help("is_zerolength")`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md)
and
[`vignette("type_coercion", package = "checkinput")`](https://jessealderliesten.github.io/checkinput/articles/type_coercion.md)
for a discussion of some issues with zero-length input.

## `NA` and `NaN` in x

By default, the check functions return `FALSE` for `x` containing `NA`
or `NaN`. Set argument `allow_NA` to `TRUE` to let them return `TRUE`
for `x` containing `NA` of the correct type, and set argument
`allow_NaN` in functions like
[`is_number()`](https://jessealderliesten.github.io/checkinput/reference/is_number.md)
to `TRUE` to let them return `TRUE` for `x` containing `NaN`.

## Return

The check functions of `checkinput` return either `TRUE` or `FALSE`. To
get a named boolean vector indicating for each element of `x` if it
`TRUE` or `FALSE` according to the check functions, use
`vapply(X = x, FUN.VALUE = logical(1), FUN = all_<func>)` instead of
`all_<func>(x)`, where `all_<func>` should be replaced with the relevant
function name. For example, to check which elements of `x` are valid
names, use `vapply(X = x, FUN.VALUE = logical(1), FUN = all_names)`
instead of `all_names(x)`. Other function arguments passed to
[`all_names()`](https://jessealderliesten.github.io/checkinput/reference/all_names.md),
e.g., `allow_underscores = FALSE` to not allow underscores, should be
placed behind argument `x`.
