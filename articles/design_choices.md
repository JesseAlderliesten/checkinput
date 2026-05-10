# Design choices regarding function input

## Introduction

The `is_<funcname>(x, ...)` and `all_<funcname>(x, ...)` functions of
`checkinput` return either `TRUE` or `FALSE` and do not throw errors for
any input to `x`: throwing an error about `x` is deferred to
[stopifnot](https://jessealderliesten.github.io/checkinput/help/stopifnot)
in which calls to functions from `checkinput` are typically wrapped when
they are used inside other functions. Errors *are* thrown about invalid
input to arguments other than `x`, e.g., providing values other than
`TRUE` or `FALSE` to `allow_zero`.

The default arguments make functions of `checkinput` more restrictive
than the equivalent functions in base R (e.g.,
[checkinput::is_logical()](https://jessealderliesten.github.io/checkinput/help/is_logical)
versus
[base::is.logical()](https://jessealderliesten.github.io/checkinput/help/is.logical)),
reflecting that the functions of `checkinput` are intended for argument
checking, where, for example, zero-length `x` is unwanted.

## Length of x

By default, `is_<funcname>(x)` only returns `TRUE` for `x` of length one
(with the obvious exception of
[is_zerolength(x)](https://jessealderliesten.github.io/checkinput/help/is_zerolength))
and `all_<funcname>(x)` only returns `TRUE` for `x` of length larger
than zero. Set argument `allow_zero` to `TRUE` to also return `TRUE` for
zero-length `x` of the correct type. See
[is_zerolength()](https://jessealderliesten.github.io/checkinput/help/is_zerolength)
and
[`vignette("type_coercion", package = "checkinput")`](https://jessealderliesten.github.io/checkinput/articles/type_coercion.md)
for a discussion of some issues with zero-length input.

## NAs in x

By default, functions only return `TRUE` for `x` without
[NA](https://jessealderliesten.github.io/checkinput/help/NA)s. Set
argument `allow_NA` to `TRUE` to also return `TRUE` for `x` containing
`NA`s of the correct type.

## Return

The `is_<funcname>(x)` and `all_<funcname>(x)` functions of `checkinput`
always return either `TRUE` or `FALSE`. To get a named boolean vector
indicating for each element of `x` if it `TRUE` or `FALSE` according to
`all_<funcname>(x)`, use
[vapply](https://jessealderliesten.github.io/checkinput/help/vapply)`(X = x, FUN.VALUE = logical(1), FUN = all_<funcname>, ...)`
instead of `all_<funcname>(x, ...)`. For example, to check which
elements of `x` are valid names, use
`vapply(X = x, FUN.VALUE = logical(1), FUN = all_names, ...)` instead of
[all_names](https://jessealderliesten.github.io/checkinput/help/all_names)`(x, ...)`.
The dots (`...`) indicate where to place other function arguments, e.g.,
`allow_underscores = FALSE`.
