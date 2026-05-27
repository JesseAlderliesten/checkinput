
<!-- badges: start -->

![](https://img.shields.io/github/r-package/v/JesseAlderliesten/checkinput?color=blue)
[![R-CMD-check](https://github.com/JesseAlderliesten/checkinput/actions/workflows/check-no-suggests.yaml/badge.svg)](https://github.com/JesseAlderliesten/checkinput/actions/workflows/check-no-suggests.yaml)
<!-- badges: end -->

# checkinput

With `checkinput`, you can write concise but flexible checks for input
to R functions.

## Installation

Visit the [checkinput
website](https://jessealderliesten.github.io/checkinput/) to explore the
package, or install `checkinput` from
[GitHub](https://github.com/JesseAlderliesten/checkinput) using the
following R code (you need to run R as administrator):

``` r
if(!requireNamespace("remotes", quietly = TRUE)) {
  install.packages(pkgs = "remotes", quiet = FALSE)
}
remotes::install_github(repo = "JesseAlderliesten/checkinput",
                        dependencies = NA, upgrade = FALSE, force = FALSE,
                        quiet = FALSE, build_vignettes = TRUE, lib = NULL,
                        verbose = getOption("verbose"))
```

For more information about installing and configuring R and RStudio, see
my package
[checkrpkgs](https://jessealderliesten.github.io/checkrpkgs/).

## Example

Say you want to collect information on people’s hobbies through a
function in which it is optional for them to provide their name. With
`checkinput`, you could write a function like `list_hobbies()`:

``` r
list_hobbies <- function(name, age, hobbies) {
  stopifnot(is_character(name, allow_empty = TRUE, allow_NA = TRUE),
            is_nonnegative(age), all_characters(hobbies))
  list(name = name, age = age, hobbies = hobbies)
}
```

The checks inside `stopifnot()` ensure that (1) `name` contains a single
character string that might be empty (`""`) or character-type `NA`
(`NA_character_`) if people do not want to give their name; (2) `age`
contains a single non-negative number; (3) `hobbies` contains at least
one character string and does not contain any empty strings or `NA`s.

The base R equivalent of `list_hobbies()` would require much more code
to check the input, increasing the chance of coding errors and making it
more difficult to read:

``` r
list_hobbies_base <- function(name, age, hobbies) {
  stopifnot(is.character(name), is.atomic(name), is.null(dim(name)),
            length(name) == 1L,
            is.numeric(age), is.atomic(age), is.null(dim(age)), length(age) == 1L,
            nzchar(x = age, keepNA = FALSE), !is.na(age), age >= 0L,
            is.character(hobbies), is.atomic(hobbies), is.null(dim(hobbies)),
            length(hobbies) > 0L, all(nzchar(hobbies, keepNA = FALSE)),
            !anyNA(hobbies))
  list(name = name, age = age, hobbies = hobbies)
}
```

If the input passes all checks, both functions produce the same output:

``` r
library(checkinput)
hobbies_John <- c("books", "construction sets")
John_checkinput <- list_hobbies(name = "John", age = 25, hobbies = hobbies_John)
John_base <- list_hobbies_base(name = "John", age = 25, hobbies = hobbies_John)
identical(John_checkinput, John_base)
#> [1] TRUE

hobbies_baby <- "drinking milk"
baby_checkinput <- list_hobbies(name = "", age = 0, hobbies = hobbies_baby)
baby_base <- list_hobbies_base(name = "", age = 0, hobbies = hobbies_baby)
identical(baby_checkinput, baby_base)
#> [1] TRUE
```

When a check fails, error messages can be more informative when using
`checkinput`:

``` r
try(list_hobbies(name = "John", age = 25, hobbies = c(hobbies_John, "")))
#> Error in list_hobbies(name = "John", age = 25, hobbies = c(hobbies_John,  : 
#>   all_characters(hobbies) is not TRUE
try(list_hobbies_base(name = "John", age = 25, hobbies = c(hobbies_John, "")))
#> Error in list_hobbies_base(name = "John", age = 25, hobbies = c(hobbies_John,  : 
#>   all(nzchar(hobbies, keepNA = FALSE)) is not TRUE

try(list_hobbies(name = "", age = -1, hobbies = hobbies_baby))
#> Error in list_hobbies(name = "", age = -1, hobbies = hobbies_baby) : 
#>   is_nonnegative(age) is not TRUE
try(list_hobbies_base(name = "", age = -1, hobbies = hobbies_baby))
#> Error in list_hobbies_base(name = "", age = -1, hobbies = hobbies_baby) : 
#>   age >= 0L is not TRUE
```

### Design choices

The `is_<func>(x, ...)` and `all_<func>(x, ...)` functions of
`checkinput` return either `TRUE` or `FALSE` and do **not** throw errors
for any input to `x`. Errors **are** thrown about invalid input to
arguments other than `x`, e.g., when values other than `TRUE` or `FALSE`
are used for `allow_NA`. This is explained in the [vignette about design
choices](https://jessealderliesten.github.io/checkinput/articles/design_choices.html):
`vignette("design_choices", package = "checkinput")`. That vignette also
shows how to get a named boolean vector indicating for each element of
`x` if it is `TRUE` or `FALSE` according to `all_<func>(x)`.

`checkinput` also contains a [vignette about type
coercion](https://jessealderliesten.github.io/checkinput/articles/type_coercion.html)
in vectors, which discusses some important aspects regarding testing of
vectors: `vignette("type_coercion", package = "checkinput")`.

## Similar packages

Functions of `checkinput` use arguments to determine which special
values should be allowed, which makes them more flexible than functions
in similar packages. Nevertheless, the following similar packages are
worth looking into:

- [arkhe](https://CRAN.R-project.org/package=arkhe): tools for cleaning
  rectangular data.
- [assertable](https://CRAN.R-project.org/package=assertable): verbose
  assertions for tabular data (data.frames and data.tables).
- [assertthat](https://CRAN.R-project.org/package=assertthat): easy pre
  and post assertions.
- [checkmate](https://CRAN.R-project.org/package=checkmate): fast and
  versatile argument checks
- [chk](https://CRAN.R-project.org/package=chk): check user-supplied
  function arguments
- [erify](https://CRAN.R-project.org/package=erify): check arguments and
  generate readable error messages
