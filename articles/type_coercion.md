# Type coercion

## Introduction

Objects in **R** are converted (‘coerced’) to a common type when
combined in a vector, see [`help("c")`](https://rdrr.io/r/base/c.html).
The type of the vector, and thus of all its components, will be the
highest type of the components in the hierarchy `NULL` \< `raw` \<
`logical` \< `integer` \< `double` \< `complex` \< `character` \< `list`
\< `expression`, see the section ‘Details’ in
[`help("c")`](https://rdrr.io/r/base/c.html) and the section ‘Value’ in
[`help("typeof")`](https://rdrr.io/r/base/typeof.html). For example,
numeric `314` will be coerced to character `"314"` when it is combined
in a vector with character `"nco"`, such that `c(314, "nco")` results in
the character vector `c("314", "nco")`. This also holds for the logical
`NA`, which will be coerced to `NA_character_` that is printed as `NA`,
as is the logical `NA`.

``` r

num <- 314
str(num)
#>  num 314
char <- "nco"
str(char)
#>  chr "nco"
str(c(num, char))
#>  chr [1:2] "314" "nco"

logiNA <- NA
str(logiNA)
#>  logi NA
logiNA_char <- c(logiNA, char)
str(logiNA_char)
#>  chr [1:2] NA "nco"
str(logiNA_char[1])
#>  chr NA
logiNA_char[1]
#> [1] NA
```

## Consequences for checks

As a consequence of type coercion, code like `all_characters(c(x, y))`
does **not** check if all elements in `x` and `y` have type `character`:
`x` and `y` will be coerced to the highest of their types before the
check is performed, such that `all_characters(c(x, y))` checks if any of
`x` or `y` has type `character` **and** none of `x` or `y` has a higher
type.

To check if all elements in `x` and `y` have type `character`, use
`all_characters(x) && all_characters(y)` or, to generalise more easily
to more than two objects,
`all(unlist(lapply(X = list(x, y), FUN = all_characters)))`. Other
function arguments passed to
[`all_characters()`](https://jessealderliesten.github.io/checkinput/reference/all_characters.md),
e.g., `allow_empty = TRUE` to allow empty quotes (`""`), should be
placed behind argument `FUN`.

Elements of a list can have different types. Using
[`unlist()`](https://rdrr.io/r/base/unlist.html) on a list creates a
vector, coercing all elements of the list to the highest of their types,
such that `all_characters(unlist(z))` does **not** check if all elements
of list `z` have type `character` but if any element of `z` has type
`character` **and** none of the elements of `z` has a higher type.

``` r

library(checkinput)

x <- 1:3
all_characters(x)
#> [1] FALSE

y <- letters[x]
all_characters(y)
#> [1] TRUE

all_characters(c(x, y)) # TRUE, even though 'x' is numeric!
#> [1] TRUE
all_characters(x) && all_characters(y)
#> [1] FALSE
all(unlist(lapply(X = list(x, y), FUN = all_characters)))
#> [1] FALSE

z <- list("a", 2, "c")
z
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] "c"
lapply(X = z, class)
#> [[1]]
#> [1] "character"
#> 
#> [[2]]
#> [1] "numeric"
#> 
#> [[3]]
#> [1] "character"
# TRUE, even though the second element of 'z' is numeric!
all_characters(unlist(z))
#> [1] TRUE
all(unlist(lapply(X = z, FUN = all_characters)))
#> [1] FALSE
```

## Zero-length values

Although zero-length objects (see
[`help("is_zerolength")`](https://jessealderliesten.github.io/checkinput/reference/is_zerolength.md))
are discarded when combined into a vector with other values, their types
are taken into account for type coercion. For example, numeric `314`
will be coerced to character `"314"` when combined into a vector with
zero-length `character(0)`, such that `c(314, character(0))` results in
the character string `"314"`, **not** in the numeric value `314`.

``` r

num <- 314
str(num)
#>  num 314
zerochar <- character(0)
str(zerochar)
#>  chr(0)
str(c(num, zerochar))
#>  chr "314"
```

## sessionInfo

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.5 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    #>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    #>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    #> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    #> 
    #> time zone: UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] checkinput_1.5.1
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
    #>  [5] xfun_0.62         cachem_1.1.0      knitr_1.52        htmltools_0.5.9  
    #>  [9] rmarkdown_2.32    lifecycle_1.0.5   cli_3.6.6         sass_0.4.10      
    #> [13] pkgdown_2.2.1     textshaping_1.0.5 jquerylib_0.1.4   systemfonts_1.3.2
    #> [17] compiler_4.6.1    tools_4.6.1       ragg_1.5.2        bslib_0.12.0     
    #> [21] evaluate_1.0.5    yaml_2.3.12       otel_0.2.0        jsonlite_2.0.0   
    #> [25] rlang_1.3.0       fs_2.1.0
