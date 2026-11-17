#### Preparations ####
expect_warning(
  expect_identical(-1L * .Machine$integer.max - 1L, NA_integer_),
  pattern = "NAs produced by integer overflow", strict = TRUE, fixed = TRUE)
expect_warning(
  expect_identical(.Machine$integer.max + 1L, NA_integer_),
  pattern = "NAs produced by integer overflow", strict = TRUE, fixed = TRUE)

# 'larger_than_tol' and 'smaller_than_tol_large' should be large enough to not
# vanish when added to 'x_large_double_pos'. To make that possible, 'tol_large'
# is used instead of the default '.Machine$double.eps^0.5'.
tol <- .Machine$double.eps^0.5
larger_than_tol <- 100 * tol
tol_large <- .Machine$double.eps^0.25
smaller_than_tol_large <- 0.01 * tol_large

x_large_double_pos <- .Machine$integer.max + 1
expect_false(is.na(x_large_double_pos))
expect_true(is.finite(x_large_double_pos))
expect_identical(sign(x_large_double_pos), 1)
expect_true(abs(x_large_double_pos + larger_than_tol - x_large_double_pos) > 0)
expect_true(
  abs(x_large_double_pos + smaller_than_tol_large - x_large_double_pos) > 0)

x_large_double_neg <- -1L * .Machine$integer.max - 1
expect_false(is.na(x_large_double_neg))
expect_true(is.finite(x_large_double_neg))
expect_identical(sign(x_large_double_neg), -1)
expect_true(abs(x_large_double_neg + larger_than_tol - x_large_double_neg) > 0)
expect_true(
  abs(x_large_double_neg + smaller_than_tol_large - x_large_double_neg) > 0)

warn_coercion <- "NAs introduced by coercion to integer range"
warn_double <- paste0("The return of 'make_natural()' has type 'double'",
                      " because 'large_as_double'")

#### Test the examples ####
x <- sqrt(3)^2
expect_false(x == 3)
expect_equal(x - 3, -4.44e-16)
expect_true(is_natural(x))
expect_identical(seq_len(x), 1:2)
expect_identical(seq_len(make_natural(x)), 1:3)

expect_true(is_natural(5 + 1e-9, tol = 1e-8))
expect_false(is_natural(5 + 1e-7, tol = 1e-8))
expect_false(is_natural(1:2))
expect_true(all_natural(1:2))
expect_false(is_natural("a"))

expect_false(is_natural(1e-9, strict = TRUE))
expect_true(is_natural(1e-9, strict = FALSE))
expect_false(is_natural(-1e-9, strict = FALSE))

expect_false(is_natural(integer(0), allow_zerolength = FALSE))
expect_true(is_natural(integer(0), allow_zerolength = TRUE))
expect_false(is_natural(numeric(0), allow_zerolength = FALSE))
expect_true(is_natural(numeric(0), allow_zerolength = TRUE))
expect_false(is_natural(logical(0), allow_zerolength = TRUE))
expect_false(is_natural(NULL, allow_zerolength = TRUE))

expect_identical(make_natural(5 + 1e-9), 5L)
expect_error(make_natural(x = 5 + 1e-7),
             pattern = "checkinput::is_natural(5 + 1e-07)", fixed = TRUE)
expect_error(make_natural(x = c(3, 5 + 1e-9), all = FALSE),
             pattern = "checkinput::is_natural(c(3, 5 + 1e-09))", fixed = TRUE)
expect_identical(make_natural(x = c(3, 5 + 1e-9), all = TRUE), c(3L, 5L))

expect_warning(
  expect_equal(
    make_natural(x = c(4, x_large_double_pos), all = TRUE,
                 large_as_double = TRUE),
    c(4, x_large_double_pos)
  ),
  pattern = warn_double, strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    make_natural(x = c(4, x_large_double_pos), all = TRUE,
                 large_as_double = FALSE),
    c(4L, NA_integer_)
  ), pattern = warn_coercion, strict = TRUE, fixed = TRUE
)

expect_false(is_natural(Inf, allow_Inf = FALSE))
expect_error(make_natural(x = Inf, allow_Inf = FALSE),
             pattern = "checkinput::is_natural(Inf) is not TRUE", fixed = TRUE)
expect_true(is_natural(Inf, allow_Inf = TRUE))
expect_warning(
  expect_identical(
    make_natural(x = Inf, allow_Inf = TRUE, large_as_double = TRUE), Inf
  ),
  pattern = warn_double, fixed = TRUE, strict = TRUE)
expect_warning(
  expect_identical(
    make_natural(x = Inf, allow_Inf = TRUE, large_as_double = FALSE), NA_integer_
  ),
  pattern = warn_coercion, fixed = TRUE, strict = TRUE)

#### Tests ####

##### vector input #####
expect_false(is_natural(c(3, 5 + 1e-9, 5.0, 1.2e4)))
expect_true(all_natural(c(3, 5 + 1e-9, 5.0, 1.2e4)))
expect_error(make_natural(x = c(3, 5 + 1e-9, 5.0, 1.2e4), all = FALSE),
             pattern = "is_natural(c(3, 5 + 1e-09, 5, 12000)) is not TRUE",
             fixed = TRUE)
expect_identical(make_natural(x = c(3, 5 + 1e-9, 5.0, 1.2e4), all = TRUE),
                 c(3L, 5L, 5L, 12000L))

##### negative numbers #####
expect_false(is_natural(-3L))
expect_false(all_natural(-3L))
expect_error(
  make_natural(x = -3L, all = FALSE),
  pattern = "is_natural(-3L) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = -3L, all = TRUE),
  pattern = "all_natural(-3L) is not TRUE", fixed = TRUE)

##### (almost) zero #####
expect_false(is_natural(0L, strict = TRUE))
expect_false(all_natural(0L, strict = TRUE))
expect_error(
  make_natural(x = 0L, strict = TRUE, all = FALSE),
  pattern = "is_natural(0L) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = 0L, strict = TRUE, all = TRUE),
  pattern = "all_natural(0L) is not TRUE", fixed = TRUE)

expect_true(is_natural(0L, strict = FALSE))
expect_true(all_natural(0L, strict = FALSE))
expect_identical(make_natural(x = 0L, strict = FALSE, all = FALSE), 0L)
expect_identical(make_natural(x = 0L, strict = FALSE, all = TRUE), 0L)

expect_false(is_natural(0, strict = TRUE))
expect_false(all_natural(0, strict = TRUE))
expect_error(
  make_natural(x = 0, strict = TRUE, all = FALSE),
  pattern = "is_natural(0) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = 0, strict = TRUE, all = TRUE),
  pattern = "all_natural(0) is not TRUE", fixed = TRUE)

expect_true(is_natural(0, strict = FALSE))
expect_true(all_natural(0, strict = FALSE))
expect_identical(make_natural(x = 0, strict = FALSE, all = FALSE), 0L)
expect_identical(make_natural(x = 0, strict = FALSE, all = TRUE), 0L)

expect_false(is_natural(1e-9, strict = TRUE))
expect_false(all_natural(1e-9, strict = TRUE))
expect_error(
  make_natural(x = 1e-9, strict = TRUE, all = FALSE),
  pattern = "is_natural(1e-09) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = 1e-9, strict = TRUE, all = TRUE),
  pattern = "all_natural(1e-09) is not TRUE", fixed = TRUE)

expect_true(is_natural(1e-9, strict = FALSE))
expect_true(all_natural(1e-9, strict = FALSE))
expect_identical(make_natural(x = 1e-9, strict = FALSE, all = FALSE), 0L)
expect_identical(make_natural(x = 1e-9, strict = FALSE, all = TRUE), 0L)

expect_false(is_natural(-1e-9, strict = TRUE))
expect_false(all_natural(-1e-9, strict = TRUE))
expect_error(
  make_natural(x = -1e-9, strict = TRUE, all = FALSE),
  pattern = "is_natural(-1e-09) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = -1e-9, strict = TRUE, all = TRUE),
  pattern = "all_natural(-1e-09) is not TRUE", fixed = TRUE)

expect_false(is_natural(-1e-9, strict = FALSE))
expect_false(all_natural(-1e-9, strict = FALSE))
expect_error(
  make_natural(x = -1e-9, strict = FALSE, all = FALSE),
  pattern = "is_natural(-1e-09) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = -1e-9, strict = FALSE, all = TRUE),
  pattern = "all_natural(-1e-09) is not TRUE", fixed = TRUE)

##### zero-length #####
expect_false(is_natural(integer(0), allow_zerolength = FALSE))
expect_false(all_natural(integer(0), allow_zerolength = FALSE))
expect_true(is_natural(integer(0), allow_zerolength = TRUE))
expect_true(all_natural(integer(0), allow_zerolength = TRUE))
expect_false(is_natural(numeric(0), allow_zerolength = FALSE))
expect_false(all_natural(numeric(0), allow_zerolength = FALSE))
expect_true(is_natural(numeric(0), allow_zerolength = TRUE))
expect_true(all_natural(numeric(0), allow_zerolength = TRUE))
expect_false(is_natural(logical(0), allow_zerolength = FALSE))
expect_false(all_natural(logical(0), allow_zerolength = FALSE))
expect_false(is_natural(logical(0), allow_zerolength = TRUE))
expect_false(all_natural(logical(0), allow_zerolength = TRUE))
expect_false(is_natural(NULL, allow_zerolength = FALSE))
expect_false(all_natural(NULL, allow_zerolength = FALSE))
expect_false(is_natural(NULL, allow_zerolength = TRUE))
expect_false(all_natural(NULL, allow_zerolength = TRUE))

expect_error(
  make_natural(x = integer(0), allow_zerolength = FALSE, all = FALSE),
  pattern = "is_natural(integer(0)) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = integer(0), allow_zerolength = FALSE, all = TRUE),
  pattern = "all_natural(integer(0)) is not TRUE", fixed = TRUE)
expect_identical(make_natural(x = integer(0), allow_zerolength = TRUE, all = FALSE), integer(0))
expect_identical(make_natural(x = integer(0), allow_zerolength = TRUE, all = TRUE), integer(0))
expect_error(
  make_natural(x = numeric(0), allow_zerolength = FALSE, all = FALSE),
  pattern = "is_natural(numeric(0)) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = numeric(0), allow_zerolength = FALSE, all = TRUE),
  pattern = "all_natural(numeric(0)) is not TRUE", fixed = TRUE)
expect_identical(
  make_natural(x = numeric(0), allow_zerolength = TRUE, all = FALSE),
  integer(0)
)
expect_identical(
  make_natural(x = numeric(0), allow_zerolength = TRUE, all = TRUE),
  integer(0)
)
expect_error(
  make_natural(x = NULL, allow_zerolength = FALSE, all = FALSE),
  pattern = "is_natural(NULL) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = NULL, allow_zerolength = FALSE, all = TRUE),
  pattern = "all_natural(NULL) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = NULL, allow_zerolength = TRUE, all = FALSE),
  pattern = "is_natural(NULL) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = NULL, allow_zerolength = TRUE, all = TRUE),
  pattern = "all_natural(NULL) is not TRUE", fixed = TRUE)

##### NAs and NaN #####
# Only NA_integer_ and NA_real_ are allowed if 'allow_NA' is TRUE
expect_true(is_natural(NA_integer_, allow_NA = TRUE, allow_NaN = FALSE))
expect_true(all_natural(NA_integer_, allow_NA = TRUE, allow_NaN = FALSE))
expect_silent(expect_identical(
  make_natural(x = NA_integer_, allow_NA = TRUE, allow_NaN = FALSE, all = FALSE),
  NA_integer_
))
expect_silent(expect_identical(
  make_natural(x = NA_integer_, allow_NA = TRUE, allow_NaN = FALSE, all = TRUE),
  NA_integer_
))

expect_false(is_natural(NA_integer_, allow_NA = FALSE, allow_NaN = TRUE))
expect_false(all_natural(NA_integer_, allow_NA = FALSE, allow_NaN = TRUE))
expect_error(
  make_natural(x = NA_integer_, allow_NA = FALSE, allow_NaN = TRUE, all = FALSE),
  pattern = "is_natural(NA_integer_) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = NA_integer_, allow_NA = FALSE, allow_NaN = TRUE, all = TRUE),
  pattern = "all_natural(NA_integer_) is not TRUE", fixed = TRUE)

expect_true(is_natural(NA_real_, allow_NA = TRUE, allow_NaN = FALSE))
expect_true(all_natural(NA_real_, allow_NA = TRUE, allow_NaN = FALSE))
expect_silent(
  expect_identical(
    make_natural(x = NA_real_, allow_NA = TRUE, allow_NaN = FALSE, all = FALSE),
    NA_integer_
  )
)
expect_silent(
  expect_identical(
    make_natural(x = NA_real_, allow_NA = TRUE, allow_NaN = FALSE, all = TRUE),
    NA_integer_
  )
)

expect_false(is_natural(NA_real_, allow_NA = FALSE, allow_NaN = TRUE))
expect_false(all_natural(NA_real_, allow_NA = FALSE, allow_NaN = TRUE))
expect_error(
  make_natural(x = NA_real_, allow_NA = FALSE, allow_NaN = TRUE, all = FALSE),
  pattern = "is_natural(NA_real_) is not TRUE", fixed = TRUE)
expect_error(
  make_natural(x = NA_real_, allow_NA = FALSE, allow_NaN = TRUE, all = TRUE),
  pattern = "all_natural(NA_real_) is not TRUE", fixed = TRUE)

expect_true(is_natural(NaN, allow_NA = FALSE, allow_NaN = TRUE))
expect_true(all_natural(NaN, allow_NA = FALSE, allow_NaN = TRUE))
expect_identical(
  make_natural(NaN, allow_NA = FALSE, allow_NaN = TRUE), NA_integer_)

expect_false(is_natural(NaN, allow_NA = TRUE, allow_NaN = FALSE))
expect_false(all_natural(NaN, allow_NA = TRUE, allow_NaN = FALSE))
expect_error(
  make_natural(NaN, allow_NA = TRUE, allow_NaN = FALSE),
  pattern = "checkinput::is_natural(NaN) is not TRUE", fixed = TRUE)

for(allow_NA in c(FALSE, TRUE)) {
  for(x in list(NA, NaN, NA_complex_, NA_character_)) {
    expect_false(is_natural(x, allow_NA = allow_NA, allow_NaN = FALSE))
    expect_false(all_natural(x, allow_NA = allow_NA, allow_NaN = FALSE))
    expect_error(
      make_natural(x = x, allow_NA = allow_NA, allow_NaN = FALSE, all = FALSE),
      pattern = "is_natural(x) is not TRUE", fixed = TRUE)
    expect_error(
      make_natural(x = x, allow_NA = allow_NA, allow_NaN = FALSE, all = TRUE),
      pattern = "all_natural(x) is not TRUE", fixed = TRUE)
  }
}

##### large values and Inf #####

# Testing values that cannot be represented as integer by R
expect_true(is_natural(x_large_double_pos))
expect_true(all_natural(x_large_double_pos))
for(arg_all in c(TRUE, FALSE)) {
  expect_warning(
    expect_equal(
      make_natural(x = x_large_double_pos, all = arg_all, large_as_double = TRUE),
      x_large_double_pos),
    pattern = warn_double, strict = TRUE, fixed = TRUE)

  expect_warning(
    expect_identical(
      make_natural(x = x_large_double_pos, all = arg_all, large_as_double = FALSE),
      NA_integer_),
    pattern = warn_coercion, strict = TRUE, fixed = TRUE)
}

expect_true(
  is_natural(x_large_double_pos - smaller_than_tol_large, tol = tol_large))
expect_true(
  all_natural(x_large_double_pos - smaller_than_tol_large, tol = tol_large))
for(arg_all in c(TRUE, FALSE)) {
  expect_warning(
    expect_equal(
      make_natural(x = x_large_double_pos - smaller_than_tol_large,
                   all = arg_all, tol = tol_large, large_as_double = TRUE),
      x_large_double_pos),
    pattern = warn_double, strict = TRUE, fixed = TRUE)

  expect_warning(
    expect_identical(
      make_natural(x = x_large_double_pos - smaller_than_tol_large,
                   all = arg_all, tol = tol_large, large_as_double = FALSE),
      NA_integer_),
    pattern = warn_coercion, strict = TRUE, fixed = TRUE)
}

expect_true(
  is_natural(x_large_double_pos + smaller_than_tol_large, tol = tol_large))
expect_true(
  all_natural(x_large_double_pos + smaller_than_tol_large, tol = tol_large))
for(arg_all in c(TRUE, FALSE)) {
  expect_warning(
    expect_equal(
      make_natural(x = x_large_double_pos + smaller_than_tol_large,
                   tol = tol_large, large_as_double = TRUE, all = arg_all),
      x_large_double_pos),
    pattern = warn_double, strict = TRUE, fixed = TRUE)

  expect_warning(
    expect_identical(
      make_natural(x = x_large_double_pos + smaller_than_tol_large,
                   tol = tol_large, large_as_double = FALSE, all = arg_all),
      NA_integer_),
    pattern = warn_coercion, strict = TRUE, fixed = TRUE)
}

expect_false(is_natural(x_large_double_pos - larger_than_tol, tol = tol))
expect_false(all_natural(x_large_double_pos - larger_than_tol, tol = tol))
for(large_as_double in c(TRUE, FALSE)) {
  expect_error(
    make_natural(x = x_large_double_pos - larger_than_tol, tol = tol,
                 large_as_double = large_as_double, all = FALSE),
    pattern = "checkinput::is_natural(x_large_double_pos - larger_than_tol)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_pos - larger_than_tol, tol = tol,
                 large_as_double = large_as_double, all = TRUE),
    pattern = "checkinput::all_natural(x_large_double_pos - larger_than_tol)",
    fixed = TRUE
  )
}

expect_false(is_natural(x_large_double_pos + larger_than_tol, tol = tol))
expect_false(all_natural(x_large_double_pos + larger_than_tol, tol = tol))
for(large_as_double in c(TRUE, FALSE)) {
  expect_error(
    make_natural(x = x_large_double_pos + larger_than_tol, tol = tol,
                 large_as_double = large_as_double, all = FALSE),
    pattern = "checkinput::is_natural(x_large_double_pos + larger_than_tol)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_pos + larger_than_tol, tol = tol,
                 large_as_double = large_as_double, all = TRUE),
    pattern = "checkinput::all_natural(x_large_double_pos + larger_than_tol)",
    fixed = TRUE
  )
}

# Negative numbers are not considered natural
expect_false(is_natural(x_large_double_neg))
expect_false(all_natural(x_large_double_neg))

expect_false(
  is_natural(x_large_double_neg - smaller_than_tol_large, tol = tol_large))
expect_false(
  all_natural(x_large_double_neg - smaller_than_tol_large, tol = tol_large))
expect_false(
  is_natural(x_large_double_neg + smaller_than_tol_large, tol = tol_large))
expect_false(
  all_natural(x_large_double_neg + smaller_than_tol_large, tol = tol_large))

expect_false(is_natural(x_large_double_neg - larger_than_tol, tol = tol))
expect_false(all_natural(x_large_double_neg - larger_than_tol, tol = tol))
expect_false(is_natural(x_large_double_neg + larger_than_tol, tol = tol))
expect_false(all_natural(x_large_double_neg + larger_than_tol, tol = tol))

for(large_as_double in c(TRUE, FALSE)) {
  expect_error(
    make_natural(x = x_large_double_neg, all = FALSE, tol = tol,
                 large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_neg)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_neg, all = TRUE, tol = tol,
                 large_as_double = large_as_double),
    pattern = "checkinput::all_natural(x_large_double_neg)",
    fixed = TRUE
  )

  expect_error(
    make_natural(x = x_large_double_neg - smaller_than_tol_large, all = FALSE,
                 tol = tol_large, large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_neg - smaller_than_tol_large)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_neg - smaller_than_tol_large, all = TRUE,
                 tol = tol_large, large_as_double = large_as_double),
    pattern = "checkinput::all_natural(x_large_double_neg - smaller_than_tol_large)",
    fixed = TRUE
  )

  expect_error(
    make_natural(x = x_large_double_neg + smaller_than_tol_large, all = FALSE,
                 tol = tol_large, large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_neg + smaller_than_tol_large)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_neg + smaller_than_tol_large, all = TRUE,
                 tol = tol_large, large_as_double = large_as_double),
    pattern = "checkinput::all_natural(x_large_double_neg + smaller_than_tol_large)",
    fixed = TRUE
  )

  expect_error(
    make_natural(x = x_large_double_neg - larger_than_tol, all = FALSE, tol = tol),
    pattern = "checkinput::is_natural(x_large_double_neg - larger_than_tol)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_neg - larger_than_tol, all = TRUE, tol = tol),
    pattern = "checkinput::all_natural(x_large_double_neg - larger_than_tol)",
    fixed = TRUE
  )

  expect_error(
    make_natural(x = x_large_double_neg + larger_than_tol, all = FALSE, tol = tol,
                 large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_neg + larger_than_tol)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_neg + larger_than_tol, all = TRUE, tol = tol,
                 large_as_double = large_as_double),
    pattern = "checkinput::all_natural(x_large_double_neg + larger_than_tol)",
    fixed = TRUE
  )
}

expect_false(is_natural(Inf, allow_Inf = FALSE))
expect_false(all_natural(Inf, allow_Inf = FALSE))
expect_error(
  make_natural(x = Inf, allow_Inf = FALSE, all = FALSE, tol = tol,
               large_as_double = TRUE),
  pattern = "checkinput::is_natural(Inf)", fixed = TRUE
)
expect_error(
  make_natural(x = Inf, allow_Inf = FALSE, all = TRUE, tol = tol,
               large_as_double = TRUE),
  pattern = "checkinput::all_natural(Inf)", fixed = TRUE
)
expect_error(
  make_natural(x = Inf, allow_Inf = FALSE, all = FALSE, tol = tol,
               large_as_double = FALSE),
  pattern = "checkinput::is_natural(Inf)", fixed = TRUE
)
expect_error(
  make_natural(x = Inf, allow_Inf = FALSE, all = TRUE, tol = tol,
               large_as_double = FALSE),
  pattern = "checkinput::all_natural(Inf)", fixed = TRUE
)


expect_true(is_natural(Inf, allow_Inf = TRUE))
expect_true(all_natural(Inf, allow_Inf = TRUE))

expect_warning(
  expect_identical(
    make_natural(
      x = Inf, allow_Inf = TRUE, all = FALSE, tol = tol, large_as_double = TRUE),
    Inf),
  pattern = warn_double, fixed = TRUE
)
expect_warning(
  expect_identical(
    make_natural(
      x = Inf, allow_Inf = TRUE, all = TRUE, tol = tol, large_as_double = TRUE),
    Inf),
  pattern = warn_double, fixed = TRUE
)
expect_warning(
  expect_identical(
    make_natural(
      x = Inf, allow_Inf = TRUE, all = FALSE, tol = tol, large_as_double = FALSE),
    NA_integer_),
  pattern = warn_coercion, fixed = TRUE
)
expect_warning(
  expect_identical(
    make_natural(
      x = Inf, allow_Inf = TRUE, all = TRUE, tol = tol, large_as_double = FALSE),
    NA_integer_),
  pattern = warn_coercion, fixed = TRUE
)

##### Miscellaneous #####
for(x in list(FALSE, TRUE, data.frame(a = "nco"),
              as.matrix(data.frame(a = "nco")))) {
  expect_false(is_natural(x))
  expect_false(all_natural(x))
  expect_error(
    make_natural(x = x, all = FALSE),
    pattern = "checkinput::is_natural(x)", fixed = TRUE
  )
  expect_error(
    make_natural(x = x, all = TRUE),
    pattern = "checkinput::all_natural(x)", fixed = TRUE
  )
}

##### Erroneous arguments #####
expect_error(is_natural(3, strict = NA),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)
expect_error(all_natural(c(3, 5), strict = NA),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, strict = NA, all = FALSE),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = c(3, 5), strict = NA, all = TRUE),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)

expect_error(is_natural(3, allow_zerolength = NA),
             pattern = "is_logical(allow_zerolength) is not TRUE", fixed = TRUE)
expect_error(all_natural(c(3, 5), allow_zerolength = NA),
             pattern = "is_logical(allow_zerolength) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, allow_zerolength = NA, all = FALSE),
             pattern = "is_logical(allow_zerolength) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = c(3, 5), allow_zerolength = NA, all = TRUE),
             pattern = "is_logical(allow_zerolength) is not TRUE", fixed = TRUE)

expect_error(is_natural(3, allow_NA = NA),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
expect_error(all_natural(c(3, 5), allow_NA = NA),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, allow_NA = NA, all = FALSE),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = c(3, 5), allow_NA = NA, all = TRUE),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)

expect_error(is_natural(3, allow_Inf = NA),
             pattern = "is_logical(allow_Inf) is not TRUE", fixed = TRUE)
expect_error(all_natural(c(3, 5), allow_Inf = NA),
             pattern = "is_logical(allow_Inf) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, allow_Inf = NA, all = FALSE),
             pattern = "is_logical(allow_Inf) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = c(3, 5), allow_Inf = NA, all = TRUE),
             pattern = "is_logical(allow_Inf) is not TRUE", fixed = TRUE)

expect_error(is_natural(3, tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(all_natural(c(3, 5), tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, all = FALSE, tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = c(3, 5), all = TRUE, tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)

expect_error(is_natural(3, tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(all_natural(c(3, 5), tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, all = FALSE, tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = c(3, 5), all = TRUE, tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)

expect_error(make_natural(x = 3, large_as_double = 0, all = FALSE),
             pattern = "is_logical(large_as_double) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, large_as_double = 0, all = TRUE),
             pattern = "is_logical(large_as_double) is not TRUE", fixed = TRUE)

expect_error(make_natural(x = 3, large_as_double = NA, all = FALSE),
             pattern = "is_logical(large_as_double) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, large_as_double = NA, all = TRUE),
             pattern = "is_logical(large_as_double) is not TRUE", fixed = TRUE)

expect_error(make_natural(x = 3, all = 0),
             pattern = "is_logical(all) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, all = NA),
             pattern = "is_logical(all) is not TRUE", fixed = TRUE)

#### Remove objects used in tests ####
rm(allow_NA, arg_all, large_as_double, larger_than_tol, smaller_than_tol_large,
   tol, tol_large, warn_coercion, warn_double, x, x_large_double_neg,
   x_large_double_pos)
