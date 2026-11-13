#### Preparations ####
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

# 'larger_than_tol' and 'smaller_than_tol_large' should be large enough to not
# vanish when added to 'x_large_double_pos'. To make that possible, 'tol_large'
# is used instead of the default '.Machine$double.eps^0.5'.
tol <- .Machine$double.eps^0.5
larger_than_tol <- 100 * tol
tol_large <- .Machine$double.eps^0.25
smaller_than_tol_large <- 0.01 * tol_large

warn_coercion <- "NAs introduced by coercion to integer range"
warn_double <- paste0("The return of 'make_natural()' has type 'double'",
                      " because 'large_as_double'")

x_large_double_pos <- .Machine$integer.max + 1
x_large_double_neg <- -1L * .Machine$integer.max - 1


#### Test the examples ####
expect_true(is_natural(x = 5 + 1e-10))
expect_false(is_natural(x = 1e-10))
expect_true(is_natural(x = 1e-10, strict = FALSE))
expect_false(is_natural(x = -1e-10, strict = FALSE))
expect_identical(make_natural(x = 1e-10, strict = FALSE), 0L)
expect_false(is_natural(x = Inf))
expect_false(is_natural(x = Inf, strict = FALSE))
expect_false(is_natural(x = "a"))
expect_false(is_natural(x = 1:2))

x <- sqrt(2)^2
expect_true(is_natural(x = x)) # TRUE
expect_false(x == 2)

expect_true(all_natural(x = c(3, 5 + 1e-10)))
expect_error(make_natural(x = c(3, 5 + 1e-10)),
             pattern = "checkinput::is_natural(c(3, 5 + 1e-10))", fixed = TRUE)
expect_identical(make_natural(x = c(3, 5 + 1e-10), all = TRUE), c(3L, 5L))

expect_false(all_natural(x = c(1e-10, 3, 5)))
expect_true(all_natural(x = c(1e-10, 3, 5), strict = FALSE))
expect_false(all_natural(x = c(-1e-10, 3, 5), strict = FALSE))
expect_false(all_natural(x = c(3, 5, Inf)))
expect_false(all_natural(x = c(3, 5, Inf), strict = FALSE))
expect_false(all_natural(x = "a"))
expect_true(all_natural(x = 1:2))

expect_warning(
  expect_equal(
    make_natural(x = c(4, x_large_double_pos + smaller_than_tol_large), all = TRUE,
                 tol = tol_large, large_as_double = TRUE),
    c(4, x_large_double_pos)
  ),
  pattern = warn_double, strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    make_natural(x = c(4, x_large_double_pos + smaller_than_tol_large), all = TRUE,
                 tol = tol_large, large_as_double = FALSE),
    c(4L, NA_integer_)
  ), pattern = warn_coercion, strict = TRUE, fixed = TRUE
)

expect_identical(toy_fun_erroneous(x = 5 - 1e-8), 1:4)
expect_identical(toy_fun_correct(x = 5 - 1e-8), 1:5)
expect_identical(toy_fun_safe(x = 5 - 1e-8), 1:5)

expect_error(toy_fun_erroneous(x = 5.1),
             pattern = "is_natural(x) is not TRUE", fixed = TRUE)
expect_error(toy_fun_correct(x = 5.1),
             pattern = "is_natural(x) is not TRUE", fixed = TRUE)
expect_error(toy_fun_safe(x = 5.1, all = FALSE),
             pattern = "is_natural(x) is not TRUE", fixed = TRUE)
expect_error(toy_fun_safe(x = 5.1, all = TRUE),
             pattern = "all_natural(x) is not TRUE", fixed = TRUE)


#### Tests ####
expect_false(is_natural(x = -3L, strict = FALSE))
expect_false(is_natural(x = 0, strict = TRUE))
expect_true(is_natural(x = 0, strict = FALSE))
expect_false(is_natural(x = 0L, strict = TRUE))
expect_error(make_natural(x = 0L, strict = TRUE),
             pattern = "is_natural(0L) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 1e-10, strict = TRUE),
             pattern = "is_natural(1e-10) is not TRUE", fixed = TRUE)
expect_identical(make_natural(x = 0L, strict = FALSE), 0L)
expect_identical(make_natural(x = 1e-10, strict = FALSE), 0L)
expect_true(is_natural(x = 0L, strict = FALSE))
expect_false(is_natural(x = Inf, strict = TRUE))
expect_false(is_natural(x = Inf, strict = FALSE))
expect_false(is_natural(x = integer(0), allow_zerolength = FALSE, strict = FALSE))
expect_true(is_natural(x = integer(0), allow_zerolength = TRUE, strict = FALSE))
expect_false(is_natural(x = numeric(0), allow_zerolength = FALSE, strict = FALSE))
expect_true(is_natural(x = numeric(0), allow_zerolength = TRUE, strict = FALSE))
expect_false(is_natural(x = NULL, allow_zerolength = TRUE, strict = FALSE))

expect_false(is_natural(x = c(3, 5 + 1e-10, 5.0, 1e4, 1.2e4)))
expect_error(make_natural(x = c(3, 5 + 1e-10, 5.0, 1e4, 1.2e4), all = FALSE),
             pattern = "is_natural(c(3, 5 + 1e-10, 5, 10000, 12000)) is not TRUE",
             fixed = TRUE)

# NA_integer_ and NA_real_ are allowed if 'allow_NA' is TRUE
expect_true(is_natural(x = NA_integer_, allow_NA = TRUE))
expect_true(is_natural(x = NA_real_, allow_NA = TRUE))
expect_false(is_natural(x = NA_integer_, allow_NA = FALSE))
expect_false(is_natural(x = NA_real_, allow_NA = FALSE))

expect_silent(expect_identical(
  make_natural(x = NA_integer_, allow_NA = TRUE, all = TRUE), NA_integer_
))
expect_silent(expect_identical(
  make_natural(x = NA_integer_, allow_NA = TRUE, all = FALSE), NA_integer_
))
expect_silent(expect_identical(
  make_natural(x = NA_real_, allow_NA = TRUE, all = TRUE), NA_integer_
))
expect_silent(expect_identical(
  make_natural(x = NA_real_, allow_NA = TRUE, all = FALSE), NA_integer_
))
expect_error(make_natural(x = NA, allow_NA = TRUE, all = TRUE),
             pattern = "all_natural(NA) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = NA, allow_NA = TRUE, all = FALSE),
             pattern = "is_natural(NA) is not TRUE", fixed = TRUE)

expect_error(make_natural(x = NA_integer_, allow_NA = FALSE, all = TRUE),
             pattern = "all_natural(NA_integer_) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = NA_integer_, allow_NA = FALSE, all = FALSE),
             pattern = "is_natural(NA_integer_) is not TRUE", fixed = TRUE)

expect_error(make_natural(x = NA_real_, allow_NA = FALSE, all = TRUE),
             pattern = "all_natural(NA_real_) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = NA_real_, allow_NA = FALSE, all = FALSE),
             pattern = "is_natural(NA_real_) is not TRUE", fixed = TRUE)

expect_error(make_natural(x = NA, allow_NA = FALSE, all = TRUE),
             pattern = "all_natural(NA) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = NA, allow_NA = FALSE, all = FALSE),
             pattern = "is_natural(NA) is not TRUE", fixed = TRUE)

for(allow_NA in c(FALSE, TRUE)) {
  for(x in list(FALSE, TRUE, NA, NaN, NA_complex_, NA_character_)) {
    expect_silent(expect_false(is_natural(x = x, allow_NA = allow_NA)))
  }
}

for(x in list(data.frame(a = "nco"), as.matrix(data.frame(a = "nco")))) {
  expect_silent(expect_false(is_natural(x = x)))
}

expect_false(all_natural(x = -3L, strict = FALSE))
expect_false(all_natural(x = 0, strict = TRUE))
expect_true(all_natural(x = 0, strict = FALSE))
expect_false(all_natural(x = 0L, strict = TRUE))
expect_true(all_natural(x = 0L, strict = FALSE))
expect_false(all_natural(x = Inf, strict = TRUE))
expect_false(all_natural(x = Inf, strict = FALSE))
expect_false(all_natural(x = integer(0), allow_zerolength = FALSE, strict = FALSE))
expect_true(all_natural(x = integer(0), allow_zerolength = TRUE, strict = FALSE))
expect_false(all_natural(x = numeric(0), allow_zerolength = FALSE, strict = FALSE))
expect_true(all_natural(x = numeric(0), allow_zerolength = TRUE, strict = FALSE))
expect_false(all_natural(x = NULL, allow_zerolength = TRUE, strict = FALSE))

# Testing values that cannot be represented as integer by R
expect_warning(
  expect_identical(.Machine$integer.max + 1L, NA_integer_),
  pattern = "NAs produced by integer overflow", strict = TRUE, fixed = TRUE)
expect_warning(
  expect_identical(-1L * .Machine$integer.max - 1L, NA_integer_),
  pattern = "NAs produced by integer overflow", strict = TRUE, fixed = TRUE)

expect_silent(expect_false(is.na(x_large_double_pos)))
expect_true(is.finite(x_large_double_pos))
expect_equal(sign(x_large_double_pos), 1)
expect_silent(expect_true(all_natural(x = x_large_double_pos)))

expect_true(abs(x_large_double_pos + larger_than_tol - x_large_double_pos) > 0)
expect_silent(expect_false(
  all_natural(x = x_large_double_pos + larger_than_tol, tol = tol)
))
expect_silent(expect_false(
  all_natural(x = x_large_double_pos - larger_than_tol, tol = tol)
))

expect_true(abs(x_large_double_pos + smaller_than_tol_large - x_large_double_pos) > 0)
expect_silent(expect_true(
  all_natural(x = x_large_double_pos + smaller_than_tol_large, tol = tol_large)
))
expect_silent(expect_true(
  all_natural(x = x_large_double_pos - smaller_than_tol_large, tol = tol_large)
))

expect_silent(expect_false(is.na(x_large_double_neg)))
expect_true(is.finite(x_large_double_neg))
expect_equal(sign(x_large_double_neg), -1)
# Negative numbers are not considered natural
expect_silent(expect_false(all_natural(x = x_large_double_neg)))

expect_true(abs(x_large_double_neg + larger_than_tol - x_large_double_neg) > 0)
expect_silent(expect_false(
  all_natural(x = x_large_double_neg + larger_than_tol, tol = tol)
))
expect_silent(expect_false(
  all_natural(x = x_large_double_neg - larger_than_tol, tol = tol)
))

expect_true(abs(x_large_double_neg + smaller_than_tol_large - x_large_double_neg) > 0)
expect_silent(expect_false(
  all_natural(x = x_large_double_neg + smaller_than_tol_large, tol = tol_large)
))
expect_silent(expect_false(
  all_natural(x = x_large_double_neg - smaller_than_tol_large, tol = tol_large)
))

expect_warning(
  expect_equal(
    make_natural(x = x_large_double_pos, large_as_double = TRUE),
    x_large_double_pos),
  pattern = warn_double, strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    make_natural(x = x_large_double_pos, large_as_double = FALSE),
    NA_integer_),
  pattern = warn_coercion, strict = TRUE, fixed = TRUE)


for(large_as_double in c(TRUE, FALSE)) {
  expect_error(
    make_natural(x = x_large_double_pos + larger_than_tol, tol = tol,
                 large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_pos + larger_than_tol)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_pos - larger_than_tol, tol = tol,
                 large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_pos - larger_than_tol)",
    fixed = TRUE
  )
}

expect_warning(
  expect_equal(
    make_natural(x = x_large_double_pos + smaller_than_tol_large,
                 tol = tol_large, large_as_double = TRUE),
    x_large_double_pos),
  pattern = warn_double, strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    make_natural(x = x_large_double_pos + smaller_than_tol_large,
                 tol = tol_large, large_as_double = FALSE),
    NA_integer_),
  pattern = warn_coercion, strict = TRUE, fixed = TRUE)

expect_warning(
  expect_equal(
    make_natural(x = x_large_double_pos - smaller_than_tol_large,
                 tol = tol_large, large_as_double = TRUE),
    x_large_double_pos),
  pattern = warn_double, strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    make_natural(x = x_large_double_pos - smaller_than_tol_large,
                 tol = tol_large, large_as_double = FALSE),
    NA_integer_),
  pattern = warn_coercion, strict = TRUE, fixed = TRUE)

for(large_as_double in c(TRUE, FALSE)) {
  expect_error(
    make_natural(x = x_large_double_neg, tol = tol,
                 large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_neg)",
    fixed = TRUE
  )

  expect_error(
    make_natural(x = x_large_double_neg + larger_than_tol, tol = tol,
                 large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_neg + larger_than_tol)",
    fixed = TRUE
  )

  expect_error(
    make_natural(x = x_large_double_neg - larger_than_tol, tol = tol),
    pattern = "checkinput::is_natural(x_large_double_neg - larger_than_tol)",
    fixed = TRUE
  )
}

# Negative values are not considered natural numbers.
tol_large

for(large_as_double in c(TRUE, FALSE)) {
  expect_error(
    make_natural(x = x_large_double_neg + smaller_than_tol_large,
                 tol = tol_large, large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_neg + smaller_than_tol_large)",
    fixed = TRUE
  )
  expect_error(
    make_natural(x = x_large_double_neg - smaller_than_tol_large,
                 tol = tol_large, large_as_double = large_as_double),
    pattern = "checkinput::is_natural(x_large_double_neg - smaller_than_tol_large)",
    fixed = TRUE
  )
}

expect_true(all_natural(x = c(3, 5 + 1e-10, 5.0, 1e4, 1.2e4)))
expect_identical(make_natural(x = c(3, 5 + 1e-10, 5.0, 1e4, 1.2e4), all = TRUE),
                 c(3L, 5L, 5L, 10000L, 12000L))

# NA_integer_ and NA_real_ are allowed if 'allow_NA' is TRUE
expect_true(all_natural(x = NA_integer_, allow_NA = TRUE))
expect_true(all_natural(x = NA_real_, allow_NA = TRUE))
expect_false(all_natural(x = NA_integer_, allow_NA = FALSE))
expect_false(all_natural(x = NA_real_, allow_NA = FALSE))
expect_error(make_natural(x = NA_integer_, allow_NA = FALSE),
             pattern = "is_natural(NA_integer_) is not TRUE", fixed = TRUE)

for(allow_NA in c(FALSE, TRUE)) {
  for(x in list(FALSE, TRUE, NA, NaN, NA_complex_, NA_character_)) {
    expect_silent(expect_false(all_natural(x = x, allow_NA = allow_NA)))
  }
}

for(x in list(data.frame(a = "nco"), as.matrix(data.frame(a = "nco")))) {
  expect_silent(expect_false(all_natural(x = x)))
}

expect_error(is_natural(x = 3, strict = NA),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)
expect_error(is_natural(x = 3, allow_NA = NA),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, all = NA),
             pattern = "is_logical(all) is not TRUE", fixed = TRUE)
expect_error(is_natural(x = 3, tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(is_natural(x = 3, tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(make_natural(x = 3, all = 0),
             pattern = "is_logical(all) is not TRUE", fixed = TRUE)

expect_error(all_natural(x = c(3, 5 + 1e-10), strict = NA),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), allow_NA = NA),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(allow_NA, large_as_double, larger_than_tol, smaller_than_tol_large, tol,
   tol_large, toy_fun_correct, toy_fun_erroneous, toy_fun_safe, warn_coercion,
   warn_double, x, x_large_double_neg, x_large_double_pos)
