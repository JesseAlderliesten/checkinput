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


#### Test the examples ####
expect_true(is_natural(x = 5 + 1e-10))
expect_false(is_natural(x = 1e-10))
expect_true(is_natural(x = 1e-10, strict = FALSE))
expect_false(is_natural(x = -1e-10, strict = FALSE))
expect_false(is_natural(x = Inf))
expect_false(is_natural(x = Inf, strict = FALSE))
expect_false(is_natural(x = "a"))
expect_false(is_natural(x = 1:2))

x <- sqrt(2)^2
expect_true(is_natural(x = x)) # TRUE
expect_false(x == 2)

expect_true(all_natural(x = c(3, 5 + 1e-10)))
expect_false(all_natural(x = c(1e-10, 3, 5)))
expect_true(all_natural(x = c(1e-10, 3, 5), strict = FALSE))
expect_false(all_natural(x = c(-1e-10, 3, 5), strict = FALSE))
expect_false(all_natural(x = c(3, 5, Inf)))
expect_false(all_natural(x = c(3, 5, Inf), strict = FALSE))
expect_false(all_natural(x = "a"))
expect_true(all_natural(x = 1:2))

expect_equal(toy_fun_erroneous(x = 5 - 1e-8), 1:4)
expect_equal(toy_fun_correct(x = 5 - 1e-8), 1:5)


#### Tests ####
expect_false(is_natural(x = -3L, strict = FALSE))
expect_false(is_natural(x = 0, strict = TRUE))
expect_true(is_natural(x = 0, strict = FALSE))
expect_false(is_natural(x = 0L, strict = TRUE))
expect_true(is_natural(x = 0L, strict = FALSE))
expect_false(is_natural(x = Inf, strict = TRUE))
expect_false(is_natural(x = Inf, strict = FALSE))
expect_false(is_natural(x = integer(0), allow_zero = FALSE, strict = FALSE))
expect_true(is_natural(x = integer(0), allow_zero = TRUE, strict = FALSE))
expect_false(is_natural(x = numeric(0), allow_zero = FALSE, strict = FALSE))
expect_true(is_natural(x = numeric(0), allow_zero = TRUE, strict = FALSE))
expect_false(is_natural(x = NULL, allow_zero = TRUE, strict = FALSE))

# Testing a value that cannot be represented as integer by R (R uses 32-bit
# integers, see 'Details' in help(`integer`)). Negative values are not natural
# anyway, so no need to test large negative numbers.
expect_false(is_natural(x = .Machine$integer.max + 1))

expect_false(is_natural(x = c(3, 5 + 1e-10, 5.0, 1e4, 1.2e4)))

# NA_integer_ and NA_real_ are allowed if 'allow_NA' is TRUE
expect_true(is_natural(x = NA_integer_, allow_NA = TRUE))
expect_true(is_natural(x = NA_real_, allow_NA = TRUE))
expect_false(is_natural(x = NA_integer_, allow_NA = FALSE))
expect_false(is_natural(x = NA_real_, allow_NA = FALSE))

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
expect_false(all_natural(x = integer(0), allow_zero = FALSE, strict = FALSE))
expect_true(all_natural(x = integer(0), allow_zero = TRUE, strict = FALSE))
expect_false(all_natural(x = numeric(0), allow_zero = FALSE, strict = FALSE))
expect_true(all_natural(x = numeric(0), allow_zero = TRUE, strict = FALSE))
expect_false(all_natural(x = NULL, allow_zero = TRUE, strict = FALSE))

# Testing a value that cannot be represented as integer by R (R uses 32-bit
# integers, see 'Details' in help(`integer`)). Negative values are not natural
# anyway, so no need to test large negative numbers.
expect_false(all_natural(x = .Machine$integer.max + 1))

expect_true(all_natural(x = c(3, 5 + 1e-10, 5.0, 1e4, 1.2e4)))

# NA_integer_ and NA_real_ are allowed if 'allow_NA' is TRUE
expect_true(all_natural(x = NA_integer_, allow_NA = TRUE))
expect_true(all_natural(x = NA_real_, allow_NA = TRUE))
expect_false(all_natural(x = NA_integer_, allow_NA = FALSE))
expect_false(all_natural(x = NA_real_, allow_NA = FALSE))

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
expect_error(is_natural(x = 3, tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(is_natural(x = 3, tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)

expect_error(all_natural(x = c(3, 5 + 1e-10), strict = NA),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), allow_NA = NA),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(allow_NA, toy_fun_correct, toy_fun_erroneous, x)
