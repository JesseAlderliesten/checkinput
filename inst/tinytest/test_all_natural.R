#### Test the examples ####
expect_true(all_natural(x = c(3, 5 + 1e-10)))
expect_false(all_natural(x = c(1e-10, 3, 5)))
expect_true(all_natural(x = c(1e-10, 3, 5), strict = FALSE))
expect_false(all_natural(x = c(-1e-10, 3, 5), strict = FALSE))
expect_false(all_natural(x = c(3, 5, Inf)))
expect_false(all_natural(x = c(3, 5, Inf), strict = FALSE))
expect_false(all_natural(x = "a"))

x <- sqrt(2)^2
expect_true(all_natural(x = x)) # TRUE
expect_false(x == 2)


#### Tests ####
expect_false(all_natural(x = -3L, strict = FALSE))

expect_false(all_natural(x = 0, strict = TRUE))
expect_true(all_natural(x = 0, strict = FALSE))
expect_false(all_natural(x = 0L, strict = TRUE))
expect_true(all_natural(x = 0L, strict = FALSE))

expect_false(all_natural(x = Inf, strict = TRUE))
expect_false(all_natural(x = Inf, strict = FALSE))

# Testing a value that cannot be represented as integer by R (R uses 32-bit
# integers, see 'Details' in help(`integer`))
expect_true(all_natural(x = .Machine$integer.max + 10)) #

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

expect_error(all_natural(x = c(3, 5 + 1e-10), strict = NA),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), allow_NA = NA),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(allow_NA, x)
