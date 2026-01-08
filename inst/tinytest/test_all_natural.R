#### Test the examples ####
expect_true(all_natural(x = c(3, 5 + 1e-10)))
expect_false(all_natural(x = c(1e-10, 3, 5)))
expect_true(all_natural(x = c(1e-10, 3, 5), strict = FALSE))
expect_false(all_natural(x = c(-1e-10, 3, 5), strict = FALSE))
expect_false(all_natural(x = c(3, 5, Inf)))
expect_false(all_natural(x = c(3, 5, Inf), strict = FALSE))

x <- sqrt(2)^2
expect_true(all_natural(x = x)) # TRUE
expect_false(x == 2)


#### Tests ####
expect_false(all_natural(x = 0, strict = TRUE))
expect_true(all_natural(x = 0, strict = FALSE))
expect_false(all_natural(x = 0L, strict = TRUE))
expect_true(all_natural(x = 0L, strict = FALSE))

expect_false(all_natural(x = Inf, strict = TRUE))
expect_false(all_natural(x = Inf, strict = FALSE))

expect_false(all_natural(x = NA_integer_, allow_NA = FALSE))
expect_true(all_natural(x = NA_integer_, allow_NA = TRUE))

expect_false(all_natural(x = NA_real_, allow_NA = FALSE))
expect_true(all_natural(x = NA_real_, allow_NA = TRUE))

expect_true(all_natural(x = 1e20))

for(allow_NA in c(FALSE, TRUE)) {
  expect_false(all_natural(x = NA_complex_, allow_NA = allow_NA))
  expect_false(all_natural(x = NA_character_, allow_NA = allow_NA))
  expect_false(all_natural(x = NA, allow_NA = allow_NA))
  expect_false(all_natural(x = NaN, allow_NA = allow_NA))
}

expect_error(all_natural(x = c(3, 5 + 1e-10), strict = NA),
             pattern = "is_logical(strict) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), allow_NA = NA),
             pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), tol = -3),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
expect_error(all_natural(x = c(3, 5 + 1e-10), tol = 0),
             pattern = "is_positive(tol) is not TRUE", fixed = TRUE)
