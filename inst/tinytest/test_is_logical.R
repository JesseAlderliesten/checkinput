# Examples
expect_true(is_logical(x = TRUE))
expect_true(is_logical(x = NA, allow_NA = TRUE))

for(x in list(c(TRUE, TRUE), 1, NA)) {
  expect_false(is_logical(x = x))
}

expect_false(is_logical(x = NA_character_, allow_NA = TRUE))

# Tests
logicals <- list(FALSE, TRUE, NA)

for(x in list(TRUE, FALSE)) {
  for(allow_NA in logicals) {
    for(allow_zero in logicals) {
      expect_true(is_logical(x = x, allow_NA = allow_NA, allow_zero = allow_zero))
    }
  }
}

for(allow_NA in logicals) {
  for(allow_zero in logicals) {
    for(x in list(NULL, c(TRUE, TRUE), c(TRUE, NA), numeric(0), NaN, NA_real_,
                  -Inf, -314, 0, 314, Inf, character(0), NA_character_, "",
                  "nco", c(NA, NA), data.frame(a = TRUE), matrix(TRUE),
                  list(TRUE))) {
      expect_false(is_logical(x = x, allow_NA = allow_NA, allow_zero = allow_zero))
    }
  }
}

# x for which return of is_logical() is equal to argument 'allow_NA'
for(allow_zero in logicals) {
  expect_true(is_logical(x = NA, allow_zero = allow_zero, allow_NA = TRUE))
  expect_false(is_logical(x = NA, allow_zero = allow_zero, allow_NA = FALSE))
  expect_true(is.na(is_logical(x = NA, allow_zero = allow_zero, allow_NA = NA)))
}

# x for which return of is_logical() is equal to argument 'allow_zero'
for(allow_NA in logicals) {
  expect_true(is_logical(x = logical(0), allow_zero = TRUE, allow_NA = allow_NA))
  expect_false(is_logical(x = logical(0), allow_zero = FALSE, allow_NA = allow_NA))
  expect_true(is.na(is_logical(x = logical(0), allow_zero = NA, allow_NA = allow_NA)))
}
