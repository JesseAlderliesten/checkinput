#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)


#### Test the examples ####
expect_true(is_logical(x = TRUE))
expect_true(is_logical(x = NA, allow_NA = TRUE))

for(x in list(c(TRUE, TRUE), 1, NA)) {
  expect_false(is_logical(x = x))
}

expect_false(is_logical(x = NA_character_, allow_NA = TRUE))


#### Tests ####
for(x in false_true) {
  for(allow_zero in false_true) {
    for(allow_NA in false_true) {
      expect_true(is_logical(x = x, allow_zero = allow_zero, allow_NA = allow_NA))
    }
  }
}

for(allow_zero in false_true) {
  for(allow_NA in false_true) {
    for(x in list(NULL, c(TRUE, TRUE), c(TRUE, NA), numeric(0), NaN, NA_real_,
                  -Inf, -314, 0, 314, Inf, character(0), NA_character_, "",
                  "nco", c(NA, NA), data.frame(a = TRUE), matrix(TRUE),
                  list(TRUE))) {
      expect_false(is_logical(x = x, allow_zero = allow_zero, allow_NA = allow_NA))
    }
  }
}

# x for which return of is_logical() is equal to argument 'allow_zero'
for(allow_NA in false_true) {
  expect_true(is_logical(x = logical(0), allow_zero = TRUE, allow_NA = allow_NA))
  expect_false(is_logical(x = logical(0), allow_zero = FALSE, allow_NA = allow_NA))
}

# x for which return of is_logical() is equal to argument 'allow_NA'
for(allow_zero in false_true) {
  expect_true(is_logical(x = NA, allow_zero = allow_zero, allow_NA = TRUE))
  expect_false(is_logical(x = NA, allow_zero = allow_zero, allow_NA = FALSE))
}

# Arguments that should result in an error.
for(allow_NA in false_true) {
  expect_error(is_logical(x = TRUE, allow_zero = NA, allow_NA = allow_NA),
               pattern = "!is.na.allow_zero) is not TRUE")
}

for(allow_zero in false_true) {
  expect_error(is_logical(x = TRUE, allow_zero = allow_zero, allow_NA = NA),
               pattern = "!is.na.allow_NA) is not TRUE")
}


#### Remove objects used in tests ####
rm(allow_zero, allow_NA, false_true)
