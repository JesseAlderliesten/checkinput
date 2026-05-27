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
  for(allow_zero_length in false_true) {
    for(allow_NA in false_true) {
      expect_silent(expect_true(
        is_logical(x = x, allow_zero_length = allow_zero_length, allow_NA = allow_NA)))
    }
  }
}

for(allow_zero_length in false_true) {
  for(allow_NA in false_true) {
    for(x in list(NULL, c(TRUE, TRUE), c(TRUE, NA), numeric(0), NaN, NA_real_,
                  -Inf, -314, 0, 314, Inf, character(0), NA_character_, "",
                  "nco", c(NA, NA), data.frame(a = TRUE), matrix(TRUE),
                  list(TRUE))) {
      expect_silent(expect_false(
        is_logical(x = x, allow_zero_length = allow_zero_length, allow_NA = allow_NA)))
    }
  }
}

# x for which return of is_logical() is equal to argument 'allow_zero_length'
for(allow_NA in false_true) {
  expect_silent(expect_true(
    is_logical(x = logical(0), allow_zero_length = TRUE, allow_NA = allow_NA)))
  expect_silent(expect_false(
    is_logical(x = logical(0), allow_zero_length = FALSE, allow_NA = allow_NA)))
}

# x for which return of is_logical() is equal to argument 'allow_NA'
for(allow_zero_length in false_true) {
  expect_silent(expect_true(
    is_logical(x = NA, allow_zero_length = allow_zero_length, allow_NA = TRUE)))
  expect_silent(expect_false(
    is_logical(x = NA, allow_zero_length = allow_zero_length, allow_NA = FALSE)))
}

# Arguments that should result in an error.
for(allow_NA in false_true) {
  expect_error(is_logical(x = TRUE, allow_zero_length = NA, allow_NA = allow_NA),
               pattern = "!is.na(allow_zero_length) is not TRUE", fixed = TRUE)
}

for(allow_zero_length in false_true) {
  expect_error(is_logical(x = TRUE, allow_zero_length = allow_zero_length, allow_NA = NA),
               pattern = "!is.na(allow_NA) is not TRUE", fixed = TRUE)
}


#### Remove objects used in tests ####
rm(allow_NA, allow_zero_length, false_true, x)
