# Examples
expect_true(is_character(x = "a"))
expect_true(is_character(x = NA_character_, allow_NA = TRUE))
expect_false(is_character(x = NA, allow_NA = TRUE))

for(x in list(c("a", "b"), 1, NA_character_)) {
  expect_false(is_character(x = x))
}

# Tests
logicals <- list(FALSE, TRUE, NA)

for(allow_empty in logicals) {
  for(allow_zero in logicals) {
    for(allow_NA in logicals) {
      expect_true(is_character(x = "nco", allow_empty = allow_empty,
                               allow_zero = allow_zero, allow_NA = allow_NA))
    }
  }
}

for(x in list(NULL, FALSE, TRUE, NA, NaN, numeric(0), NA_real_, 0, 314, Inf,
              c(NA_character_, NA_character_), c("", ""), c("nco", ""),
              c("nco", "ibf"), data.frame(a = "nco"), matrix("nco"),
              list("nco"))) {
  for(allow_empty in logicals) {
    for(allow_zero in logicals) {
      for(allow_NA in logicals) {
        expect_false(is_character(x = x, allow_empty = allow_empty,
                                  allow_zero = allow_zero, allow_NA = allow_NA))
      }
    }
  }
}

# x for which return of is_character() is equal to argument 'allow_empty'
for(allow_zero in logicals) {
  for(allow_NA in logicals) {
    expect_true(is_character(x = "", allow_empty = TRUE,
                             allow_zero = allow_zero, allow_NA = allow_NA))
    expect_false(is_character(x = "", allow_empty = FALSE,
                              allow_zero = allow_zero, allow_NA = allow_NA))
    expect_true(is.na(is_character(x = "", allow_empty = NA,
                                   allow_zero = allow_zero, allow_NA = allow_NA)))
  }
}

# x for which return of is_character() is equal to argument 'allow_zero'
for(allow_empty in logicals) {
  for(allow_NA in logicals) {
    expect_true(is_character(x = character(0), allow_empty = allow_empty,
                             allow_zero = TRUE, allow_NA = allow_NA))
    expect_false(is_character(x = character(0), allow_empty = allow_empty,
                              allow_zero = FALSE, allow_NA = allow_NA))
    expect_true(is.na(is_character(x = character(0), allow_empty = allow_empty,
                                   allow_zero = NA, allow_NA = allow_NA)))
  }
}

# x for which return of is_character() is equal to argument 'allow_NA'
for(allow_empty in logicals) {
  for(allow_zero in logicals) {
    expect_true(is_character(x = NA_character_, allow_empty = allow_empty,
                             allow_zero = allow_zero, allow_NA = TRUE))
    expect_false(is_character(x = NA_character_, allow_empty = allow_empty,
                              allow_zero = allow_zero, allow_NA = FALSE))
    expect_true(is.na(is_character(x = NA_character_, allow_empty = allow_empty,
                                   allow_zero = allow_zero, allow_NA = NA)))
  }
}
