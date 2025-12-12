logicals <- list(FALSE, TRUE, NA)

#### Test the examples ####
expect_true(all_characters(x = c("a", "b")))


#### Tests ####
for(x in list("nco", c("nco", "ibf"))) {
  for(allow_empty in logicals) {
    for(allow_zero in logicals) {
      for(allow_NA in logicals) {
        expect_true(all_characters(x = x, allow_empty = allow_empty,
                                   allow_zero = allow_zero, allow_NA = allow_NA))
      }
    }
  }
}

for(x in list(NULL, FALSE, TRUE, NA, -Inf, -314, 0, 314, Inf, numeric(0), NaN,
              NA_real_, NaN)) {
  for(allow_empty in logicals) {
    for(allow_zero in logicals) {
      for(allow_NA in logicals) {
        expect_false(all_characters(x = x, allow_empty = allow_empty,
                                    allow_zero = allow_zero, allow_NA = allow_NA))
      }
    }
  }
}

# x for which return of all_characters() is equal to argument 'allow_empty'
for(x in list("", c("nco", ""), c("", ""))) {
  for(allow_zero in logicals) {
    for(allow_NA in logicals) {
      expect_true(all_characters(x = x, allow_empty = TRUE,
                                 allow_zero = allow_zero, allow_NA = allow_NA))
      expect_false(all_characters(x = x, allow_empty = FALSE,
                                  allow_zero = allow_zero, allow_NA = allow_NA))
      expect_true(is.na(all_characters(x = x, allow_empty = NA,
                                       allow_zero = allow_zero, allow_NA = allow_NA)))
    }
  }
}

# x for which return of all_characters() is equal to argument 'allow_zero'
for(allow_empty in logicals) {
  for(allow_NA in logicals) {
    expect_true(all_characters(x = character(0), allow_empty = allow_empty,
                               allow_zero = TRUE, allow_NA = allow_NA))
    expect_false(all_characters(x = character(0), allow_empty = allow_empty,
                                allow_zero = FALSE, allow_NA = allow_NA))
    expect_true(is.na(all_characters(x = character(0), allow_empty = allow_empty,
                                     allow_zero = NA, allow_NA = allow_NA)))
  }
}

# x for which return of all_characters() is equal to argument 'allow_NA'
for(x in list(NA_character_, c(NA_character_, NA_character_),
              c("nco", NA_character_, "ibf"))) {
  for(allow_empty in logicals) {
    for(allow_zero in logicals) {
      expect_true(all_characters(x = x, allow_empty = allow_empty,
                                 allow_zero = allow_zero, allow_NA = TRUE))
      expect_false(all_characters(x = x, allow_empty = allow_empty,
                                  allow_zero = allow_zero, allow_NA = FALSE))
      expect_true(is.na(all_characters(x = x, allow_empty = allow_empty,
                                       allow_zero = allow_zero, allow_NA = NA)))
    }
  }
}
