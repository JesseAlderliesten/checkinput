#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)


#### Test the examples ####
expect_true(is_character(x = "a"))
expect_true(is_character(x = NA_character_, allow_NA = TRUE))
expect_false(is_character(x = NA, allow_NA = TRUE))

for(x in list(c("a", "b"), 1, NA_character_)) {
  expect_silent(expect_false(is_character(x = x)))
}


#### Tests ####
for(allow_empty in false_true) {
  for(allow_zero in false_true) {
    for(allow_NA in false_true) {
      expect_silent(expect_true(is_character(x = "nco", allow_empty = allow_empty,
                               allow_zero = allow_zero, allow_NA = allow_NA)))
    }
  }
}

for(x in list(NULL, FALSE, TRUE, NA, NaN, numeric(0), NA_real_, 0, 314, Inf,
              c(NA_character_, NA_character_), c("", ""), c("nco", ""),
              c("nco", "ibf"), data.frame(a = "nco"), matrix("nco"),
              list("nco"))) {
  for(allow_empty in false_true) {
    for(allow_zero in false_true) {
      for(allow_NA in false_true) {
        expect_silent(expect_false(is_character(x = x, allow_empty = allow_empty,
                                  allow_zero = allow_zero, allow_NA = allow_NA)))
      }
    }
  }
}

# x for which return of is_character() is equal to argument 'allow_empty'
for(allow_zero in false_true) {
  for(allow_NA in false_true) {
    expect_silent(expect_true(is_character(x = "", allow_empty = TRUE,
                             allow_zero = allow_zero, allow_NA = allow_NA)))
    expect_silent(expect_false(is_character(x = "", allow_empty = FALSE,
                              allow_zero = allow_zero, allow_NA = allow_NA)))
  }
}

# x for which return of is_character() is equal to argument 'allow_zero'
for(allow_empty in false_true) {
  for(allow_NA in false_true) {
    expect_silent(expect_true(is_character(x = character(0), allow_empty = allow_empty,
                             allow_zero = TRUE, allow_NA = allow_NA)))
    expect_silent(expect_false(is_character(x = character(0), allow_empty = allow_empty,
                              allow_zero = FALSE, allow_NA = allow_NA)))
  }
}

# x for which return of is_character() is equal to argument 'allow_NA'
for(allow_empty in false_true) {
  for(allow_zero in false_true) {
    expect_silent(expect_true(is_character(x = NA_character_, allow_empty = allow_empty,
                             allow_zero = allow_zero, allow_NA = TRUE)))
    expect_silent(expect_false(is_character(x = NA_character_, allow_empty = allow_empty,
                              allow_zero = allow_zero, allow_NA = FALSE)))
  }
}

# Arguments that should result in an error.
for(allow_zero in false_true) {
  for(allow_NA in false_true) {
    expect_error(is_character(x = "nco", allow_empty = NA,
                              allow_zero = allow_zero, allow_NA = allow_NA),
                 pattern = "is_logical(allow_empty) is not TRUE", fixed = TRUE)
  }
}

for(allow_empty in false_true) {
  for(allow_NA in false_true) {
    expect_error(all_characters(x = "nco", allow_empty = allow_empty,
                                allow_zero = NA, allow_NA = allow_NA),
                 pattern = "is_logical(allow_zero) is not TRUE", fixed = TRUE)
  }
}

for(allow_empty in false_true) {
  for(allow_zero in false_true) {
    expect_error(all_characters(x = "nco", allow_empty = allow_empty,
                                allow_zero = allow_zero, allow_NA = NA),
                 pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
  }
}


#### Remove objects used in tests ####
rm(allow_empty, allow_zero, allow_NA, false_true)
