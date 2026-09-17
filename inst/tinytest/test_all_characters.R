#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)


#### Test the examples ####
expect_true(is_character(x = "a"))
expect_true(all_characters(x = c("a", "b")))
expect_true(is_character(x = NA_character_, allow_NA = TRUE))
expect_false(is_character(x = NA, allow_NA = TRUE))

for(x in list(c("a", "b"), 1, NA_character_)) {
  expect_false(is_character(x = x))
}


#### Tests ####
for(x in list("nco", c("nco", "ibf"))) {
  for(allow_empty in false_true) {
    for(allow_zerolength in false_true) {
      for(allow_NA in false_true) {
        expect_true(
          all_characters(x = x, allow_empty = allow_empty,
                         allow_zerolength = allow_zerolength, allow_NA = allow_NA))
      }
    }
  }
}

for(allow_empty in false_true) {
  for(allow_zerolength in false_true) {
    for(allow_NA in false_true) {
      expect_true(
        is_character(x = "nco", allow_empty = allow_empty,
                     allow_zerolength = allow_zerolength, allow_NA = allow_NA))
    }
  }
}

for(x in list(NULL, FALSE, TRUE, NA, -Inf, -314, 0, 314, Inf, numeric(0), NaN,
              NA_real_, NaN, data.frame(a = 314), as.matrix(data.frame(a = 314)),
              list())) {
  for(allow_empty in false_true) {
    for(allow_zerolength in false_true) {
      for(allow_NA in false_true) {
        expect_false(
          all_characters(x = x, allow_empty = allow_empty,
                         allow_zerolength = allow_zerolength, allow_NA = allow_NA))
      }
    }
  }
}

for(x in list(NULL, FALSE, TRUE, NA, NaN, numeric(0), NA_real_, 0, 314, Inf,
              c(NA_character_, NA_character_), c("", ""), c("nco", ""),
              c("nco", "ibf"), data.frame(a = "nco"), matrix("nco"),
              list("nco"))) {
  for(allow_empty in false_true) {
    for(allow_zerolength in false_true) {
      for(allow_NA in false_true) {
        expect_false(
          is_character(x = x, allow_empty = allow_empty,
                       allow_zerolength = allow_zerolength, allow_NA = allow_NA))
      }
    }
  }
}

# x for which return of all_characters() is equal to argument 'allow_empty'
for(x in list("", c("nco", ""), c("", ""))) {
  for(allow_zerolength in false_true) {
    for(allow_NA in false_true) {
      expect_true(
        all_characters(x = x, allow_empty = TRUE,
                       allow_zerolength = allow_zerolength, allow_NA = allow_NA))
      expect_false(
        all_characters(x = x, allow_empty = FALSE,
                       allow_zerolength = allow_zerolength, allow_NA = allow_NA))
    }
  }
}

# x for which return of is_character() is equal to argument 'allow_empty'
for(allow_zerolength in false_true) {
  for(allow_NA in false_true) {
    expect_true(
      is_character(x = "", allow_empty = TRUE,
                   allow_zerolength = allow_zerolength, allow_NA = allow_NA))
    expect_false(
      is_character(x = "", allow_empty = FALSE,
                   allow_zerolength = allow_zerolength, allow_NA = allow_NA))
  }
}

# x for which return of all_characters() and is_character() is equal to argument 'allow_zerolength'
for(allow_empty in false_true) {
  for(allow_NA in false_true) {
    expect_true(
      all_characters(x = character(0), allow_empty = allow_empty,
                     allow_zerolength = TRUE, allow_NA = allow_NA))
    expect_false(
      all_characters(x = character(0), allow_empty = allow_empty,
                     allow_zerolength = FALSE, allow_NA = allow_NA))
    expect_true(
      is_character(x = character(0), allow_empty = allow_empty,
                   allow_zerolength = TRUE, allow_NA = allow_NA))
    expect_false(
      is_character(x = character(0), allow_empty = allow_empty,
                   allow_zerolength = FALSE, allow_NA = allow_NA))
  }
}

# x for which return of all_characters() is equal to argument 'allow_NA'
for(x in list(NA_character_, c(NA_character_, NA_character_),
              c("nco", NA_character_, "ibf"))) {
  for(allow_empty in false_true) {
    for(allow_zerolength in false_true) {
      expect_true(
        all_characters(x = x, allow_empty = allow_empty,
                       allow_zerolength = allow_zerolength, allow_NA = TRUE))
      expect_false(
        all_characters(x = x, allow_empty = allow_empty,
                       allow_zerolength = allow_zerolength, allow_NA = FALSE))
    }
  }
}

# x for which return of is_character() is equal to argument 'allow_NA'
for(allow_empty in false_true) {
  for(allow_zerolength in false_true) {
    expect_true(
      is_character(x = NA_character_, allow_empty = allow_empty,
                   allow_zerolength = allow_zerolength, allow_NA = TRUE))
    expect_false(
      is_character(x = NA_character_, allow_empty = allow_empty,
                   allow_zerolength = allow_zerolength, allow_NA = FALSE))
  }
}

##### Arguments that should result in an error #####
for(allow_zerolength in false_true) {
  for(allow_NA in false_true) {
    expect_error(
      all_characters(x = c("nco", "ibf"), allow_empty = NA,
                     allow_zerolength = allow_zerolength, allow_NA = allow_NA),
      pattern = "is_logical(allow_empty) is not TRUE", fixed = TRUE)
  }
}

for(allow_empty in false_true) {
  for(allow_NA in false_true) {
    expect_error(
      all_characters(x = c("nco", "ibf"), allow_empty = allow_empty,
                     allow_zerolength = NA, allow_NA = allow_NA),
      pattern = "is_logical(allow_zerolength) is not TRUE", fixed = TRUE)
  }
}

for(allow_empty in false_true) {
  for(allow_zerolength in false_true) {
    expect_error(
      all_characters(x = c("nco", "ibf"), allow_empty = allow_empty,
                     allow_zerolength = allow_zerolength, allow_NA = NA),
      pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
  }
}

for(allow_zerolength in false_true) {
  for(allow_NA in false_true) {
    expect_error(is_character(x = "nco", allow_empty = NA,
                              allow_zerolength = allow_zerolength, allow_NA = allow_NA),
                 pattern = "is_logical(allow_empty) is not TRUE", fixed = TRUE)
  }
}

for(allow_empty in false_true) {
  for(allow_NA in false_true) {
    expect_error(
      all_characters(x = "nco", allow_empty = allow_empty,
                     allow_zerolength = NA, allow_NA = allow_NA),
      pattern = "is_logical(allow_zerolength) is not TRUE", fixed = TRUE)
  }
}

for(allow_empty in false_true) {
  for(allow_zerolength in false_true) {
    expect_error(
      all_characters(x = "nco", allow_empty = allow_empty,
                     allow_zerolength = allow_zerolength, allow_NA = NA),
      pattern = "is_logical(allow_NA) is not TRUE", fixed = TRUE)
  }
}


#### Remove objects used in tests ####
rm(allow_empty, allow_NA, allow_zerolength, false_true, x)
