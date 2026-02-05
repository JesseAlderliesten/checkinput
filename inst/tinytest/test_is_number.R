#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)


#### Test section 'Programming note' ####
expect_identical(mode(3.2), "numeric")
expect_identical(mode(3L), "numeric")
expect_identical(class(3.2), "numeric")
expect_identical(class(3L), "integer")


#### Test the examples ####
expect_true(is_number(x = 1))
expect_true(is_number(x = 3.14))
expect_false(is_number(x = c(1, 2)))
expect_true(all_numbers(x = c(1, 2)))
expect_false(is_number(x = "a"))
expect_false(is_number(x = numeric(0)))
expect_true(is_number(x = numeric(0), allow_zero = TRUE))
expect_false(is_number(x = NA_real_))
expect_true(is_number(x = NA_real_, allow_NA = TRUE))
expect_false(is_number(x = NA_character_, allow_NA = TRUE))
expect_false(is_number(x = NaN, allow_NA = TRUE))
expect_true(is_number(x = NaN, allow_NaN = TRUE))
expect_true(is_number(x = Inf))
expect_true(is_nonnegative(x = 3))
expect_true(is_nonnegative(x = 0))
expect_true(all_nonnegative(x = c(3, 0)))
expect_true(all_nonnegative(x = numeric(0), allow_zero = TRUE))
expect_true(is_positive(x = 3))
expect_false(is_positive(x = 0))


#### Tests ####
##### is_number #####
for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(-Inf, -314, 0, 314, Inf)) {
        expect_true(is_number(x = x, allow_zero = allow_zero,
                              allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}

for(allow_NA in false_true) {
  for(allow_zero in false_true) {
    expect_false(is_number(x = NaN, allow_zero = allow_zero,
                           allow_NA = allow_NA, allow_NaN = FALSE))
    expect_true(is_number(x = NaN, allow_zero = allow_zero,
                          allow_NA = allow_NA, allow_NaN = TRUE))
  }
}

for(allow_NaN in false_true) {
  for(allow_zero in false_true) {
    expect_false(is_number(x = NA_real_, allow_zero = allow_zero,
                           allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(is_number(x = NA_real_, allow_zero = allow_zero,
                          allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(is_number(x = NA_integer_, allow_zero = allow_zero,
                           allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(is_number(x = NA_integer_, allow_zero = allow_zero,
                          allow_NA = TRUE, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    expect_true(is_number(x = numeric(0), allow_zero = TRUE,
                          allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_false(is_number(x = numeric(0), allow_zero = FALSE,
                           allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_true(is_number(x = integer(0), allow_zero = TRUE,
                          allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_false(is_number(x = integer(0), allow_zero = FALSE,
                           allow_NA = allow_NA, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(NULL, FALSE, TRUE, NA, logical(0), c(0, 0), c(314, 314),
                    c(Inf, Inf), c(NA_real_, NA_real_), NA_complex_,
                    c(NA_integer_, NA_integer_), c(NaN, NaN), c(NaN, NA_real_),
                    "", "nco", character(0), NA_character_, data.frame(a = 314),
                    matrix(314), list(314), list())) {
        expect_false(is_number(x = x, allow_zero = allow_zero,
                               allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}

##### is_positive #####
for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(314, Inf)) {
        expect_true(is_positive(x = x, allow_zero = allow_zero,
                                allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}


for(allow_NA in false_true) {
  for(allow_zero in false_true) {
    expect_false(is_positive(x = NaN, allow_zero = allow_zero,
                             allow_NA = allow_NA, allow_NaN = FALSE))
    expect_true(is_positive(x = NaN, allow_zero = allow_zero,
                            allow_NA = allow_NA, allow_NaN = TRUE))
  }
}

for(allow_NaN in false_true) {
  for(allow_zero in false_true) {
    expect_false(is_positive(x = NA_real_, allow_zero = allow_zero,
                             allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(is_positive(x = NA_real_, allow_zero = allow_zero,
                            allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(is_positive(x = NA_integer_, allow_zero = allow_zero,
                             allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(is_positive(x = NA_integer_, allow_zero = allow_zero,
                            allow_NA = TRUE, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    expect_true(is_positive(x = numeric(0), allow_zero = TRUE,
                            allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_false(is_positive(x = numeric(0), allow_zero = FALSE,
                             allow_NA = allow_NA, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(NULL, FALSE, TRUE, NA, logical(0), -Inf, -314, 0, c(0, 0),
                    c(314, 314), c(Inf, Inf), c(NA_real_, NA_real_), NA_complex_,
                    c(NA_integer_, NA_integer_), c(NaN, NaN), c(NaN, NA_real_),
                    "", "nco", character(0), NA_character_, data.frame(a = 314),
                    matrix(314), list(314))) {
        expect_false(is_positive(x = x, allow_zero = allow_zero,
                                 allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}

##### is_nonnegative #####
for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(0, 314, Inf)) {
        expect_true(is_nonnegative(x = x, allow_zero = allow_zero,
                                   allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}

for(allow_NA in false_true) {
  for(allow_zero in false_true) {
    expect_false(is_nonnegative(x = NaN, allow_zero = allow_zero,
                                allow_NA = allow_NA, allow_NaN = FALSE))
    expect_true(is_nonnegative(x = NaN, allow_zero = allow_zero,
                               allow_NA = allow_NA, allow_NaN = TRUE))
  }
}

for(allow_NaN in false_true) {
  for(allow_zero in false_true) {
    expect_false(is_nonnegative(x = NA_real_, allow_zero = allow_zero,
                                allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(is_nonnegative(x = NA_real_, allow_zero = allow_zero,
                               allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(is_nonnegative(x = NA_integer_, allow_zero = allow_zero,
                                allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(is_nonnegative(x = NA_integer_, allow_zero = allow_zero,
                               allow_NA = TRUE, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    expect_true(is_nonnegative(x = numeric(0), allow_zero = TRUE,
                               allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_false(is_nonnegative(x = numeric(0), allow_zero = FALSE,
                                allow_NA = allow_NA, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(NULL, FALSE, TRUE, NA, logical(0), -Inf, -314, c(0, 0),
                    c(314, 314), c(Inf, Inf), c(NA_real_, NA_real_), NA_complex_,
                    c(NA_integer_, NA_integer_), c(NaN, NaN), c(NaN, NA_real_),
                    "", "nco", character(0), NA_character_, data.frame(a = 314),
                    matrix(314), list(314))) {
        expect_false(is_nonnegative(x = x, allow_zero = allow_zero,
                                    allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}

##### all_nonnegative #####
for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(0, 314, Inf, c(0, 0), c(314, 314), c(Inf, Inf))) {
        expect_true(all_nonnegative(x = x, allow_zero = allow_zero,
                                    allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}

for(allow_NA in false_true) {
  for(allow_zero in false_true) {
    expect_false(all_nonnegative(x = NaN, allow_zero = allow_zero,
                                 allow_NA = allow_NA, allow_NaN = FALSE))
    expect_true(all_nonnegative(x = NaN, allow_zero = allow_zero,
                                allow_NA = allow_NA, allow_NaN = TRUE))
    expect_false(all_nonnegative(x = c(NaN, NaN), allow_zero = allow_zero,
                                 allow_NA = allow_NA, allow_NaN = FALSE))
    expect_true(all_nonnegative(x = c(NaN, NaN), allow_zero = allow_zero,
                                allow_NA = allow_NA, allow_NaN = TRUE))
    if(allow_NA) {
      expect_false(all_nonnegative(x = c(NaN, NA_real_), allow_zero = allow_zero,
                                   allow_NA = allow_NA, allow_NaN = FALSE))
      expect_true(all_nonnegative(x = c(NaN, NA_real_), allow_zero = allow_zero,
                                  allow_NA = allow_NA, allow_NaN = TRUE))
    }
  }
}

for(allow_NaN in false_true) {
  for(allow_zero in false_true) {
    expect_false(all_nonnegative(x = NA_real_, allow_zero = allow_zero,
                                 allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(all_nonnegative(x = NA_real_, allow_zero = allow_zero,
                                allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(all_nonnegative(x = c(NA_real_, NA_real_), allow_zero = allow_zero,
                                 allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(all_nonnegative(x = c(NA_real_, NA_real_), allow_zero = allow_zero,
                                allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(all_nonnegative(x = NA_integer_, allow_zero = allow_zero,
                                 allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(all_nonnegative(x = NA_integer_, allow_zero = allow_zero,
                                allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(all_nonnegative(x = c(NA_integer_, NA_integer_), allow_zero = allow_zero,
                                 allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(all_nonnegative(x = c(NA_integer_, NA_integer_), allow_zero = allow_zero,
                                allow_NA = TRUE, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    expect_true(all_nonnegative(x = numeric(0), allow_zero = TRUE,
                                allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_false(all_nonnegative(x = numeric(0), allow_zero = FALSE,
                                 allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_true(all_nonnegative(x = integer(0), allow_zero = TRUE,
                                allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_false(all_nonnegative(x = integer(0), allow_zero = FALSE,
                                 allow_NA = allow_NA, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(NULL, FALSE, TRUE, NA, logical(0), -Inf, -314,
                    c(-314, -314), c(-Inf, Inf), NA_complex_,
                    "", "nco", character(0), NA_character_, data.frame(a = 314),
                    matrix(314), list(314), list())) {
        expect_false(all_nonnegative(x = x, allow_zero = allow_zero,
                                     allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}

##### all_numbers #####
for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(-Inf, -314, 0, 314, Inf, c(-314, -314), c(0, 0),
                    c(314, 314), c(Inf, Inf), c(-Inf, Inf))) {
        expect_true(all_numbers(x = x, allow_zero = allow_zero,
                                allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}

for(allow_NA in false_true) {
  for(allow_zero in false_true) {
    expect_false(all_numbers(x = NaN, allow_zero = allow_zero,
                             allow_NA = allow_NA, allow_NaN = FALSE))
    expect_true(all_numbers(x = NaN, allow_zero = allow_zero,
                            allow_NA = allow_NA, allow_NaN = TRUE))
    expect_false(all_numbers(x = c(NaN, NaN), allow_zero = allow_zero,
                             allow_NA = allow_NA, allow_NaN = FALSE))
    expect_true(all_numbers(x = c(NaN, NaN), allow_zero = allow_zero,
                            allow_NA = allow_NA, allow_NaN = TRUE))
    if(allow_NA) {
      expect_false(all_numbers(x = c(NaN, NA_real_), allow_zero = allow_zero,
                               allow_NA = allow_NA, allow_NaN = FALSE))
      expect_true(all_numbers(x = c(NaN, NA_real_), allow_zero = allow_zero,
                              allow_NA = allow_NA, allow_NaN = TRUE))
    }
  }
}

for(allow_NaN in false_true) {
  for(allow_zero in false_true) {
    expect_false(all_numbers(x = NA_real_, allow_zero = allow_zero,
                             allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(all_numbers(x = NA_real_, allow_zero = allow_zero,
                            allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(all_numbers(x = c(NA_real_, NA_real_), allow_zero = allow_zero,
                             allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(all_numbers(x = c(NA_real_, NA_real_), allow_zero = allow_zero,
                            allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(all_numbers(x = NA_integer_, allow_zero = allow_zero,
                             allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(all_numbers(x = NA_integer_, allow_zero = allow_zero,
                            allow_NA = TRUE, allow_NaN = allow_NaN))
    expect_false(all_numbers(x = c(NA_integer_, NA_integer_), allow_zero = allow_zero,
                             allow_NA = FALSE, allow_NaN = allow_NaN))
    expect_true(all_numbers(x = c(NA_integer_, NA_integer_), allow_zero = allow_zero,
                            allow_NA = TRUE, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    expect_true(all_numbers(x = numeric(0), allow_zero = TRUE,
                            allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_false(all_numbers(x = numeric(0), allow_zero = FALSE,
                             allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_true(all_numbers(x = integer(0), allow_zero = TRUE,
                            allow_NA = allow_NA, allow_NaN = allow_NaN))
    expect_false(all_numbers(x = integer(0), allow_zero = FALSE,
                             allow_NA = allow_NA, allow_NaN = allow_NaN))
  }
}

for(allow_NaN in false_true) {
  for(allow_NA in false_true) {
    for(allow_zero in false_true) {
      for(x in list(NULL, FALSE, TRUE, NA, logical(0), NA_complex_,
                    "", "nco", character(0), NA_character_, data.frame(a = 314),
                    matrix(314), list(314), list())) {
        expect_false(all_numbers(x = x, allow_zero = allow_zero,
                                 allow_NA = allow_NA, allow_NaN = allow_NaN))
      }
    }
  }
}


#### Remove objects used in tests ####
rm(allow_NA, allow_NaN, allow_zero, false_true, x)
