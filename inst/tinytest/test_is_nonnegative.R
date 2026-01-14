#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)


#### Test the examples ####
expect_true(is_nonnegative(x = 3))


#### Tests ####
for(allow_zero in false_true) {
  for(x in list(0, 314, Inf)) {
    expect_silent(expect_true(is_nonnegative(x = x, allow_zero = allow_zero)))
  }
}

expect_silent(expect_true(is_number(x = numeric(0), allow_zero = TRUE)))
expect_silent(expect_false(is_number(x = numeric(0), allow_zero = FALSE)))

for(allow_zero in false_true) {
  for(x in list(NULL, FALSE, TRUE, NA, c(NaN, NaN), c(NaN, NA_real_),
                c(NA_real_, NA_real_), character(0), NA_character_, "", "nco",
                -Inf, -314, c(0, 0), c(314, 314), c(Inf, Inf))) {
    expect_silent(expect_false(is_nonnegative(x = x)))
  }
}

for(x in list(NaN, NA_real_)) {
  expect_silent(expect_true(is.na(is_nonnegative(x = x))))
}


#### Remove objects used in tests ####
rm(allow_zero, false_true, x)
