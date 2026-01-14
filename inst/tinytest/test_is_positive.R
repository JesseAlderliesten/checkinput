#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)


#### Test the examples ####
expect_true(is_positive(x = 3))
expect_false(is_positive(x = 0))


#### Tests ####
for(allow_zero in false_true) {
  for(x in list(314, Inf)) {
    expect_silent(expect_true(is_positive(x = x, allow_zero = allow_zero)))
  }
}

expect_silent(expect_true(is_number(x = numeric(0), allow_zero = TRUE)))
expect_silent(expect_false(is_number(x = numeric(0), allow_zero = FALSE)))

for(allow_zero in false_true) {
  for(x in list(NULL, FALSE, TRUE, NA, c(NaN, NaN),
                c(NA_real_, NA_real_), -Inf, -314, 0, c(314, 314), c(Inf, Inf),
                character(0), NA_character_, NA_complex_, "", "nco",
                data.frame(a = 314), as.matrix(data.frame(a = 314)), list())) {
    expect_silent(expect_false(is_positive(x = x)))
  }
}

for(x in list(NaN, NA_real_)) {
  expect_silent(expect_true(is.na(is_positive(x = x))))
}


#### Remove objects used in tests ####
rm(allow_zero, false_true, x)
