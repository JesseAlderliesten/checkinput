#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)


#### Test the examples ####
expect_silent(expect_true(all_nonnegative(x = c(3, 0))))
expect_silent(expect_true(all_nonnegative(numeric(0), allow_zero = TRUE)))


#### Tests ####
for(allow_zero in false_true) {
  for(x in list(c(0, 0), c(314, 314), c(Inf, Inf))) {
    expect_silent(expect_true(all_nonnegative(x = x, allow_zero = allow_zero)))
  }
}

expect_silent(expect_true(is_number(x = numeric(0), allow_zero = TRUE)))
expect_silent(expect_false(is_number(x = numeric(0), allow_zero = FALSE)))

for(allow_zero in false_true) {
  for(x in list(NULL, FALSE, TRUE, NA, character(0), "",
                NA_character_, NA_complex_, "nco", -Inf -314, c(314, -314),
                data.frame(a = 314), as.matrix(data.frame(a = 314)), list())) {
    expect_silent(expect_false(all_nonnegative(x = x)))
  }
}

for(x in list(NaN, NA_real_)) {
  expect_silent(expect_true(is.na(all_nonnegative(x = x))))
}


#### Remove objects used in tests ####
rm(allow_zero, false_true, x)
