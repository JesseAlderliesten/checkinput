#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)


#### Test the examples ####
expect_silent(expect_true(all_numbers(x = c(1, 2))))


#### Tests ####
for(allow_zero in false_true) {
  for(x in list(-314, 0, 314, c(-314, 0, 314), c(-Inf, Inf), c(NA_real_, NaN))) {
    expect_silent(expect_true(all_numbers(x = x, allow_zero = allow_zero)))
  }
}

expect_silent(expect_true(is_number(x = numeric(0), allow_zero = TRUE)))
expect_silent(expect_false(is_number(x = numeric(0), allow_zero = FALSE)))

for(allow_zero in false_true) {
  for(x in list(NULL, FALSE, TRUE, NA, "nco", character(0), "",
                NA_character_, NA_complex_, data.frame(a = 314), matrix(314),
                list(314))) {
    expect_silent(expect_false(all_numbers(x = x)))
  }
}


#### Remove objects used in tests ####
rm(allow_zero, false_true, x)
