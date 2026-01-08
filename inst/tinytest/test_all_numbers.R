#### Test the examples ####
expect_silent(expect_true(all_numbers(x = c(1, 2))))


#### Tests ####
for(x in list(-314, 0, 314, c(-314, 0, 314), c(-Inf, Inf), c(NA_real_, NaN))) {
  expect_silent(expect_true(all_numbers(x = x)))
}

for(x in list(NULL, FALSE, TRUE, NA, numeric(0), "nco", character(0), "",
              NA_character_, NA_complex_, data.frame(a = 314), matrix(314),
              list(314))) {
  expect_silent(expect_false(all_numbers(x = x)))
}
