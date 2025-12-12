#### Test the examples ####
expect_true(is_nonnegative(x = 3))


#### Tests ####
for(x in list(0, 314, Inf)) {
  expect_true(is_nonnegative(x = x))
}

for(x in list(NULL, FALSE, TRUE, NA, numeric(0), c(NaN, NaN), c(NaN, NA_real_),
              c(NA_real_, NA_real_), character(0), NA_character_, "", "nco",
              -Inf, -314, c(0, 0), c(314, 314), c(Inf, Inf))) {
  expect_false(is_nonnegative(x = x))
}

for(x in list(NaN, NA_real_)) {
  expect_true(is.na(is_nonnegative(x = x)))
}
