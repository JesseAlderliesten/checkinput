for(x in list(314, Inf, c(314, 314))) {
  expect_true(all_positive(x = x))
}

for(x in list(NULL, FALSE, TRUE, NA, numeric(0), -Inf, -314, 0, character(0),
              NA_character_, "", "nco")) {
  expect_false(all_positive(x = x))
}

for(x in list(NaN, NA_real_, c(314, NA_real_))) {
  expect_true(is.na(all_positive(x = x)))
}
