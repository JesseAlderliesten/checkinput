for(x in list(314, Inf)) {
  expect_true(is_positive(x = x))
}

for(x in list(NULL, FALSE, TRUE, NA, numeric(0), c(NaN, NaN),
              c(NA_real_, NA_real_), -Inf, -314, 0, c(314, 314), c(Inf, Inf),
              character(0), NA_character_, "", "nco")) {
  expect_false(is_positive(x = x))
}

for(x in list(NaN, NA_real_)) {
  expect_true(is.na(is_positive(x = x)))
}
