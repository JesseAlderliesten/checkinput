# Used in examples
for(x in list(1, NA_real_)) {
  expect_true(is_number(x = x))
}

for(x in list(c(1, 2), "a", NA_character_)) {
  expect_false(is_number(x = x))
}

# Tests
for(x in list(-Inf, -314, 0, 314, Inf, NaN)) {
  expect_true(is_number(x = x))
}

for(x in list(NULL, FALSE, TRUE, NA, numeric(0), c(NA_real_, NA_real_),
              c(NaN, NaN), c(314, 314), character(0), NA_character_, "", "nco")) {
  expect_false(is_number(x = x))
}
