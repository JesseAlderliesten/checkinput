#### Test section 'Programming note' ####
expect_identical(mode(3.2), "numeric")
expect_identical(mode(3L), "numeric")
expect_identical(class(3.2), "numeric")
expect_identical(class(3L), "integer")


#### Test the examples ####
for(x in list(1, NA_real_, NaN, Inf)) {
  expect_true(is_number(x = x))
}

for(x in list(c(1, 2), "a", numeric(0), NA_character_)) {
  expect_false(is_number(x = x))
}


#### Tests ####
# NaN is also considered a number.
for(x in list(-Inf, -314, 0, 314, Inf, NaN)) {
  expect_true(is_number(x = x))
}

for(x in list(NULL, FALSE, TRUE, NA, numeric(0), c(NA_real_, NA_real_),
              c(NaN, NaN), c(314, 314), character(0), NA_character_, NA_complex_,
              "", "nco", data.frame(a = 314), matrix(314), list(314))) {
  expect_false(is_number(x = x))
}
