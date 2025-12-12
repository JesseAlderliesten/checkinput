#### Test the examples ####
expect_true(all_nonnegative(x = c(3, 0)))


#### Tests ####
for(x in list(c(0, 0), c(314, 314), c(Inf, Inf))) {
  expect_true(all_nonnegative(x = x))
}

for(x in list(NULL, FALSE, TRUE, NA, character(0), numeric(0), "",
              NA_character_, "nco", -Inf -314, c(314, -314))) {
  expect_false(all_nonnegative(x = x))
}

for(x in list(NaN, NA_real_)) {
  expect_true(is.na(all_nonnegative(x = x)))
}
