#### Create objects to use in tests ####
NA_NaN_num <- c(NA_real_, NA_integer_, NaN)


#### Test the examples ####
expect_identical(are_NA_numeric(c(-Inf, -3, 0, 3, Inf), allow_NaN = TRUE),
                 rep(FALSE, 5))

expect_identical(are_NA_numeric(NA_NaN_num), c(TRUE, TRUE, FALSE))
expect_identical(are_NA_numeric(NA_NaN_num, allow_NaN = TRUE),
                 c(TRUE, TRUE, TRUE))
expect_false(are_NA_numeric(NA, allow_NaN = TRUE))

for(x in list(numeric(0), logical(0), character(0))) {
  expect_false(are_NA_numeric(x = x))
}


#### Test programming notes ####
expect_true(is.na(NA))
expect_identical(is.na(NA_NaN_num), rep(TRUE, 3L))
expect_true(is.na(NA_complex_))
expect_identical(is.na(logical(0)), logical(0))
expect_identical(is.na(numeric(0)), logical(0))

expect_false(is.nan(NA))
expect_identical(is.nan(c(NA_real_, NA_integer_)), rep(FALSE, 2L))
expect_true(is.nan(NaN))
expect_false(is.nan(NA_complex_))
expect_identical(is.nan(logical(0)), logical(0))
expect_identical(is.nan(numeric(0)), logical(0))


#### Tests ####
expect_identical(are_NA_numeric(x = c(-Inf, -314, 0, 314, Inf), allow_NaN = FALSE),
                 rep(FALSE, 5L))

expect_identical(are_NA_numeric(x = c(-Inf, -314, 0, 314, Inf), allow_NaN = TRUE),
                 rep(FALSE, 5L))

expect_identical(are_NA_numeric(x = NA_NaN_num, allow_NaN = FALSE),
                 c(TRUE, TRUE, FALSE))

expect_identical(are_NA_numeric(x = NA_NaN_num, allow_NaN = TRUE),
                 rep(TRUE, 3L))

for(x in list(NULL, FALSE, TRUE, NA, logical(0), numeric(0), "nco",
              character(0), "", NA_character_, NA_complex_, list(314))) {
  expect_false(are_NA_numeric(x = x, allow_NaN = TRUE))
}

for(x in list(data.frame(a = 314), matrix(314))) {
  expect_error(are_NA_numeric(x = x, allow_NaN = TRUE),
               pattern = "is.null(dim(x)) is not TRUE", fixed = TRUE)
}


#### Remove objects used in tests ####
rm(NA_NaN_num)
