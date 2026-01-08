#### Create objects to use in tests ####
test_df_logi <- data.frame(a = TRUE)
test_mat_logi <- as.matrix(test_df_logi)
test_list_logi <- list(314)

test_df_num <- data.frame(a = 314)
test_mat_num <- as.matrix(test_df_num)
test_list_num <- list(314)

test_df_char <- data.frame(a = "nco")
test_mat_char <- as.matrix(test_df_char)
test_list_char  <- list("nco")


#### Test the examples ####
expect_true(is_zerolength(x = character(0)))
expect_false(is_zerolength(x = 0))
expect_true(is_zerolength(x = test_mat_num[numeric(0), , drop = FALSE]))
expect_false(is_zerolength(x = test_df_num[numeric(0), , drop = FALSE]))


#### Tests ####
for(x in list(NULL, numeric(0), character(0), data.frame(), matrix(numeric(0)),
              vector(mode = "list"),
              test_df_num[numeric(0), numeric(0), drop = FALSE],
              test_df_num[, numeric(0), drop = FALSE],
              test_mat_num[numeric(0), numeric(0), drop = FALSE],
              test_mat_num[, numeric(0), drop = FALSE],
              test_mat_num[numeric(0), , drop = FALSE],
              test_list_num[numeric(0)])) {
  expect_silent(expect_true(is_zerolength(x = x)))
}

for(x in list(FALSE, TRUE, NA, -Inf, -314, 0, 314, Inf, NaN, NA_character_, "",
              "nco", test_df_logi, test_mat_logi, test_list_logi,
              test_df_num, test_mat_num, test_list_num,
              test_df_char, test_mat_char, test_list_char,
              test_df_num[numeric(0), , drop = FALSE])) {
  expect_silent(expect_false(is_zerolength(x = x)))
}


#### Remove objects used in tests ####
rm(test_df_logi, test_mat_logi, test_list_logi,
   test_df_num, test_mat_num, test_list_num,
   test_df_char, test_mat_char, test_list_char)
