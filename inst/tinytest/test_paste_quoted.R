#### Notes ####
# Testing the INTERNAL function in 'checkinput'.
# See the fuller, exported version of this function in package 'progutils'.


#### Create objects to use in tests ####
list_input <-  list("a", c("a", "b"), c("a", "b", "a"),
                    c(3, 4), NULL, NA, NA_character_)
list_output <- list("'a'", "'a', 'b'", "'a', 'b', 'a'",
                    "'3', '4'", "'NULL'", "'NA'", "'NA'")
list_input_zerolength <- list(NULL, character(0), numeric(0), logical(0), "")
list_output_zerolength <- list("'NULL'", "'character(0)'", "'numeric(0)'",
                               "'logical(0)'", "''")


#### Tests ####
for(index in seq_along(list_input)) {
  expect_identical(checkinput:::paste_quoted(x = list_input[[index]]),
                   list_output[[index]])
}

expect_error(
  checkinput:::paste_quoted(3, 4),
  pattern = "unused argument (4)", fixed = TRUE)

expect_error(
  checkinput:::paste_quoted(c(3, 4), 5:6),
  pattern = "unused argument (5:6)", fixed = TRUE)

expect_error(
  checkinput:::paste_quoted(c(3, 4), 5:6, 7),
  pattern = "unused arguments (5:6, 7)", fixed = TRUE)

expect_error(
  checkinput:::paste_quoted(c(3, 4), h = 5, 7),
  pattern = "unused arguments (h = 5, 7)", fixed = TRUE)

expect_identical(checkinput:::paste_quoted(c(3, 4)), "'3', '4'")
expect_identical(checkinput:::paste_quoted(c(a = 3, b = 4)), "'3', '4'")
expect_identical(checkinput:::paste_quoted(NULL), "'NULL'")

for(index_NULL in seq_along(list_input_zerolength)) {
  expect_silent(expect_identical(
    checkinput:::paste_quoted(list_input_zerolength[[index_NULL]]),
    list_output_zerolength[[index_NULL]]))
}


#### Remove objects used in tests ####
rm(index, index_NULL, list_input, list_input_zerolength, list_output,
   list_output_zerolength)
