#### Notes ####
# - This file tests the INTERNAL function checkinput:::paste_quoted().
# - A fuller, exported version of this function is progutils::paste_quoted().
# - Also adjust the tests of progutils::paste_quoted() when making changes here!


#### Create objects to use in tests ####
list_input <-  list("a", c("a", "b"), c("a", "b", "a"),
                    c(3, 4), NULL, NA, NA_character_)
list_output <- list("'a'", "'a', 'b'", "'a', 'b', 'a'",
                    "'3', '4'", "'NULL'", "'NA'", "'NA'")
list_input_zerolength <- list(NULL, character(0), numeric(0), logical(0),
                              vector(mode = "list"), "")
list_output_zerolength <- list("'NULL'", "'character(0)'", "'numeric(0)'",
                               "'logical(0)'", "'list(0)'", "''")
x_fact_ind <- c(4:6, 5L)
x_fact <- as.factor(letters[x_fact_ind])
x_fact_int <- as.factor(x_fact_ind)
x_fact_num <- as.factor(x_fact_ind / 16)


#### Tests ####
# Examples in the exported function progutils::paste_quoted
# Exported version progutils::paste_quoted() warns about dropping names
expect_silent(expect_identical(checkinput:::paste_quoted(c(a = 3, b = 4)),
                               "'3', '4'"))
expect_identical(checkinput:::paste_quoted(c(a = 3, b = 4)), "'3', '4'")
expect_identical(checkinput:::paste_quoted(NULL), "'NULL'")

# Tests proper
for(index in seq_along(list_input)) {
  expect_identical(checkinput:::paste_quoted(x = list_input[[index]]), list_output[[index]])
}

expect_silent(expect_identical(checkinput:::paste_quoted(x = x_fact),
                               "'d', 'e', 'f', 'e'"))
expect_silent(expect_identical(checkinput:::paste_quoted(x = x_fact_int),
                               "'4', '5', '6', '5'"))
expect_silent(expect_identical(checkinput:::paste_quoted(x = x_fact_num),
                               "'0.25', '0.3125', '0.375', '0.3125'"))

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

for(index_NULL in seq_along(list_input_zerolength)) {
  expect_silent(expect_identical(
    checkinput:::paste_quoted(list_input_zerolength[[index_NULL]]),
    list_output_zerolength[[index_NULL]]))
}

for(x in list(data.frame(a = 314), as.matrix(data.frame(a = 314)))) {
  expect_error(
    checkinput:::paste_quoted(x),
    pattern = "is.null(dim(x)) is not TRUE", fixed = TRUE)
}


#### Remove objects used in tests ####
rm(index, index_NULL, list_input, list_input_zerolength, list_output,
   list_output_zerolength, x, x_fact, x_fact_ind, x_fact_int, x_fact_num)
