#### Create objects to use in tests ####
empty_string_quoted <- "'\"\"' (i.e., an empty string)"
false_true <- list(FALSE, TRUE)
# Although invalid could be suspicious, (e.g., "3X.3", "3X.234") invalid names
# will not have been created by make.names()!
invalid <- c("", ".3a", "for", "NA", "a/b", "a-b", "3a", "3X.3", "3X.234")
invalid_quoted <- paste0(checkinput:::paste_quoted(invalid[-1]), ", ",
                         empty_string_quoted)
use_makenames <- ".\nUse 'x <- make.names(x, unique = TRUE)"
valid_nonsusp <- c("A", ".a", ".V1", ".V234", "VV1", "VV234", "X.", "X.3.",
                   "X.234.", "V1.", "V234.", "X.3X", "X.234X", "V1V", "V234V",
                   "X.a2", "X..", "X..X", "X.2X", "X.X", "X.X.X", "X.A", "A.X.A")
valid_susp1 <- c("X", "X.3", "X.2", "X.234", "V1", "V234")
valid_susp1_quoted <- checkinput:::paste_quoted(valid_susp1)
valid_susp2 <- c("e.3", ".X.3", ".X.234", "XX.3", "XX.234", "X..3", "XX.2",
                 "X.X.2", "X..2", "Xa.2", "A.X.2")
valid_susp2_quoted <- checkinput:::paste_quoted(valid_susp2)
warn_dots <- "consist only of dots: '.'"
warn_dupl <- "are duplicated: "
warn_susp_v1 <- "might have been created by read.csv: "
warn_susp_v2 <- "might have been modified by make.names(x, unique = TRUE): "
warn_syntax <- "are syntactically invalid: "
x_underscores <- c("abc_def", "ghi", "jk_l")


#### Test the examples ####
expect_true(all_names(x = names(c(a = 1, b = 2))))

expect_warning(expect_false(
  all_names(x = names(c(a = 1, 2)))),
  pattern = paste0(warn_syntax, empty_string_quoted), strict = TRUE, fixed = TRUE)
expect_warning(expect_false(
  all_names(x = NULL)), pattern = "'x' is NULL", strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(x = c("a", "b", "a"))),
  pattern = paste0(warn_dupl, "'a'"), strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(x = "X.3", allow_susp = FALSE)),
  pattern = paste0(warn_susp_v1, "'X.3'"), strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "X.3", allow_susp = TRUE))

expect_warning(expect_false(
  all_names(x = "e.3", allow_susp = FALSE)),
  pattern = paste0(warn_susp_v2, "'e.3'"), strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "e.3", allow_susp = TRUE))

expect_silent(expect_true(all_names(x = x_underscores, allow_underscores = TRUE)))

expect_warning(expect_false(
  all_names(x = x_underscores, allow_underscores = FALSE)),
  pattern = "Names contain underscores: 'abc_def', 'jk_l'",
  strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(x = c("abc.def", "..abc..def..", ".", "..", "...", "...."))),
  pattern ="Names consist only of dots: '.', '..', '...', '....'",
  strict = TRUE, fixed = TRUE)


#### Test section 'Details' ####
expect_true(all_names("X.2a"))
expect_equal(
  make.names(names = rep(c("ab", "NA", "", NA_character_), each = 2L),
             unique = TRUE),
  c("ab", "ab.1", "NA.", "NA..1", "X", "X.1", "NA..2", "NA..3"))
expect_equal(make.names(names = c(NA, NA), unique = TRUE),
             c("NA.", "NA..1"))
expect_equal(make.names(names = rep(c(NA_real_, 3), each = 2), unique = TRUE),
             c("NA.", "NA..1", "X3", "X3.1"))


#### Tests ####
# zero-length values
expect_warning(expect_false(
  all_names(x = NULL, allow_susp = TRUE)),
  pattern = "'x' is NULL", strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(x = character(0), allow_susp = TRUE)),
  pattern = "x has length zero but is not NULL", strict = TRUE, fixed = TRUE)

# Unique valid, not suspicious
expect_silent(expect_true(
  all_names(x = valid_nonsusp, allow_susp = TRUE)))

# Duplicated valid, not suspicious
expect_warning(expect_false(
  all_names(x = c(valid_nonsusp, valid_nonsusp[c(2, 5)]), allow_susp = FALSE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(valid_nonsusp[c(2, 5)])),
  strict = TRUE, fixed = TRUE)

for(allow_susp in false_true) {
  # Unique invalid, not suspicious
  expect_warning(expect_false(
    all_names(x = invalid, allow_susp = allow_susp)),
    pattern = paste0(warn_syntax, invalid_quoted), strict = TRUE, fixed = TRUE)

  expect_warning(expect_false(
    all_names(x = "", allow_susp = allow_susp)),
    pattern = paste0(warn_syntax, empty_string_quoted, use_makenames),
    strict = TRUE, fixed = TRUE)

  expect_warning(expect_false(
    all_names(x = NA_character_, allow_susp = allow_susp)),
    pattern = paste0(warn_syntax, "'NA'", use_makenames), strict = TRUE,
    fixed = TRUE)

  expect_warning(
    all_names(x = NA, allow_susp = allow_susp),
    pattern = "'x' is not a character vector!", strict = TRUE, fixed = TRUE)

  # Unique valid, suspicious v1
  if(allow_susp) {
    expect_true(
      all_names(x = valid_susp1, allow_susp = allow_susp))
  } else {
    expect_warning(expect_false(
      all_names(x = valid_susp1, allow_susp = allow_susp)),
      pattern = paste0(warn_susp_v1, valid_susp1_quoted), strict = TRUE,
      fixed = TRUE)
  }

  # Unique valid, suspicious v2
  if(allow_susp) {
    expect_true(
      all_names(x = valid_susp2, allow_susp = allow_susp))
  } else {
    expect_warning(expect_false(
      all_names(x = valid_susp2, allow_susp = allow_susp)),
      pattern = paste0(warn_susp_v2, valid_susp2_quoted),
      strict = TRUE, fixed = TRUE)
  }
}

# Duplicated invalid, not suspicious
expect_warning(expect_false(
  all_names(x = c(invalid, invalid[2]))),
  pattern = paste0(warn_dupl, "'", invalid[2], "'; and ", warn_syntax,
                   invalid_quoted, use_makenames), strict = TRUE,
  fixed = TRUE)

for(allow_susp in false_true) {
  # Mix
  names_mix <- c(valid_nonsusp, valid_susp1, valid_susp2, invalid)

  expect_warning(expect_false(
    all_names(names_mix, allow_susp = TRUE)),
    pattern = paste0(warn_syntax, invalid_quoted, use_makenames),
    strict = TRUE, fixed = TRUE)

  expect_warning(expect_false(
    all_names(names_mix, allow_susp = FALSE)),
    pattern = paste0(warn_syntax, invalid_quoted, "; and ", warn_susp_v1,
                     valid_susp1_quoted, "; and ", warn_susp_v2,
                     valid_susp2_quoted, use_makenames),
    strict = TRUE, fixed = TRUE)
}

expect_silent(expect_true(
  all_names(x = x_underscores, allow_underscores = TRUE)))

expect_warning(expect_false(
  all_names(x = x_underscores, allow_underscores = FALSE)),
  pattern = paste0(
    "Names contain underscores: 'abc_def', 'jk_l'.\nUse 'x <-",
    " make.names(x, unique = TRUE, allow_ = FALSE)' to\ncreate unique,",
    " syntactically valid names without underscores"),
  strict = TRUE, fixed = TRUE)

# Duplicated valid, suspicious v1
expect_warning(expect_false(
  all_names(x = c(valid_susp1, valid_susp1[c(2, 4)]), allow_susp = TRUE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(valid_susp1[c(2, 4)]),
                   use_makenames),
  strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(x = c(valid_susp1, valid_susp1[c(2, 4)]), allow_susp = FALSE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(valid_susp1[c(2, 4)]),
                   "; and ", warn_susp_v1, valid_susp1_quoted),
  strict = TRUE, fixed = TRUE)


# Duplicated valid, suspicious v2
warning("Create tests for input values of type 'Duplicated valid, suspicious v2'!")


# only dots
expect_warning(expect_false(
  all_names(c(".", ".a.", "..a..", "...", "b."))),
  pattern = paste0("Names ", warn_dots, ", '...'"), strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(c(".", ".a.", ".", "..a..", "...", "b.", "..."))),
  pattern = paste0(warn_dupl, "'.', '...'; and ", warn_dots, ", '...'"),
  strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(c(".", "."))),
  pattern = paste0(warn_dupl, "'.'; and ", warn_dots), strict = TRUE,
  fixed = TRUE)


expect_error(all_names(),
             pattern = "argument \"x\" is missing, with no default", fixed = TRUE)
expect_error(all_names(x = names(c(a = 1, b = 2)), allow_susp = NA),
             pattern = "is_logical(allow_susp) is not TRUE", fixed = TRUE)
expect_error(all_names(x = names(c(a = 1, b = 2)), allow_underscores = NA),
             pattern = "is_logical(allow_underscores) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(allow_susp, empty_string_quoted, false_true, invalid,
   invalid_quoted, names_mix, valid_nonsusp, valid_susp1, valid_susp1_quoted,
   valid_susp2, valid_susp2_quoted, use_makenames, warn_dots, warn_dupl,
   warn_susp_v1, warn_susp_v2, warn_syntax, x_underscores)
