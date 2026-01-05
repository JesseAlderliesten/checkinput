#### Create objects to use in tests ####
false_true <- list(FALSE, TRUE)
warn_syntax <- "syntactically invalid values: "
warn_dupl <- "duplicated names: "
warn_susp_v1 <- "names that might have been created by read.csv: '"
warn_susp_v2 <- paste0("names that might have been modified by",
                       " make.names\\(x, unique = TRUE): '")
use_makenames <- ".\nUse 'x <- make.names\\(x, unique = TRUE)"

valid_nonsusp <- c("A", ".a", ".V1", ".V234", "VV1", "VV234", "X.", "X.1.",
                   "X.234.", "V1.", "V234.", "X.1X", "X.234X", "V1V", "V234V",
                   "X.a2", "X..", "X..X", "X.2X", "X.X", "X.X.X", "X.A", "A.X.A")
valid_susp1 <- c("X", "X.1", "X.2", "X.234", "V1", "V234")
valid_susp1_quoted <- paste(valid_susp1, collapse = "', '")
valid_susp2 <- c("e.1", ".X.1", ".X.234", "XX.1", "XX.234", "X..1", "XX.2",
                 "X.X.2", "X..2", "Xa.2", "A.X.2")
valid_susp2_quoted <- paste(valid_susp2, collapse = "', '")
# Although invalid could be suspicious, (e.g., "3X.1", "3X.234") invalid names
# will not have been created by make.names()!
invalid <- c("", ".3a", "for", "NA", "a/b", "a-b", "3a", "3X.1", "3X.234")
empty_string_quoted <- "'\"\"' \\(i.e., an empty string)"
invalid_quoted <- paste0("'", paste0(invalid[-1], collapse = "', '"),
                         "', ", empty_string_quoted)
x_underscores <- c("abc_def", "ghi", "jk_l")


#### Test the examples ####
expect_true(all_names(x = names(c(a = 1, b = 2))))

expect_warning(expect_false(
  all_names(x = names(c(a = 1, 2)))),
  pattern = paste0(warn_syntax, empty_string_quoted), strict = TRUE)
expect_warning(expect_false(
  all_names(x = NULL)), pattern = "'x' is NULL", strict = TRUE)

expect_warning(expect_false(
  all_names(x = c("a", "b", "a"))),
  pattern = paste0(warn_dupl, "'a'"), strict = TRUE)
expect_true(all_names(x = c("a", "b", "a"), allow_dupl = TRUE))

expect_warning(expect_false(
  all_names(x = "X.1", allow_susp = FALSE)),
  pattern = paste0(warn_susp_v1, "X.1'"), strict = TRUE)
expect_true(all_names(x = "X.1", allow_susp = TRUE))

expect_warning(expect_false(
  all_names(x = "e.1", allow_susp = FALSE)),
  pattern = paste0(warn_susp_v2, "e.1'"), strict = TRUE)
expect_true(all_names(x = "e.1", allow_susp = TRUE))

expect_silent(expect_true(all_names(x = x_underscores, allow_underscores = TRUE)))

expect_warning(expect_false(
  all_names(x = x_underscores, allow_underscores = FALSE)),
  pattern = "'x' contains values containing underscores: 'abc_def', 'jk_l'",
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
  pattern = "'x' is NULL", strict = TRUE)

expect_warning(expect_false(
  all_names(x = character(0), allow_susp = TRUE)),
  pattern = "zero-length values", strict = TRUE)

# Unique valid, not suspicious
expect_silent(expect_true(
  all_names(x = valid_nonsusp, allow_susp = TRUE)))

# Duplicated valid, not suspicious
expect_true(
  all_names(x = c(valid_nonsusp, valid_nonsusp[c(2, 5)]),
            allow_dupl = TRUE, allow_susp = FALSE))

expect_warning(expect_false(
  all_names(x = c(valid_nonsusp, valid_nonsusp[c(2, 5)]),
            allow_dupl = FALSE, allow_susp = FALSE)),
  pattern = paste0(warn_dupl, "'",
                   paste0(valid_nonsusp[c(2, 5)], collapse = "', '"), "'"),
  strict = TRUE)

for(allow_susp in false_true) {
  # Unique invalid, not suspicious
  expect_warning(expect_false(
    all_names(x = invalid, allow_susp = allow_susp)),
    pattern = paste0(warn_syntax, invalid_quoted), strict = TRUE)

  expect_warning(expect_false(
    all_names(x = "", allow_susp = allow_susp)),
    pattern = paste0(warn_syntax, empty_string_quoted, use_makenames),
    strict = TRUE)

  expect_warning(expect_false(
    all_names(x = NA_character_, allow_susp = allow_susp)),
    pattern = paste0(warn_syntax, "'NA'", use_makenames), strict = TRUE)

  expect_warning(expect_false(
    all_names(x = NA, allow_susp = allow_susp)),
    pattern = paste0(warn_syntax, "'NA'", use_makenames), strict = TRUE)

  # Unique valid, suspicious v1
  if(allow_susp) {
    expect_true(
      all_names(x = valid_susp1, allow_susp = allow_susp))
  } else {
    expect_warning(expect_false(
      all_names(x = valid_susp1, allow_susp = allow_susp)),
      pattern = paste0(warn_susp_v1, valid_susp1_quoted, "'"), strict = TRUE)
  }

  # Unique valid, suspicious v2
  if(allow_susp) {
    expect_true(
      all_names(x = valid_susp2, allow_susp = allow_susp))
  } else {
    expect_warning(expect_false(
      all_names(x = valid_susp2, allow_susp = allow_susp)),
      pattern = paste0(warn_susp_v2, valid_susp2_quoted, "'"),
      strict = TRUE)
  }
}

# Duplicated invalid, not suspicious
expect_warning(expect_false(
  all_names(x = c(invalid, invalid[2]), allow_dupl = TRUE)),
  pattern = paste0(warn_syntax, invalid_quoted, use_makenames),
  strict = TRUE)

expect_warning(expect_false(
  all_names(x = c(invalid, invalid[2]), allow_dupl = FALSE)),
  pattern = paste0(warn_syntax, invalid_quoted, "; and ", warn_dupl,
                   "'", invalid[2], "'", use_makenames), strict = TRUE)

for(allow_dupl in false_true) {
  for(allow_susp in false_true) {
    # Mix
    names_mix <- c(valid_nonsusp, valid_susp1, valid_susp2, invalid)

    expect_warning(expect_false(
      all_names(names_mix, allow_susp = TRUE)),
      pattern = paste0(warn_syntax, invalid_quoted, use_makenames),
      strict = TRUE)

    expect_warning(expect_false(
      all_names(names_mix, allow_susp = FALSE)),
      pattern = paste0(warn_syntax, invalid_quoted, "; and ", warn_susp_v1,
                       valid_susp1_quoted, "'.; and ", warn_susp_v2, # TO DO: remove extraneous dot.
                       valid_susp2_quoted, "'.", use_makenames), # TO DO: remove extraneous dot.
      strict = TRUE)
  }
}

expect_silent(expect_true(
  all_names(x = x_underscores, allow_underscores = TRUE)))

expect_warning(expect_false(
  all_names(x = x_underscores, allow_underscores = FALSE)),
  pattern = paste0("'x' contains values containing underscores: 'abc_def', 'jk_l'.",
                   "\nUse 'x <- make.names(x, unique = TRUE, allow_ = FALSE)'",
                   " to create unique,\nsyntactically valid names without underscores"),
  strict = TRUE, fixed = TRUE)

# Duplicated valid, suspicious v1
expect_warning(expect_false(
  all_names(x = c(valid_susp1, valid_susp1[c(2, 4)]), allow_dupl = FALSE,
            allow_susp = TRUE)),
  pattern = paste0(warn_dupl, "'", paste0(valid_susp1[c(2, 4)], collapse = "', '"),
                   "'", use_makenames), strict = TRUE)

expect_true(
  all_names(x = c(valid_susp1, valid_susp1[c(2, 4)]), allow_dupl = TRUE,
            allow_susp = TRUE))

expect_warning(expect_false(
  all_names(x = c(valid_susp1, valid_susp1[c(2, 4)]), allow_dupl = FALSE,
            allow_susp = FALSE)),
  pattern = paste0(warn_dupl, "'", paste0(valid_susp1[c(2, 4)], collapse = "', '"),
                   "'; and ", warn_susp_v1, valid_susp1_quoted, "'"), strict = TRUE)

expect_warning(expect_false(
  all_names(x = c(valid_susp1, valid_susp1[c(2, 4)]), allow_dupl = TRUE,
            allow_susp = FALSE)),
  pattern = paste0(warn_susp_v1, valid_susp1_quoted, "'"), strict = TRUE)


# Duplicated valid, suspicious v2
# ... nog maken

expect_error(all_names(),
             pattern = "argument \"x\" is missing, with no default")
expect_error(all_names(x = names(c(a = 1, b = 2)), allow_dupl = NA),
             pattern = "is_logical\\(allow_dupl) is not TRUE")
expect_error(all_names(x = names(c(a = 1, b = 2)), allow_susp = NA),
             pattern = "is_logical\\(allow_susp) is not TRUE")
expect_error(all_names(x = names(c(a = 1, b = 2)), allow_underscores = NA),
             pattern = "is_logical\\(allow_underscores) is not TRUE")


#### Remove objects used in tests ####
rm(allow_dupl, allow_susp, empty_string_quoted, false_true, invalid,
   invalid_quoted, names_mix, valid_nonsusp, valid_susp1, valid_susp1_quoted,
   valid_susp2, valid_susp2_quoted, use_makenames, warn_dupl, warn_syntax,
   warn_susp_v1, warn_susp_v2, x_underscores)
