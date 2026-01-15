#### Create objects to use in tests ####
empty_string_quoted <- "'\"\"' (i.e., an empty string)"
false_true <- list(FALSE, TRUE)
# Although invalid could be suspicious, (e.g., "3X.3", "3X.234") invalid names
# will not have been created by make.names()!
invalid <- c("", ".3a", "for", "NA", "a/b", "a-b", "3a", "3X.3", "3X.234")
invalid_quoted <- paste0(checkinput:::paste_quoted(invalid[-1]), ", ",
                         empty_string_quoted)
use_makenames <- ".\nUse 'x <- make.names(x, unique = TRUE)"
valid <- c("A", ".a", ".V1", ".V234", "VV1", "VV234", "X.", "X.3.", "X.234.",
           "V1.", "V234.", "X.3X", "X.234X", "V1V", "V234V", "X.a2", "X..",
           "X..X", "X.2X", "X.X", "X.X.X", "X.A", "A.X.A")
x_underscores <- c("abc_def", "ghi", "jk_l")

susp_both <- c(".X.3", "X..3", ".X.234", "X..2")
susp_both_quoted <- checkinput:::paste_quoted(susp_both)
susp_makenames <- c("e.3", "XX.3", "XX.234", "XX.2", "X.X.2", "Xa.2", "A.X.2")
susp_makenames_quoted <- checkinput:::paste_quoted(susp_makenames)
susp_readcsv <- c("X", "X.3", "X.2", "X.234", "V1", "V234")
susp_readcsv_quoted <- checkinput:::paste_quoted(susp_readcsv)
susp_vecasnames <- c(".a", ".V1", ".V234", "..abc..def..")
susp_vecasnames_quoted <- checkinput:::paste_quoted(susp_vecasnames)

warn_dots <- "only consist of dots: '.'"
warn_dupl <- "are duplicated: "
warn_susp_both <- "modified by make.names(x, unique = TRUE) or by vctrs::vec_as_names(x): "
warn_susp_makenames <- "might have been modified by make.names(x, unique = TRUE): "
warn_susp_readcsv <- "might have been created by read.csv: "
warn_susp_vecasnames <- "might have been modified by vctrs::vec_as_names(x): "
warn_syntax <- "are syntactically invalid: "


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
  pattern = paste0(warn_susp_readcsv, "'X.3'"), strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "X.3", allow_susp = TRUE))

expect_warning(expect_false(
  all_names(x = "e.3", allow_susp = FALSE)),
  pattern = paste0(warn_susp_makenames, "'e.3'"), strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "e.3", allow_susp = TRUE))

expect_silent(expect_true(all_names(x = x_underscores, allow_underscores = TRUE)))

expect_warning(expect_false(
  all_names(x = x_underscores, allow_underscores = FALSE)),
  pattern = "Names contain underscores: 'abc_def', 'jk_l'",
  strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(x = c(".", "..", "...", "...."))),
  pattern = paste0(warn_dots, ", '..', '...', '....'"), strict = TRUE, fixed = TRUE)

expect_silent(expect_true(all_names(x = c("abc.def", "abc..def.."))))

expect_warning(expect_false(all_names(x = "..abc..def..")),
               pattern = paste0(warn_susp_vecasnames, "'..abc..def..'"),
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
  all_names(x = valid, allow_susp = TRUE)))

# Duplicated valid, not suspicious
expect_warning(expect_false(
  all_names(x = c(valid, valid[c(2, 5)]), allow_susp = FALSE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(valid[c(2, 5)])),
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

  for(x in list(NA, data.frame(a = "nco"), as.matrix(data.frame(a = "nco")),
                list())) {
    expect_warning(
      all_names(x = x, allow_susp = allow_susp),
      pattern = "'x' is not a character vector!", strict = TRUE, fixed = TRUE)
  }
}

expect_true(all_names(x = susp_readcsv, allow_susp = TRUE))
expect_warning(expect_false(
  all_names(x = susp_readcsv, allow_susp = FALSE)),
  pattern = paste0(warn_susp_readcsv, susp_readcsv_quoted),
  strict = TRUE, fixed = TRUE)

expect_true(all_names(x = susp_makenames, allow_susp = TRUE))
expect_warning(expect_false(
  all_names(x = susp_makenames, allow_susp = FALSE)),
  pattern = paste0(warn_susp_makenames, susp_makenames_quoted),
  strict = TRUE, fixed = TRUE)

expect_true(all_names(x = susp_both, allow_susp = TRUE))
expect_warning(expect_false(
  all_names(x = susp_both, allow_susp = FALSE)),
  pattern = paste0(warn_susp_both, susp_both_quoted),
  strict = TRUE, fixed = TRUE)

expect_true(all_names(x = susp_vecasnames, allow_susp = TRUE))
expect_warning(expect_false(
  all_names(x = susp_vecasnames, allow_susp = FALSE)),
  pattern = paste0(warn_susp_vecasnames, susp_vecasnames_quoted),
  strict = TRUE, fixed = TRUE)


# Duplicated invalid, not suspicious
expect_warning(expect_false(
  all_names(x = c(invalid, invalid[2]))),
  pattern = paste0(warn_dupl, "'", invalid[2], "'; and ", warn_syntax,
                   invalid_quoted, use_makenames), strict = TRUE,
  fixed = TRUE)

# Mix
expect_warning(expect_false(
  all_names(c(valid, susp_readcsv, susp_makenames, susp_vecasnames, invalid), allow_susp = TRUE)),
  pattern = paste0(warn_syntax, invalid_quoted, use_makenames),
  strict = TRUE, fixed = TRUE)
expect_warning(expect_false(
  all_names(c(valid, susp_readcsv, susp_makenames, susp_vecasnames, invalid), allow_susp = FALSE)),
  pattern = paste0(warn_syntax, invalid_quoted, "; and ", warn_susp_readcsv,
                   susp_readcsv_quoted, "; and ", warn_susp_makenames,
                   susp_makenames_quoted, "; and ", warn_susp_vecasnames,
                   susp_vecasnames_quoted, use_makenames),
  strict = TRUE, fixed = TRUE)

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
  all_names(x = c(susp_readcsv, susp_readcsv[c(2, 4)]), allow_susp = TRUE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(susp_readcsv[c(2, 4)]),
                   use_makenames),
  strict = TRUE, fixed = TRUE)

expect_warning(expect_false(
  all_names(x = c(susp_readcsv, susp_readcsv[c(2, 4)]), allow_susp = FALSE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(susp_readcsv[c(2, 4)]),
                   "; and ", warn_susp_readcsv, susp_readcsv_quoted),
  strict = TRUE, fixed = TRUE)


# Duplicated valid, suspicious v2
# Not yet created tests for input values of type 'Duplicated valid, suspicious v2'!


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
rm(allow_susp, empty_string_quoted, false_true, invalid, invalid_quoted,
   susp_both, susp_both_quoted, susp_makenames, susp_makenames_quoted,
   susp_readcsv, susp_readcsv_quoted, susp_vecasnames, susp_vecasnames_quoted,
   use_makenames, valid, warn_dots, warn_dupl, warn_susp_both,
   warn_susp_makenames, warn_susp_readcsv, warn_susp_vecasnames, warn_syntax, x,
   x_underscores)
