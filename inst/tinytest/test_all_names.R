#### Create objects to use in tests ####
##### Sets #####
x_invld_caught <- c(
  ".1a", "_0", "_0A", "_A", "_A0", "1a._", # invalid start
  "for", "NA", # reserved words
  c("a-b", "a1$", "a1._#", "ab#cd", "c/d")) # contain invalid characters
x_csv_df <- c("V1", "V234")
x_dupl <- c(".X", "X.", "ab", "a.", "A.X", "X.X")
x_invld <- c("", x_invld_caught)
x_invld_empty_q <- "'\"\"' (i.e., an empty string)"
x_invld_mn_made <- make.names(names = x_invld_caught)
x_invld_q <- paste0(checkinput:::paste_quoted(x_invld[-1]), ", ",
                    x_invld_empty_q)
x_invld_vcsnm_repaired <- vctrs::vec_as_names(names = x_invld_caught,
                                              repair = "universal_quiet")
x_mknm <- c(
  # started with a dot followed by a number: c(X0.3", "X0.33")
  make.names(c(.3, .33)),
  # started with a digit: c("X1.1", "X3.1")
  make.names(c("1.1", "3.1")))
x_mknm_q <- checkinput:::paste_quoted(x_mknm)
x_mknm_csv <- c(
  # first empty column with read.csv(x, header = TRUE): "X.1"
  make.names(rep("", 2L), unique = TRUE)[2L],
  "X.2", "X.234") # read.csv(x, header = TRUE): 3rd and 235th unnamed column
x_mknm_csv_q <- checkinput:::paste_quoted(x_mknm_csv)
# c("X1", "X3", "X33"): consisted only of digits;
# c("X0", "X00", "X03", "X013", "X033"): consisted only of digits;
# c("X0cc", "X00cc", "X03cc", "X033cc", "X3cc", "X33cc", "X0.3cc", "X0.33cc"):
# started with a digit; c("X_", "X_.", "X_ab", "X_3b"): started with an underscore;
# c("X.0", "X.00", "X.03"): started with a dot followed by a number;
# c("X.0a", "X.03a"): started with a dot followed by a number;
# c("FALSE.", "for.", "NA.", "TRUE.", "while."): were reserved words
x_mknm_F <- make.names(
  c(".0", ".00", ".03", ".03a", ".0a", "_", "_.", "_3b", "_ab", "0", "0.33cc",
    "0.3cc", "00", "00cc", "013", "03", "033", "033cc", "03cc", "0cc", "1", "3",
    "33", "33cc", "3cc", "FALSE", "for", "NA", "TRUE", "while"))
# "TRUE..1": a reserved word; c(".X.1", ".X.2", "X..1", "X..2", "XX.1", "XX.2",
# "a..1", "a..2", "A.X.1", "A.X.2", "X.X.1", "X.X.2", "Xa.1", "Xa.2")
x_mknm_T <- make.names(names = rep(x = x_dupl, each = 3L), unique = TRUE)
x_mknm_T <- c(make.names(rep(TRUE, 2L), unique = TRUE)[-1L],
              x_mknm_T[!(x_mknm_T %in% x_dupl)], "c..1", "c..314")
x_mknm_T_q <- checkinput:::paste_quoted(x_mknm_T)
# Partly from help(vctrs::vec_as_names) and their test suite
x_mknm_T_vcsnm <- c(
  # or vctrs::vec_as_names(as.character(c(1:3)), repair = "universal")
  make.names(rep("..", 4L), unique = TRUE)[-1L], # c("...1", "...2", "...3")
  # or vctrs::vec_as_names(rep("if", 2L), repair = "universal")
  make.names(rep("&if<=", 3L), unique = TRUE)[-1L], # c(".if...1", ".if...2")
  "a...2", "X...11")
x_mknm_T_vcsnm_q <- checkinput:::paste_quoted(x_mknm_T_vcsnm)
x_mknm_vcsnm <- "X0...11"
x_susp_dot <- c("..a..", ".a", ".a.", ".V1", ".V234", "a...03", "V1.", "V234.",
                "X..", "X..X", "X.A", "X.a2", "X.b.", "X.X.X")
x_susp_undersc <- c(".a1_", "a.1_", "a1._", "X._")
x_susp_undersc_q <- checkinput:::paste_quoted(x_susp_undersc)
# From help(vctrs::vec_as_names) and their test suite, created by
# vctrs::vec_as_names(c("0a", "1a", "22c", "2fa", "_", "_0", "_a.", "_a1",
#                       "_foo", "_z", FALSE, "for", "if", "Inf", "NA_real_",
#                       "TRUE", "#$3##"), repair = "universal")
x_vcsnm <- c("..0a", "..1a", "..22c", "..2fa", "..3..", "._", "._0", "._a.",
             "._a1", "._foo", "._z", ".FALSE", ".for", ".if", ".Inf",
             ".NA_real_", ".TRUE")
x_vcsnm_q <- checkinput:::paste_quoted(x_vcsnm)
x_vld <- c("a", "A", "C", "nco", "V", "V0", "V03", "v1", "V1V", "V234V", "VV1",
           "VV234", "x", "x0", "Xa", "XX")
x_vld_undersc <- c("g_hi", "V_", "V0_3", "V1V_", "V2_34V", "VV_1", "VV_234")

##### Messages and warnings #####
use_mknm <- ".\nUse 'x <- make.names(x, unique = TRUE)"
use_mknm_undersc <- paste0(
  ".\nUse 'x <- make.names(x, unique = TRUE, allow_ = FALSE)' to create",
  " unique,\nsyntactically valid names without underscores")
warn_dots <- "consist of only dots, which is a reserved word: "
warn_dots_pattern <- paste0("consist of two dots followed by digits, which is",
                            " a reserved word: ")
warn_dupl <- "are duplicated: "
warn_suspicious <- "Names are suspicious: "
warn_syntax <- "are syntactically invalid: "
warn_undersc <- paste0("contain underscores (which are not allowed if",
                       " 'allow_underscores' is FALSE):\n")
note_mknm_dots <- paste0("\n(it does not recognise names that consist of only",
                         " dots, or two dots followed by digits)")


#### Test the examples ####
expect_true(all_names(x = c("a", "b1a")))
expect_warning(
  expect_false(all_names(x = c("a", "b1a", "a"))),
  pattern = paste0(warn_dupl, "'a'", use_mknm), strict = FALSE, fixed = TRUE)

invalid_names <- c("a", "ab#cd", "", "for", "..", "..23")
expect_warning(
  expect_false(all_names(x = invalid_names)),
  pattern = paste0(
    "Names are syntactically invalid: 'ab#cd', 'for', ", x_invld_empty_q,
    "; and ", warn_dots, "'..'; and ", warn_dots_pattern, "'..23'", use_mknm,
    "' to create unique, syntactically valid names", note_mknm_dots),
  strict = FALSE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = make.names(invalid_names))),
  pattern = paste0(warn_dots, "'..'; and ", warn_dots_pattern,
                   "'..23'; and are suspicious: 'ab.cd', 'X', 'for.'"),
  strict = FALSE, fixed = TRUE)

x_susp_example <- c("e.2", "a.1b", ".TRUE", "..22c", "a...2",
                    "V3", "X.2", "X0...11", "X0.3", "X3")
expect_warning(
  expect_false(all_names(x = x_susp_example)),
  pattern = paste0("Names are suspicious: ",
                   checkinput:::paste_quoted(x_susp_example)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = "abc_def", allow_underscores = FALSE)),
  pattern = paste0("Names ", warn_undersc, "'abc_def'", use_mknm_undersc),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "abc_def", allow_underscores = TRUE))

expect_warning(
  expect_false(all_names(x = names(1:3))),
  pattern = "'x' is NULL", strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(13)),
  pattern = "'x' is not a character vector: 13", strict = TRUE, fixed = TRUE)


#### Test some sets ####
expect_true(identical(c(x_vld, x_vld_undersc),
                      make.names(c(x_vld, x_vld_undersc))))
expect_true(all(is.na(x_invld) | (x_invld != make.names(x_invld))))

expect_silent(expect_identical(make.names(x_susp_dot), x_susp_dot))
expect_silent(expect_identical(make.names(x_susp_undersc), x_susp_undersc))


#### Test the used patterns ####
# Catch not removing names that consist of all dots before looking for
# suspicious names
expect_warning(
  expect_false(all_names("..")),
  pattern = paste0(warn_dots, "'..'"), strict = TRUE, fixed = TRUE)

# Catch using a dot (i.e., 'any character') instead of a literal dot to create
# bool_susp_csv
expect_silent(expect_true(all_names(x = "Xa123")))

# Catch using any digit instead of digits one to nine for first digit to create
# bool_susp_csv_dataframe
expect_silent(expect_true(all_names(x = "V023")))

# Catch using dots (i.e., 'any character') instead of literal dots
expect_silent(expect_true(all_names(x = "abc123")))


#### Tests ####
##### Non-character-vector #####
for(x in list(NA, data.frame(a = "nco"), as.matrix(data.frame(a = "nco")),
              list(a = 314), list(), 314)) {
  expect_warning(
    expect_false(all_names(x = x)),
    pattern = "Input to 'x' is not a character vector: x", strict = TRUE, fixed = TRUE)
}

##### Zero-length values #####
expect_warning(
  expect_false(all_names(x = NULL)),
  pattern = "'x' is NULL: did you use names() or colnames() on an object without",
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = character(0))),
  pattern = "x has length zero but is not NULL", strict = TRUE, fixed = TRUE)

##### Duplicated values #####
expect_warning(
  expect_false(all_names(x = c(x_vld, x_vld[c(2, 3)]))),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_vld[c(2, 3)])),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_invld, x_invld[c(2, 3)]))),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_invld[c(2, 3)]),
                   "; and ", warn_syntax, x_invld_q, use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_susp_dot, x_susp_dot[c(2, 3)]))),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_susp_dot[c(2, 3)]),
                   "; and are suspicious: ", checkinput:::paste_quoted(x_susp_dot)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_vld_undersc, x_vld_undersc[c(2, 3)]),
                         allow_underscores = TRUE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_vld_undersc[c(2, 3)]),
                   use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_vld_undersc, x_vld_undersc[c(2, 3)]),
                         allow_underscores = FALSE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_vld_undersc[c(2, 3)]),
                   "; and ", warn_undersc, checkinput:::paste_quoted(x_vld_undersc),
                   use_mknm_undersc),
  strict = TRUE, fixed = TRUE)

##### Valid and invalid names #####
expect_silent(
  expect_true(all_names(x = c(x_vld, x_vld_undersc), allow_underscores = TRUE)))

# Invalid names are not created by make.names(), so should not be suspicious
expect_warning(
  expect_false(
    all_names(x = c(x_vld, x_vld_undersc, x_invld), allow_underscores = TRUE)
  ),
  pattern = paste0(warn_syntax, x_invld_q, use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = "")),
  pattern = paste0(warn_syntax, x_invld_empty_q, use_mknm),
  strict = TRUE, fixed = TRUE)

##### Only dots or patterned dots #####
for(x in list(".", "..", "...", "....")) {
  expect_warning(
    expect_false(all_names(x = x)),
    pattern = paste0(warn_dots, "'", x, "'"), strict = TRUE, fixed = TRUE)
}

for(x in list("..1", "..314")) {
  expect_warning(
    expect_false(all_names(x = x)),
    pattern = paste0(warn_dots_pattern, "'", x, "'"),
    strict = TRUE, fixed = TRUE)
}

##### Respect 'allow_underscores' #####
expect_silent(
  expect_true(all_names(x = c(x_vld, x_vld_undersc),
                        allow_underscores = TRUE)))

expect_warning(
  expect_false(all_names(x = c(x_vld, x_vld_undersc),
                         allow_underscores = FALSE)),
  pattern = paste0("Names ", warn_undersc,
                   checkinput:::paste_quoted(x_vld_undersc),
                   use_mknm_undersc), strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_susp_undersc, allow_undersc = TRUE)),
  pattern = paste0("are suspicious: ", x_susp_undersc_q),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_susp_undersc, allow_undersc = FALSE)),
  pattern = paste0("Names ", warn_undersc, x_susp_undersc_q, use_mknm_undersc),
  strict = TRUE, fixed = TRUE)

##### Suspicious names #####
expect_warning(
  # Also created by read.csv(x, header = TRUE) to name the first unnamed column
  expect_false(all_names(x = make.names(""))), # "X"
  pattern = paste0(warn_suspicious, "'X'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_csv_df)),
  pattern = paste0(warn_suspicious, checkinput:::paste_quoted(x_csv_df)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_invld_mn_made)),
  pattern = paste0("Names are suspicious: ",
                   checkinput:::paste_quoted(x_invld_mn_made)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_invld_vcsnm_repaired)),
  pattern = paste0("Names are suspicious: ",
                   checkinput:::paste_quoted(x_invld_vcsnm_repaired)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_mknm_F)),
  pattern = paste0(warn_suspicious, checkinput:::paste_quoted(x_mknm_F)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_mknm_T)),
  pattern = paste0(warn_suspicious, x_mknm_T_q),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_mknm)),
  pattern = paste0(warn_suspicious, x_mknm_q),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_mknm_csv)),
  pattern = paste0(warn_suspicious, x_mknm_csv_q),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_mknm_T_vcsnm)),
  pattern = paste0(warn_suspicious, x_mknm_T_vcsnm_q),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_mknm_vcsnm)),
  pattern = paste0(warn_suspicious, checkinput:::paste_quoted(x_mknm_vcsnm)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_susp_dot)),
  pattern = paste0(warn_suspicious, checkinput:::paste_quoted(x_susp_dot)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_vcsnm)),
  pattern = paste0(warn_suspicious, x_vcsnm_q),
  strict = TRUE, fixed = TRUE)

##### Mix #####
expect_warning(
  expect_false(
    all_names(x = c(x_invld, x_vld, x_vld_undersc, x_mknm, x_mknm_csv, x_mknm_T,
                    x_mknm_T_vcsnm, x_vcsnm), allow_underscores = TRUE)
  ),
  pattern = paste0(warn_syntax, x_invld_q, "; and are suspicious: ",
                   x_mknm_q, ", ", x_mknm_csv_q, ", ", x_mknm_T_q, ", ",
                   x_mknm_T_vcsnm_q, ", ", x_vcsnm_q, use_mknm),
  strict = TRUE, fixed = TRUE)

if(any(grepl(pattern = "_", x = c(x_mknm_q, x_mknm_csv_q, x_mknm_T_q,
                                  x_mknm_T_vcsnm_q), fixed = TRUE))) {
  warning("Next test will fail because some sets contain underscores that are",
          " not removed when listing suspicious names.")
}

expect_warning(
  expect_false(
    all_names(x = c(x_vld, x_mknm, x_mknm_csv, x_mknm_T, x_mknm_T_vcsnm,
                    x_vcsnm, x_vld_undersc, x_invld), allow_underscores = FALSE)
  ),
  pattern = paste0(
    warn_syntax, x_invld_q, "; and ", warn_undersc,
    checkinput:::paste_quoted(
      # x_invld should NOT be included here, because those are returned as
      # invalid, not as containing underscores!
      grep(pattern = "_", x = c(x_vld, x_mknm, x_mknm_csv, x_mknm_T,
                                x_mknm_T_vcsnm, x_vcsnm, x_vld_undersc),
           value = TRUE, fixed = TRUE)),
    "; and are suspicious: ", x_mknm_q, ", ", x_mknm_csv_q, ", ", x_mknm_T_q,
    ", ", x_mknm_T_vcsnm_q, ", ",
    checkinput:::paste_quoted(grep(pattern = "_", x = x_vcsnm, value = TRUE,
                                   fixed = TRUE, invert = TRUE)),
    use_mknm_undersc),
  strict = TRUE, fixed = TRUE)

##### Erroneous input #####
expect_error(all_names(),
             pattern = "argument \"x\" is missing, with no default",
             fixed = TRUE)
expect_error(all_names(x = names(c(a = 1, b = 2)), allow_underscores = NA),
             pattern = "is_logical(allow_underscores) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(invalid_names, note_mknm_dots, use_mknm, use_mknm_undersc, warn_dots,
   warn_dots_pattern, warn_dupl, warn_suspicious, warn_syntax, warn_undersc, x,
   x_csv_df, x_dupl, x_invld, x_invld_caught, x_invld_empty_q, x_invld_mn_made,
   x_invld_q, x_invld_vcsnm_repaired, x_mknm, x_mknm_csv, x_mknm_csv_q,
   x_mknm_F, x_mknm_q, x_mknm_T, x_mknm_T_q, x_mknm_T_vcsnm, x_mknm_T_vcsnm_q,
   x_mknm_vcsnm, x_susp_dot, x_susp_example, x_susp_undersc, x_susp_undersc_q,
   x_vcsnm, x_vcsnm_q, x_vld, x_vld_undersc)
