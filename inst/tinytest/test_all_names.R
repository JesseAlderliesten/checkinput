#### Create objects to use in tests ####
##### Sets #####
x_invld_caught <- c(
  ".1a", "_0", "_0A", "_A", "_A0", "1a._", # invalid start
  "for", "NA") # reserved words
x_invld_caughtnot <- c("a-b", "a1$", "a1._#", "ab#cd", "c/d")

x_csv_df <- c("V1", "V234")
x_csv_df_q <- checkinput:::paste_quoted(x_csv_df)
x_dupl <- c(".X", "X.", "XX", "a", "a.", "A.X", "X.X", "Xa")
x_invld <- c("", x_invld_caught, x_invld_caughtnot)
x_invld_empty_q <- "'\"\"' (i.e., an empty string)"
x_invld_mn_made <- make.names(names = x_invld_caught)
x_invld_q <- paste0(checkinput:::paste_quoted(x_invld[-1]), ", ",
                    x_invld_empty_q)
x_invld_vcsnm_repaired <- vctrs::vec_as_names(names = x_invld_caught,
                                              repair = "universal_quiet")
x_mknm <- c(
  # started with a dot followed by a number: c(X0.3", "X0.33")
  make.names(c(.3, .33)),
  # started with a digit; or (not identified) read.csv(x, header = TRUE) with
  # duplicated column names '1' and '3': c("X1.1", "X3.1")
  make.names(c("1.1", "3.1")))
x_mknm_csv <- c(
  # first empty column with read.csv(x, header = TRUE): "X.1"
  make.names(rep("", 2L), unique = TRUE)[2L],
  "X.2", "X.234") # read.csv(x, header = TRUE): 3rd and 235th unnamed column
x_mknm_csv_q <- checkinput:::paste_quoted(x_mknm_csv)
# c("X1", "X3", "X33"): consisted only of digits or (not identified) created by
# read.csv(x, header = TRUE) with column names '1', '3' and '33';
# c("X0", "X00", "X03", "X013", "X033"): consisted only of digits;
# c("X0cc", "X00cc", "X03cc", "X033cc", "X3cc", "X33cc", "X0.3cc", "X0.33cc"):
# started with a digit; c("X_", "X_.", "X_ab", "X_3b"): started with an underscore;
# c("X.0", "X.00", "X.03"): started with a dot followed by a number;
# c("X.0a", "X.03a"): started with a dot followed by a number;
# c("FALSE.", "for.", "NA.", "TRUE.", "while."): were reserved words
x_mknm_F <- make.names(
  c("1", "3", "33", "0", "00", "03", "013", "033", "0cc", "00cc", "03cc",
    "033cc", "3cc", "33cc", "0.3cc", "0.33cc", "_", "_.", "_ab", "_3b", ".0",
    ".00", ".03", ".0a", ".03a", "FALSE", "for", "NA", "TRUE", "while"))
# "TRUE..1": a reserved word; c("a.1", "a.2"): could also have been the last two
# names of read.csv(x, header = TRUE) with three column names 'a', but are not
# identified as such; c(".X.1", ".X.2", "X..1", "X..2", "XX.1", "XX.2", "a..1",
# "a..2", "A.X.1", "A.X.2", "X.X.1", "X.X.2", "Xa.1", "Xa.2")
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
x_mknm_q <- checkinput:::paste_quoted(x_mknm)
x_mknm_vcsnm <- "X0...11"
x_mknm_vcsnm_q <- checkinput:::paste_quoted(x_mknm_vcsnm)
# From help(vctrs::vec_as_names) and their test suite, created by
# vctrs::vec_as_names(c("0a", "1a", "22c", "2fa", "_", "_0", "_a.", "_a1",
#                       "_foo", "_z", FALSE, "for", "if", "Inf", "NA_real_",
#                       "TRUE", "#$3##"), repair = "universal")
x_vcsnm <- c("..0a", "..1a", "..22c", "..2fa", "._", "._0", "._a.", "._a1",
             "._foo", "._z", ".FALSE", ".for", ".if", ".Inf", ".NA_real_",
             ".TRUE", "..3..")
x_vcsnm_q <- checkinput:::paste_quoted(x_vcsnm)
x_vld <- c("..a..", ".a", ".a.", ".b", ".V1", ".V234", "A", "a...03", "b.", "C",
           "d.", "nco", "V", "V0", "V03", "v1", "V1.", "V1V", "V234.", "V234V",
           "VV1", "VV234", "x", "X.", "X..", "X..X", "X.A", "X.a2", "X.b.",
           "X.X", "X.X.X", "x0")
x_vld_undersc <- c(".a1_", "a.1_", "a1._", "g_hi", "V_", "V0_3", "V1V_",
                   "V2_34V", "VV_1", "VV_234", "X._")
x_vld_undersc_q <- checkinput:::paste_quoted(x_vld_undersc)
x_mix <- c(x_invld, x_vld, x_mknm_csv, x_mknm, x_vcsnm, x_mknm_T_vcsnm, x_mknm_T)

##### Messages and warnings #####
mknm_F <- "make.names(x, unique = FALSE)"
mknm_T <- "make.names(x, unique = TRUE)"
use_mknm <- paste0(".\nUse 'x <- ", mknm_T)
use_mknm_undersc <- paste0(
  ".\nUse 'x <- make.names(x, unique = TRUE, allow_ = FALSE)' to create",
  " unique,\nsyntactically valid names without underscores")
warn_mod <- "might have been modified by "

warn_csv_df <- "might have been created by read.csv() or data.frame(): "
warn_dots <- "consist of only dots (make.names() will not fix that): "
warn_dupl <- "are duplicated: "
warn_mknm <- paste0(warn_mod, mknm_F, " or ", mknm_T, ": ")
warn_mknm_csv <- paste0(
  warn_mod, mknm_F, " or ", mknm_T, ", or have been created by read.csv(): ")
warn_mknm_F <- paste0(warn_mod, mknm_F, ": ")
warn_mknm_T <- paste0(warn_mod, mknm_T, ": ")
warn_mknm_T_vcsnm <- paste0(warn_mod, mknm_T, " or vctrs::vec_as_names(): ")
warn_mknm_vcsnm <- paste0(
  warn_mod, mknm_F, ", ", mknm_T, ", or vctrs::vec_as_names(): ")
warn_syntax <- "are syntactically invalid: "
warn_vcsnm <- paste0(warn_mod, "vctrs::vec_as_names(): ")


#### Test section 'Details' ####
expect_equal(make.names(".1"), "X.1")
expect_equal(make.names(c("a-b", "ab#cd", "c/d")), c("a.b", "ab.cd", "c.d"))
expect_silent(expect_true(all_names(c("a.b", "ab.cd", "c.d"))))
expect_warning(
  expect_false(all_names("X.2")),
  pattern = paste0(warn_mknm_csv, "'X.2'"), strict = TRUE, fixed = TRUE)


#### Test the examples ####
expect_true(all_names(x = c("a", "b.1a")))
expect_warning(
  expect_false(all_names(x = c("a", "b.1a", "a"))),
  pattern = paste0(warn_dupl, "'a'", use_mknm), strict = FALSE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c("a", "ab#cd", "", "for", ".."),
                         allow_suspicious = TRUE)),
  pattern = paste0(warn_syntax, "'ab#cd', 'for', ", x_invld_empty_q, "; and ",
                   warn_dots, "'..'", use_mknm),
  strict = FALSE, fixed = TRUE)
expect_warning(
  expect_false(all_names(x = NULL)),
  pattern = "'x' is NULL", strict = TRUE, fixed = TRUE)

# "might have been created by read.csv() or data.frame(): "
expect_warning(
  expect_false(all_names(x = "V3")),
  pattern = paste0(warn_csv_df, "'V3'"),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "V3", allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = "X3")),
  pattern = paste0(warn_mknm_F, "'X3'"),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "X3", allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = "e.2")),
  pattern = paste0(warn_mknm_T, "'e.2'"),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "e.2", allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = "X.2.1")),
  pattern = paste0(warn_mknm, "'X.2.1'"),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "X.2.1", allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = "X.2")),
  pattern = paste0(warn_mknm_csv, "'X.2'"), strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "X.2", allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = c("..22c", ".TRUE"))),
  pattern = paste0(warn_vcsnm, "'..22c', '.TRUE'"),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = c("..22c", ".TRUE"), allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = "X0...11")),
  pattern = paste0(warn_mknm_vcsnm, "'X0...11'"),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "X0...11", allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = c(".if...4", "a...2"))),
  pattern = paste0(warn_mknm_T_vcsnm, "'.if...4', 'a...2'"),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = c(".if...4", "a...2"), allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = "abc_def", allow_undersc = FALSE)),
  pattern = paste0("Names contain underscores: 'abc_def'", use_mknm_undersc),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = "abc_def", allow_undersc = TRUE))


#### Test some sets ####
expect_true(identical(c(x_vld, x_vld_undersc),
                      make.names(c(x_vld, x_vld_undersc))))
expect_true(all(is.na(x_invld) | (x_invld != make.names(x_invld))))

expect_warning(
  expect_false(all_names(x = x_invld_mn_made)),
  pattern = paste0(warn_mknm_F, checkinput:::paste_quoted(x_invld_mn_made)),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = x_invld_vcsnm_repaired)),
  pattern = paste0(warn_vcsnm,
                   checkinput:::paste_quoted(x_invld_vcsnm_repaired)),
  strict = TRUE, fixed = TRUE)

expect_silent(expect_true(all_names(x_dupl)))


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
# bool_susp_csv
expect_warning(
  expect_false(all_names(x = "X.023")),
  pattern = paste0(warn_mknm_F, "'X.023'"), strict = TRUE, fixed = TRUE)

# Catch using any digit instead of digits one to nine for first digit to create
# bool_susp_csv_dataframe
expect_silent(expect_true(all_names(x = "V023")))

# Catch using dots (i.e., 'any character') instead of literal dots to create
# bool_susp_vecasnames or bool_susp_makenames_unique
expect_silent(expect_true(all_names(x = "abc123")))

# Catch using any digit instead of digits one to nine for first digit to create
# bool_susp_vecasnames or bool_susp_makenames_unique
expect_silent(expect_true(all_names(x = "a...023")))


#### Tests ####
##### Non-character-vector #####
for(x in list(NA, data.frame(a = "nco"), as.matrix(data.frame(a = "nco")),
              list(a = 314), list(), 314)) {
  expect_warning(
    expect_false(all_names(x = x)),
    pattern = "'x' is not a character vector!", strict = TRUE, fixed = TRUE)
}

##### Zero-length values #####
expect_warning(
  expect_false(all_names(x = NULL, allow_suspicious = TRUE)),
  pattern = "'x' is NULL", strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = character(0), allow_suspicious = TRUE)),
  pattern = "x has length zero but is not NULL", strict = TRUE, fixed = TRUE)

##### Duplicated values #####
expect_warning(
  expect_false(all_names(x = c(x_vld, x_vld[c(2, 3)]))),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_vld[c(2, 3)]),
                   use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_vld_undersc, x_vld_undersc[c(2, 3)]),
                         allow_undersc = TRUE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_vld_undersc[c(2, 3)]),
                   use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_invld, x_invld[c(2, 3)]))),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_invld[c(2, 3)]),
                   "; and ", warn_syntax, x_invld_q, use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(c(".", "."))),
  pattern = paste0(warn_dupl, "'.'; and ", warn_dots, "'.'", use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_mknm_csv, x_mknm_csv[2]),
                         allow_suspicious = TRUE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_mknm_csv[2]), use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_mknm, x_mknm[c(2, 3)]),
                         allow_suspicious = TRUE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_mknm[c(2, 3)]),
                   use_mknm), strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x = c(x_vcsnm, x_vcsnm[c(2, 4)]),
                         allow_suspicious = TRUE)),
  pattern = paste0(warn_dupl, checkinput:::paste_quoted(x_vcsnm[c(2, 4)]),
                   use_mknm),
  strict = TRUE, fixed = TRUE)

##### Valid and invalid names #####
expect_silent(
  expect_true(all_names(x = c(x_vld, x_vld_undersc),
                        allow_suspicious = FALSE, allow_undersc = TRUE)))

# Invalid names are not created by make.names(), so should not be suspicious
expect_warning(
  expect_false(all_names(x = c(x_vld, x_vld_undersc, x_invld),
                         allow_suspicious = FALSE, allow_undersc = TRUE)),
  pattern = paste0(warn_syntax, x_invld_q, use_mknm),
  strict = TRUE, fixed = TRUE)

for(allow_suspicious in c(TRUE, FALSE)) {
  expect_warning(
    expect_false(all_names(x = "", allow_suspicious = allow_suspicious)),
    pattern = paste0(warn_syntax, x_invld_empty_q, use_mknm),
    strict = TRUE, fixed = TRUE)
}

##### Only dots or patterned dots #####
for(x in list(".", "..", "...", "....")) {
  expect_warning(
    expect_false(all_names(x = x, allow_suspicious = TRUE)),
    pattern = paste0(warn_dots, "'", x, "'"), strict = TRUE, fixed = TRUE)
}
expect_silent(expect_true(all_names(x = c(".a.", "..a..", ".b", "b."))))

for(x in list("..1", "..314")) {
  expect_warning(
    expect_false(all_names(x = x, allow_suspicious = TRUE)),
    pattern = paste0("Names consist of two dots followed by digits, which is a",
                     " reserved pattern: '", x, "'"),
    strict = TRUE, fixed = TRUE)
}


##### Respect 'allow_undersc' #####
expect_silent(
  expect_true(all_names(x = c(x_vld, x_vld_undersc),
                        allow_suspicious = FALSE, allow_undersc = TRUE)))

expect_warning(
  expect_false(all_names(x = c(x_vld, x_vld_undersc),
                         allow_suspicious = FALSE, allow_undersc = FALSE)),
  pattern = paste0("Names contain underscores: ", x_vld_undersc_q,
                   use_mknm_undersc), strict = TRUE, fixed = TRUE)

##### Suspicious names #####
expect_warning(
  # Also created by read.csv(x, header = TRUE) to name the first unnamed column
  expect_false(all_names(x = make.names(""))), # "X"
  pattern = paste0(warn_mknm_csv, "'X'"),
  strict = TRUE, fixed = TRUE)
expect_silent(expect_true(all_names(x = "X", allow_suspicious = TRUE)))

expect_warning(
  expect_false(all_names(x = x_csv_df)),
  pattern = paste0(warn_csv_df, x_csv_df_q),
  strict = TRUE, fixed = TRUE)
expect_silent(expect_true(all_names(x = x_csv_df, allow_suspicious = TRUE)))
expect_silent(expect_true(all_names(x = c("V", "V0", "V03", "V1V", "V234V",
                                          "VV1", "VV234"))))

expect_warning(
  expect_false(all_names(x = x_mknm_F)),
  pattern = paste0(warn_mknm_F, checkinput:::paste_quoted(x_mknm_F)),
  strict = TRUE, fixed = TRUE)
expect_silent(expect_true(all_names(x = x_mknm_F, allow_suspicious = TRUE)))

expect_warning(
  expect_false(all_names(x = x_mknm_T)),
  pattern = paste0(warn_mknm_T, x_mknm_T_q),
  strict = TRUE, fixed = TRUE)
expect_true(all_names(x = x_mknm_T, allow_suspicious = TRUE))

expect_warning(
  expect_false(all_names(x = x_mknm)),
  pattern = paste0(warn_mknm, x_mknm_q),
  strict = TRUE, fixed = TRUE)
expect_silent(expect_true(all_names(x = x_mknm, allow_suspicious = TRUE)))

expect_warning(
  expect_false(all_names(x = x_mknm_csv)),
  pattern = paste0(warn_mknm_csv, x_mknm_csv_q),
  strict = TRUE, fixed = TRUE)
expect_silent(expect_true(all_names(x = x_mknm_csv, allow_suspicious = TRUE)))

expect_warning(
  expect_false(all_names(x = x_vcsnm)),
  pattern = paste0(warn_vcsnm, x_vcsnm_q),
  strict = TRUE, fixed = TRUE)
expect_silent(expect_true(all_names(x = x_vcsnm, allow_suspicious = TRUE)))

expect_warning(
  expect_false(all_names(x = x_mknm_T_vcsnm)),
  pattern = paste0(warn_mknm_T_vcsnm, x_mknm_T_vcsnm_q),
  strict = TRUE, fixed = TRUE)
expect_silent(expect_true(all_names(x = x_mknm_T_vcsnm,
                                    allow_suspicious = TRUE)))

expect_warning(
  expect_false(all_names(x = x_mknm_vcsnm)),
  pattern = paste0(warn_mknm_vcsnm, x_mknm_vcsnm_q),
  strict = TRUE, fixed = TRUE)
expect_silent(expect_true(all_names(x = x_mknm_vcsnm, allow_suspicious = TRUE)))

##### Mix #####
expect_warning(
  expect_false(all_names(x_mix)),
  pattern = paste0(warn_syntax, x_invld_q,
                   "; and ", warn_mknm_T, x_mknm_T_q,
                   "; and ", warn_mknm, x_mknm_q,
                   "; and ", warn_mknm_csv, x_mknm_csv_q,
                   "; and ", warn_vcsnm, x_vcsnm_q,
                   "; and ", warn_mknm_T_vcsnm, x_mknm_T_vcsnm_q,
                   use_mknm),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(all_names(x_mix, allow_suspicious = TRUE)),
  pattern = paste0(warn_syntax, x_invld_q, use_mknm),
  strict = TRUE, fixed = TRUE)

##### Erroneous input #####
expect_error(all_names(),
             pattern = "argument \"x\" is missing, with no default",
             fixed = TRUE)
expect_error(all_names(x = names(c(a = 1, b = 2)), allow_suspicious = NA),
             pattern = "is_logical(allow_suspicious) is not TRUE", fixed = TRUE)
expect_error(all_names(x = names(c(a = 1, b = 2)), allow_undersc = NA),
             pattern = "is_logical(allow_underscores) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(mknm_F, mknm_T, use_mknm, use_mknm_undersc, warn_csv_df, warn_dots,
   warn_dupl, warn_mknm, warn_mknm_csv, warn_mknm_F, warn_mknm_T,
   warn_mknm_T_vcsnm, warn_mknm_vcsnm, warn_mod, warn_syntax, warn_vcsnm, x,
   x_csv_df, x_csv_df_q, x_invld, x_invld_caught, x_invld_caughtnot,
   x_invld_empty_q, x_invld_mn_made, x_invld_q, x_invld_vcsnm_repaired, x_mknm,
   x_mknm_csv, x_mknm_csv_q, x_mknm_F, x_mknm_q, x_mknm_T, x_mknm_T_q,
   x_mknm_T_vcsnm, x_mknm_T_vcsnm_q, x_mknm_vcsnm, x_mknm_vcsnm_q, x_mix,
   x_vcsnm, x_vcsnm_q, x_vld, x_vld_undersc, x_vld_undersc_q)
