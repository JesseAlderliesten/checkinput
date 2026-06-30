#### Create objects to use in tests ####
Windows_reserved <- c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9),
                      paste0("LPT", 1:9))
Windows_reserved <- c(Windows_reserved, tolower(Windows_reserved))
illegal_chars <- c('"', "*", "?", "|", "<", ">")
warn_Windows_reserved <- "should not contain Windows-reserved names"
warn_space_dot <- "should not end with ' ' or '.'"


#### Test the examples ####
expect_true(is_path(getwd()))
expect_true(is_path(fs::path_wd("abcd")))
expect_warning(
  expect_false(is_path(fs::path_wd("ab|cd"))),
  pattern = "should not contain '\"', '*'", fixed = TRUE)

expect_true(is_path(fs::path_wd("abcd.txt")))
expect_true(is_path(fs::path_wd("abcd.txt.gz")))
expect_true(is_path(fs::path_wd("abcd.gz")))

expect_silent(expect_true(is_path("D:/")))
expect_warning(
  expect_false(is_path(fs::path_wd("ab:cd"))),
  pattern = "should not contain ':'", fixed = TRUE)
expect_warning(
  expect_false(is_path(fs::path_wd("ab:cd.txt"))),
  pattern = "should not contain ':'", fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path_wd("ab|cd"))),
  pattern = "should not contain '\"', '*'", fixed = TRUE)
expect_warning(
  expect_false(is_path(fs::path_wd("ab|cd.txt"))),
  pattern = "should not contain '\"', '*'", fixed = TRUE)


#### Tests ####
##### require_sep #####
expect_warning(
  expect_false(is_path("abcd.txt", require_sep = TRUE)),
  pattern = "should contain file separators", strict = TRUE, fixed = TRUE)
expect_warning(
  expect_false(is_path(".txt", require_sep = TRUE)),
  pattern = "should contain file separators", strict = TRUE, fixed = TRUE)
expect_warning(
  expect_false(is_path(".gz", require_sep = TRUE)),
  pattern = "should contain file separators", strict = TRUE, fixed = TRUE)
expect_warning(
  expect_false(is_path("abcd", require_sep = TRUE)),
  pattern = "should contain file separators", strict = TRUE, fixed = TRUE)
expect_warning(
  expect_false(is_path("abcd.gz", require_sep = TRUE)),
  pattern = "should contain file separators", strict = TRUE, fixed = TRUE)
expect_warning(
  expect_false(is_path("abc.tx#", require_sep = TRUE)),
  pattern = "should contain file separators", strict = TRUE, fixed = TRUE)

expect_silent(expect_true(is_path("abcd.txt", require_sep = FALSE)))
expect_silent(expect_true(is_path(".txt", require_sep = FALSE)))
expect_silent(expect_true(is_path(".gz", require_sep = FALSE)))
expect_silent(expect_true(is_path("abcd", require_sep = FALSE)))
expect_silent(expect_true(is_path("abcd.gz", require_sep = FALSE)))
expect_silent(expect_true(is_path("abc.tx#", require_sep = FALSE)))

##### Illegal characters #####
for(illegal_char in illegal_chars) {
  expect_warning(
    expect_false(is_path(paste0("ab", illegal_char, "cd"), require_sep = FALSE)),
    pattern = "should not contain '\"', '*'", fixed = TRUE)
  expect_warning(
    expect_false(is_path(paste0("ab", illegal_char, "cd.txt"), require_sep = FALSE)),
    pattern = "should not contain '\"', '*'", fixed = TRUE)
}

expect_silent(expect_true(is_path("ab:cd/mno/file.txt", require_sep = FALSE)))

expect_warning(
  expect_false(is_path("abcd/ef:gh/file.txt", require_sep = FALSE)),
  pattern = "should not contain ':'", fixed = TRUE)

expect_warning(
  expect_false(is_path("abcd/efgh/fi:le.txt", require_sep = FALSE)),
  pattern = "should not contain ':'", fixed = TRUE)

expect_silent(expect_true(is_path("ab:cd/efgh", require_sep = FALSE)))

expect_warning(
  expect_false(is_path("abcd/ef:gh", require_sep = FALSE)),
  pattern = "should not contain ':'", fixed = TRUE)

expect_silent(expect_true(is_path("ab:cd/file.txt", require_sep = FALSE)))

expect_warning(
  expect_false(is_path("abcd/fi:le.txt", require_sep = FALSE)),
  pattern = "should not contain ':'", fixed = TRUE)

expect_silent(expect_true(is_path("ab:cd", require_sep = FALSE)))

expect_warning(
  expect_false(is_path("fi:le.txt", require_sep = FALSE)),
  pattern = "should not contain ':'", fixed = TRUE)

# fs::path_ext_remove(filename) normalized "C:" to "C:/" leading to the
# erroneous warning that the filename should not contain ':'.
expect_silent(expect_true(is_path("C:", require_sep = FALSE)))

for(control_char in paste0("\005", "\025", "\035", "\177")) {
  expect_warning(
    expect_false(is_path(paste0("ab", control_char, "cd"), require_sep = FALSE)),
    pattern = "should not contain control characters", fixed = TRUE)

  expect_warning(
    expect_false(is_path(paste0("ab", control_char, "cd.txt"), require_sep = FALSE)),
    pattern = "should not contain control characters", fixed = TRUE)
}

##### Windows reserved names #####
# These are not allowed as path components but are allowed as filename
for(Windows_name in Windows_reserved) {
  expect_warning(
    expect_false(is_path(Windows_name, require_sep = FALSE)),
    pattern = warn_Windows_reserved, fixed = TRUE)

  expect_warning(
    expect_false(is_path(fs::path_wd("subdir", Windows_name, "filename.txt"))),
    pattern = warn_Windows_reserved, fixed = TRUE)

  expect_true(
    is_path(fs::path_wd("subdir", paste0(Windows_name, ".txt"), "filename.txt")))

  expect_true(
    is_path(fs::path_wd("subdir", paste0(Windows_name, ".txt"))))
}

# 'COM', 'COM0', 'LPT' and 'LPT0' are allowed as filename and as path component
for(Windows_allowed in c("COM", "COM0", "LPT", "LPT0")) {
  expect_true(
    is_path(Windows_allowed, require_sep = FALSE))

  expect_true(
    is_path(fs::path_wd("subdir", Windows_allowed, "filename.txt")))

  expect_true(
    is_path(fs::path_wd("subdir", paste0(Windows_allowed, ".txt"),
                        "filename.txt")))

  expect_true(
    is_path(fs::path_wd("subdir", paste0(Windows_allowed, ".txt"))))
}

##### Spaces and dots #####
# Path components should not end with a space
expect_true(is_path(fs::path("a b", "def"), require_sep = FALSE))

expect_true(is_path(fs::path("a  b", "def"), require_sep = FALSE))

expect_warning(
  expect_false(is_path(fs::path("ab", " ", "def"), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab", "  ", "def"), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab ", "def"), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab  ", "def"), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab", "def "), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab", "def "), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

# "." and ".." are only allowed as first path component
expect_true(is_path(fs::path(".", "a.b", "def"), require_sep = FALSE))

expect_true(is_path(fs::path("..", "a..b", "def"), require_sep = FALSE))

expect_warning(
  expect_false(is_path(fs::path("ab", "..", "def"), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab", ".", "def"), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

# Path components should not end with a dot
expect_warning(
  expect_false(is_path("ab.", require_sep = FALSE)),
  pattern = "should not end with ' ' or '.'", fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab.", "def"), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab..", "def"), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab", "def."), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path("ab", "def.."), require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

# Filenames should not end with a space or a dot
expect_warning(
  expect_false(is_path("..txt", require_sep = FALSE)),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path_wd("subdir", "filename "))),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path_wd("subdir", "filename .txt"))),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path_wd("subdir", "filename."))),
  pattern = warn_space_dot, fixed = TRUE)

expect_warning(
  expect_false(is_path(fs::path_wd("subdir", "filename..txt"))),
  pattern = warn_space_dot, fixed = TRUE)

# Filenames should not start with a space or a hyphen
expect_silent(expect_true(is_path(fs::path_wd("subdir", " filename"))))

expect_warning(
  expect_false(is_path(fs::path_wd("subdir", " filename.txt"))),
  pattern = "should not start with ' ' (i.e., a space) or '-'", fixed = TRUE)

expect_silent(expect_true(is_path(fs::path_wd("subdir", "-filename"))))

expect_warning(
  expect_false(is_path(fs::path_wd("subdir", "-filename.txt"))),
  pattern = "should not start with ' ' (i.e., a space) or '-'", fixed = TRUE)

##### Temporary directory #####
expect_silent(expect_true(is_path(tempdir())))
expect_true(is_path(fs::path(tempdir(), "subdir")))


##### Successive file separators #####
# Need file.path() because fs::path_wd() removes successive file separators
expect_silent(
  expect_true(is_path(file.path("subdir", "/filename.txt"), require_sep = FALSE)))

expect_silent(
  expect_true(is_path(file.path("subdir", "\\filename.txt"), require_sep = FALSE)))

expect_silent(
  expect_true(is_path(file.path("subdir", "\\\\filename.txt"), require_sep = FALSE)))

##### Trailing file separators #####
# To prevent warning about successive file separators on MacOS and Ubuntu (where
# the paths will end in a slash)
path_in <- fs::path_wd("subdir", "filename.txt")
if(endsWith(path_in, suffix = "/")) {
  expect_warning(
    expect_false(is_path(path_in, require_sep = FALSE)),
    pattern = "should not end with a slash or backslash",
    strict = TRUE, fixed = TRUE)
} else {
  expect_warning(
    expect_false(is_path(paste0(path_in, "/"), require_sep = FALSE)),
    pattern = "should not end with a slash or backslash",
    strict = TRUE, fixed = TRUE)
}

expect_warning(
  expect_false(is_path(paste0(fs::path_wd("subdir", "filename.txt"), "/"))),
  pattern = "should not end with a slash or backslash", strict = TRUE, fixed = TRUE)

expect_warning(
  expect_false(is_path(paste0(fs::path_wd("subdir", "filename.txt"), "\\"))),
  pattern = "should not end with a slash or backslash", strict = TRUE, fixed = TRUE)

##### Non-character input #####
expect_warning(
  expect_false(is_path(3, require_sep = FALSE)),
  pattern = "should be a non-empty, non-NA_character_ character string",
  fixed = TRUE)

expect_warning(
  expect_false(is_path(c("abc.txt", "def.html"), require_sep = FALSE)),
  pattern = "should be a non-empty, non-NA_character_ character string",
  fixed = TRUE)

expect_warning(
  expect_false(is_path(NA_character_, require_sep = FALSE)),
  pattern = "should be a non-empty, non-NA_character_ character string",
  fixed = TRUE)


#### Cleaning up ####
rm(control_char, illegal_char, illegal_chars, warn_space_dot,
   warn_Windows_reserved, Windows_allowed, Windows_name, Windows_reserved)
