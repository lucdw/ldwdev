test_that("compare_files works", {
  filenames <- tempfile(fileext = c(".txt", ".txt", ".diff"))
  writeLines(c("a", "b", "c", "d", "e", "f", "g", "h"), filenames[1])
  writeLines(c("0", "1", "a", "b", "e", "f", "grrr", "h"), filenames[2])
  compare_files(filenames[1], filenames[2], filenames[3])
  expect_equal(readLines(filenames[3]),
               c("0a1,2", "> 0", "> 1", "3,4d", "< c", "< d", "7c7", "< g", "---", "> grrr"))
})
