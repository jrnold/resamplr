context("spelling")

expect_no_misspellings <- function(...) {
  misspellings <- devtools::spell_check(...)
  cond <- length(misspellings) == 0L
  msg <- tryCatch(capture_output(print(misspellings)),
          warning = function(w) NULL)
  expect(cond, paste("Mispellings:\n", msg, sep = "\n"))
}

test_that("No mispellings", {
  #expect_no_misspellings(ignore = lazysample:::.SPELLING$ignore,
  #                       dict = lazysample:::.SPELLING$dict)
})
