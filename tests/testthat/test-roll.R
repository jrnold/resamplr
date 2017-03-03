context("roll")

{
  dat <- tibble::tibble(a = 1:5, b = c(1, 1, 2, 2, 3))

  test_that("resample_window align=left works as expected", {
    expect_equal(resample_window(dat, 1, 3), resample(dat, 1:3))
    expect_equal(resample_window(dat, 3, 3), resample(dat, 3:5))
    expect_equal(resample_window(dat, 1, 3, align = "left"),
                 resample(dat, 1:3))
  })

  test_that("resample_window works with width 1", {
    expect_equal(resample_window(dat, 2, 1), resample(dat, 2L))
  })

  test_that("resample_window align=center works as expected", {
    expect_equal(resample_window(dat, 3, 3, align = "center"),
                 resample(dat, 2:4))
    expect_equal(resample_window(dat, 1, 3, align = "center", partial = TRUE),
                 resample(dat, 1:2))
    expect_equal(resample_window(dat, 1, 3, align = "center", partial = FALSE),
                 NULL)
  })

  test_that("resample_window align=right works as expected", {
    expect_equal(resample_window(dat, 3, 3, align = "right"),
                 resample(dat, 1:3))
    expect_equal(resample_window(dat, 1, 3, align = "right", partial = TRUE),
                 resample(dat, 1L))
    expect_equal(resample_window(dat, 1, 3, align = "right", partial = FALSE),
                 NULL)
  })

  test_that("roll align=left works as expected", {
    expect_identical(roll(dat, 3, by = 1L, align = "left", partial = TRUE),
                 tibble(window =
                          resample_list(dat, list(1:3, 2:4, 3:5, 4:5, 5)),
                       .id = 1:5,
                       .width = 3L,
                       .align = "L"))
    expect_identical(roll(dat, 3, by = 1L, align = "left", partial = FALSE),
                     tibble(window =
                              resample_list(dat, list(1:3, 2:4, 3:5)),
                            .id = 1:3,
                            .width = 3L,
                            .align = "L"))
  })

  test_that("roll align=center works as expected", {
    expect_identical(roll(dat, 3, by = 1L, align = "center", partial = TRUE),
                     tibble(window =
                              resample_list(dat, list(1:2, 1:3, 2:4, 3:5, 4:5)),
                            .id = 1:5,
                            .width = 3L,
                            .align = "C"))
    expect_identical(roll(dat, 3, by = 1L, align = "center", partial = FALSE),
                     tibble(window =
                              resample_list(dat, list(1:3, 2:4, 3:5)),
                            .id = 2:4,
                            .width = 3L,
                            .align = "C"))
  })

  test_that("roll align=right works as expected", {
    expect_identical(roll(dat, 3, by = 1L, align = "right", partial = TRUE),
                     tibble(window =
                              resample_list(dat, list(1, 1:2, 1:3, 2:4, 3:5)),
                            .id = 1:5,
                            .width = 3L,
                            .align = "R"))
    expect_identical(roll(dat, 3, by = 1L, align = "right", partial = FALSE),
                     tibble(window = resample_list(dat, list(1:3, 2:4, 3:5)),
                            .id = 3:5,
                            .width = 3L,
                            .align = "R"))
  })

  test_that("roll with by works expected", {
    expect_identical(roll(dat, 3, by = 3L, align = "center", partial = TRUE),
                     tibble(window = resample_list(dat, list(1:2, 3:5)),
                            .id = as.integer(c(1, 4)),
                            .width = 3L,
                            .align = "C"))
  })

}
