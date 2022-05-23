test_that("Error if incorrect argument type", {
  expect_error(date_range(46, suffix = "true"))
  expect_error(date_range(46, year = 2022))
})

test_that("Provides correct answer", {
  expect_equal(date_range(46, suffix = FALSE), "28 April - 04 May")
  expect_equal(date_range(46, suffix = TRUE), "28th April - 4th May")
  expect_equal(date_range(30, "A"), "16th September - 22nd September")
  expect_equal(date_range(46, year = TRUE), "28th April - 4th May 2022")
  expect_equal(date_range(37, "B", year = TRUE),
               "30th December 2021 - 5th January 2022")
})
