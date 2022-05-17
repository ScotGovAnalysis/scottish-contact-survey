test_that("Provides correct answer", {
  expect_equal(date_range(46, suffix = FALSE), "28 April - 04 May")
  expect_equal(date_range(46, suffix = TRUE), "28th April - 4th May")
  expect_equal(date_range(30, "A"), "16th September - 22nd September")
})