# start_date

test_that("Error if panel missing or invalid for wave 43 or earlier", {
  expect_error(start_date(4))
  expect_error(start_date(43))
  expect_error(start_date(30, "C"))
  expect_error(start_date(wave = 30, panel = 4))
  expect_error(start_date(wave = c(30:31), panel = c("A", "D")))
})

test_that("Error if wave missing or invalid", {
  expect_error(start_date(panel = "A"))
  expect_error(start_date(0, "A"))
  expect_error(start_date(-4, "A"))
  expect_error(start_date(4.5, "A"))
  expect_error(start_date("four", "A"))
  expect_error(start_date(wave = c(10.5, 15), panel = c("A", "B")))
})

test_that("Warning if panel supplied for wave 44 or later", {
  expect_warning(start_date(44, "A"))
  expect_warning(start_date(50, "B"))
  expect_warning(start_date(50, "C"))
  expect_warning(start_date(40:45, rep("A", 6)))
})

test_that("Error if incompatible argument lengths supplied.", {
  expect_error(start_date(1:5, c("A", "B")))
  expect_error(start_date(c(2, 43), c("A", "B", "B")))
})

test_that("Provides correct answer", {
  expect_equal(start_date(1, "A"), lubridate::dmy(06082020))
  expect_equal(start_date(42, "B"), lubridate::dmy(10032022))
  expect_equal(start_date(44), lubridate::dmy(31032022))
  expect_equal(start_date(30, c("A", "B")),
               c(lubridate::dmy(16092021), lubridate::dmy(23092021)))

  tibble::tibble(wave = c(5, 40, 42), panel = c("A", "A", "B")) %>%
    dplyr::mutate(date = start_date(wave, panel)) %>%
    dplyr::pull(date) %>%
    expect_equal(c(lubridate::dmy(01102020),
                   lubridate::dmy(03022022),
                   lubridate::dmy(10032022)))

})


# date_range

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
  expect_equal(date_range(44:46),
               c("31st March - 6th April",
                 "14th April - 20th April",
                 "28th April - 4th May"))
})
