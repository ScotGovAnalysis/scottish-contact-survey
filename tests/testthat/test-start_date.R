test_that("Error if vector supplied for either argument", {
  expect_error(start_date(45:46))
  expect_error(start_date(1:2, "A"))
  expect_error(start_date(30, c("A", "B")))
  expect_error(start_date(40:41, c("A", "A")))
})

test_that("Error if no panel supplied for wave 43 or earlier", {
  expect_error(start_date(4))
  expect_error(start_date(43))
})

test_that("Error if panel not A or B for wave 43 or earlier", {
  expect_error(start_date(4, "C"))
})

test_that("Error if wave number incorrect format", {
  expect_error(start_date(0, "A"))
  expect_error(start_date(-4, "A"))
  expect_error(start_date(4.5, "A"))
  expect_error(start_date("four", "A"))
})

test_that("Warning if panel supplied for wave 44 or later", {
  expect_warning(start_date(44, "A"))
  expect_warning(start_date(50, "B"))
  expect_warning(start_date(50, "C"))
})

test_that("Provides correct answer", {
  expect_equal(start_date(1, "A"), lubridate::dmy(06082020))
  expect_equal(start_date(42, "B"), lubridate::dmy(10032022))
  expect_equal(start_date(44), lubridate::dmy(31032022))
})