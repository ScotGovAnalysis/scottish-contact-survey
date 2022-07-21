test_that("Error if no panel supplied for wave 43 or earlier", {
  expect_error(age(as.Date("1980-03-30"), 2))
  expect_error(age(as.Date("1980-03-30"), 43))
})

test_that("Error if panel not A or B for wave 43 or earlier", {
  expect_error(age(as.Date("1980-03-30"), 4, "C"))
})

test_that("Error if wave number incorrect format", {
  expect_error(age(as.Date("1980-03-30"), -4, "A"))
  expect_error(age(as.Date("1980-03-30"), 4.5, "A"))
  expect_error(age(as.Date("1980-03-30"), "four", "A"))
})

test_that("Warning if panel supplied for wave 44 or later", {
  expect_warning(age(as.Date("1980-03-30"), 44, "A"))
  expect_warning(age(as.Date("1980-03-30"), 50, "B"))
  expect_warning(age(as.Date("1980-03-30"), 50, "C"))
})

test_that("Provides correct answer", {
  expect_equal(age(as.Date("1980-03-30"), 1, "A"), 40)
  expect_equal(age(as.Date("1980-03-30"), 42, "B"), 41)
  expect_equal(age(as.Date("1980-03-30"), 44), 42)
  expect_equal(age(as.Date("1980-03-30"), 44, grouped = TRUE), "40-49")
})