# age

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

test_that("Error if date_of_birth isn't date format", {
  expect_error(age("1980-03-30", 50))
  expect_error(age(30031980, 50))
})

test_that("Error if grouped isn't logical", {
  expect_error(age(as.Date("1980-03-30"), 50, grouped = "TRUE"))
  expect_error(age(as.Date("1980-03-30"), 50, grouped = 1))
})

test_that("Provides correct answer", {
  expect_equal(age(as.Date("1980-03-30"), 1, "A"), 40)
  expect_equal(age(as.Date("1980-03-30"), 42, "B"), 41)
  expect_equal(age(as.Date("1980-03-30"), 44), 42)
  expect_equal(age(as.Date("1980-03-30"), 44, grouped = TRUE), "40-49")
})


# age_group

test_that("Error if age isn't correct format", {
  expect_error(age_group("eighty"))
  expect_error(age_group(-50))
  expect_error(age_group(50.5))
  expect_error(age_group(c(25, 49.4)))
})

test_that("Warning if age 18 or less", {
  expect_warning(age_group(15))
  expect_warning(age_group(c(15, 30)))
})

test_that("Returns correct answer", {
  expect_equal(age_group(40), "40-49")
  expect_equal(age_group(62), "60-69")
  expect_equal(age_group(89), "70+")
  expect_equal(suppressWarnings(age_group(15)), NA_character_)
  expect_equal(age_group(c(45, 71)), c("40-49", "70+"))
})
