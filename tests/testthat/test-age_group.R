test_that("Error if age isn't correct format", {
  expect_error(age_group("eighty"))
})

test_that("Warning if age 18 or less", {
  expect_warning(age_group(15))
})

test_that("Returns correct answer", {
  expect_equal(age_group(40), "40-49")
  expect_equal(age_group(62), "60-69")
  expect_equal(age_group(89), "70+")
  expect_equal(suppressWarnings(age_group(15)), NA_character_)
})