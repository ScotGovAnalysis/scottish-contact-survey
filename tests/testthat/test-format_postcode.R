test_that("Check NA returned when invalid format supplied", {
  expect_equal(format_postcode("1234"), NA_character_)
})

test_that("Check return value is correct", {
  expect_equal(format_postcode("EH11AA"), "EH1 1AA")
  expect_equal(format_postcode("G11AA"), "G1  1AA")
  expect_equal(format_postcode("EH111AA"), "EH111AA")
})
