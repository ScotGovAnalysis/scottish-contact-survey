
test_that("Error if date_updated not in date format", {
  expect_error(reg_data_updates(dummy_reg, dummy_resp, "01-01-2022"))
})

test_that("Returned value has same format and names as `reg_data`", {

  updated <- reg_data_updates(dummy_reg, dummy_resp, as.Date("2022-01-01"))

  expect_true(is.data.frame(updated))
  expect_equal(dim(dummy_reg), dim(updated))
  expect_named(updated, names(dummy_reg))

})