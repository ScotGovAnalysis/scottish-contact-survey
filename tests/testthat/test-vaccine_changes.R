
test_that("Error if required variables don't exist in raw_vaccine_data", {
  expect_error(vaccine_changes(dummy_reg,
                               dummy_resp %>% dplyr::select(-vacc_1)))
  expect_error(vaccine_changes(dummy_reg,
                               dummy_resp %>% dplyr::select(-cp_number,
                                                            -vacc_1,
                                                            -vacc_2)))
})

test_that("Returned value is a tibble", {
  expect_true(tibble::is_tibble(vaccine_changes(dummy_reg, dummy_resp)))
})

test_that("Variable names in returned value are same as reg_data", {
  expect_equal(names(dummy_reg), names(vaccine_changes(dummy_reg, dummy_resp)))
})

test_that("Correct value returned", {

  test_vacc_data <- tibble::tibble(
    cp_number = dummy_reg$cp_number[1:4],
    vacc_1 = c("No",
               NA_character_,
               NA_character_,
               "Yes, I have received four doses"),
    vacc_2 = c(NA_character_,
               "Yes, this is still correct",
               "I have now had two doses",
               NA_character_)
  )

  expect_equal(
    vaccine_changes(dummy_reg, test_vacc_data)$vaccine_n_doses[1:4],
    c("no doses", "dummy_value", "two doses", "four doses")
  )

})
