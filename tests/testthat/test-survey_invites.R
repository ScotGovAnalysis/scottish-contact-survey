
test_that("Error if reg_data isn't a data frame", {
  expect_error(survey_invites("registration"))
})

test_that("Error if required variables aren't in reg_data", {
  expect_error(survey_invites(subset(dummy_reg, select = -status)))
})

test_that("Invalid panel supplied", {
  expect_error(survey_invites(dummy_reg, "invalid_panel"))
})

test_that("Returned value is tibble", {
  expect_true(tibble::is_tibble(survey_invites(dummy_reg)))
})

test_that("Correct variables returned", {
  expect_true(all.equal(
    c("email", "employment", "studying", "vaccine_n_doses", "to_update",
      paste0("hm", 1:10, "_name"), "household_members"),
    names(survey_invites(dummy_reg))
  ))
})

test_that("Correct number of participants returned", {

  active   <- dplyr::filter(dummy_reg, status == "active")
  active_a <- dplyr::filter(dummy_reg, panel == "A" & status == "active")

  expect_equal(nrow(survey_invites(dummy_reg)), nrow(active))
  expect_equal(nrow(survey_invites(dummy_reg, "A")), nrow(active_a))

})
