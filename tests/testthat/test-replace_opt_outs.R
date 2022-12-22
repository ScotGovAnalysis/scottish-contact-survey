
reg_replaced <- replace_opt_outs(dummy_reg, dummy_opt_outs, 50)

test_that("Error if reg_data not in correct format", {
  expect_error(replace_opt_outs("dummy_reg", dummy_opt_outs, 50))
  expect_error(replace_opt_outs(dplyr::select(dummy_reg, -cp_number),
                                dummy_opt_outs,
                                50))
})

test_that("Error if opt_out_data not in correct format", {
  expect_error(replace_opt_outs(dummy_reg, "dummy_opt_outs", 50))
  expect_error(replace_opt_outs(dummy_reg,
                                dplyr::select(dummy_opt_outs, -age_group),
                                50))
})

test_that("Error/warning if incorrect panel value supplied", {
  expect_error(replace_opt_outs(dummy_reg, dummy_opt_outs, 40))
  expect_error(replace_opt_outs(dummy_reg, dummy_opt_outs, 40, "invalid_panel"))
  expect_warning(suppressMessages(
    replace_opt_outs(dummy_reg, dummy_opt_outs, 50, "A")
  ))
})

test_that("Participants added from reserve list", {
  expect_gt(
    nrow(dplyr::filter(reg_replaced, status == "active")),
    nrow(dplyr::filter(dummy_reg, status == "active"))
  )
})
