
# remove_opt_outs

cp_active <- dummy_reg %>% dplyr::filter(status == "active")
cp_to_opt_out <- cp_active$cp_number[1]

reg_opt_outs <- remove_opt_outs(dummy_reg, cp_to_opt_out)

test_that("Error if reg_data not in correct format", {
  expect_error(remove_opt_outs("reg_data", cp_to_opt_out))
  expect_error(remove_opt_outs(dplyr::select(dummy_reg, -cp_number),
                               cp_to_opt_out))
})

test_that("Warning if any CP numbers to opt out not in reg_data", {
  expect_warning(remove_opt_outs(dummy_reg, "invalid_cp"))
  expect_warning(remove_opt_outs(dummy_reg, c(cp_to_opt_out, "invalid_cp")))
})

test_that("dummy_reg returned when no CP numbers to opt out", {
  expect_true(
    all.equal(
      suppressWarnings(remove_opt_outs(dummy_reg, "invalid_cp")),
      dummy_reg)
  )
})

test_that("Correct value returned", {

  expect_equal(nrow(dummy_reg), nrow(reg_opt_outs))

  expect_true(
    all.equal(
      dplyr::filter(reg_opt_outs, cp_number != cp_to_opt_out),
      dplyr::filter(dummy_reg, cp_number != cp_to_opt_out)
    )
  )

  expect_equal(
    dplyr::filter(reg_opt_outs, cp_number == cp_to_opt_out) %>%
      dplyr::pull(status),
    "opt-out"
  )

  expect_true(
    all(is.na(
      dplyr::filter(reg_opt_outs, cp_number == cp_to_opt_out) %>%
        dplyr::select(!c(cp_number, status, panel))
    ))
  )

})


# replace_opt_outs

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
