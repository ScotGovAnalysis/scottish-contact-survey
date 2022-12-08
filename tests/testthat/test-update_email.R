
test_that("Error if reg_data doesn't have email variable", {
  expect_error(update_email("CPA0001@email.com",
                            "test@email.com",
                            subset(dummy_reg, select = -email)))
})

test_that("Error if old or new email more than length 1.", {
  expect_error(update_email(c("CPA0001@email.com", "CPA0001@email.com"),
                            "test@email.com",
                            subset(dummy_reg, select = -email)))
})

test_that("Warning if old_email doesn't exist or is the same as new_email", {
  expect_warning(update_email("test@email.com",
                              "CPA0001@email.com",
                              dummy_reg))
  expect_warning(update_email("CPA0001@email.com",
                              "CPA0001@email.com",
                              dummy_reg))
})

test_that("Correct value returned", {

  x1 <- update_email("CPA0001@email.com", "test@email.com", dummy_reg)
  x2 <- suppressWarnings(
    update_email("CPA0001@email.com", "CPA0001@email.com", dummy_reg)
  )

  expect_equal(which(dummy_reg$email == "CPA0001@email.com"),
               which(x1$email == "test@email.com"))

  expect_false("CPA0001@email.com" %in% x1$email)

  expect_equal(x2, dummy_reg)

})