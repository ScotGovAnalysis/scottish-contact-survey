test_that("Error if CP Number format incorrect", {
  expect_error(generate_cp_number("CPAA0", "A", 5))
  expect_error(generate_cp_number(c("CPA0001", "CP"), "A", 5))
})

test_that("Error if panel or n format incorrect", {
  expect_error(generate_cp_number("CPA0001", "AB", 5))
  expect_error(generate_cp_number("CPA0001", "A", "5"))
})

test_that("Returns correct answer", {
  expect_equal(generate_cp_number("CPA0001", "A", 2),
               c("CPA0002", "CPA0003"))
  expect_equal(generate_cp_number(c("CPA0045", "CPB0431"), "J", 1),
               "CPJ0001")
  expect_equal(generate_cp_number(c("CPK1234", "CPD0234"), "K", 2),
               c("CPK1235", "CPK1236"))
})