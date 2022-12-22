
test_that("Error if survey in wrong format", {
  expect_error(scs_filepath(filename = "file1",
                            survey = "invalid",
                            create_folder = FALSE))
  expect_error(scs_filepath(filename = "file1",
                            survey = "500",
                            create_folder = FALSE))
  expect_error(scs_filepath(filename = "file1",
                            survey = "5C",
                            create_folder = FALSE))
})

test_that("Error if data_folder doesn't exist", {
  expect_error(scs_filepath(filename = "file1",
                            survey = "50",
                            data_folder = "/fake/folder/",
                            create_folder = FALSE))
})

test_that("Error if create_folder isn't logical", {
  expect_error(scs_filepath(filename = "file1",
                            survey = "50",
                            create_folder = "TRUE"))
})

test_that("Error if registration isn't logical", {
  expect_error(scs_filepath(filename = "file1",
                            survey = "50",
                            registration = "TRUE"))
})

dir1 <- paste0(tempdir(check = TRUE), "\\dir1\\")
dir.create(dir1)

test_that("Warning if create_folder = FALSE and folder doesn't exist", {

  expect_warning(scs_filepath(filename = "file1",
                              survey = "50",
                              data_folder = dir1,
                              create_folder = FALSE))

})

test_that("Function returns correct value", {

  x1 <- suppressWarnings(scs_filepath(filename = "file1",
                                      survey = "50",
                                      data_folder = dir1,
                                      create_folder = FALSE))

  expect_equal(x1, paste0(dir1, "/50/50_file1.rds"))
  expect_false(dir.exists(paste0(dir1, "/50/")))

  x2 <- scs_filepath(filename = "file2",
                     survey = "55",
                     fileext = "csv",
                     data_folder = dir1,
                     create_folder = TRUE)

  expect_equal(x2, paste0(dir1, "/55/55_file2.csv"))
  expect_true(dir.exists(paste0(dir1, "/55/")))

  x3 <- scs_filepath(filename = paste0("file", 1:5),
                     survey = "6A",
                     data_folder = dir1)

  expect_equal(x3, paste0(dir1, "/6A/6A_file", 1:5, ".rds"))

  x4 <- scs_filepath(filename = "file6",
                     survey = "10",
                     data_folder = dir1,
                     registration = TRUE)

  expect_equal(x4, paste0(dir1, "/registration-data/10_file6.rds"))
  expect_true(dir.exists(paste0(dir1, "/registration-data/")))

})


dir2 <- paste0(tempdir(check = TRUE), "\\dir2\\")
dir.create(dir2)

dir3 <- paste0(tempdir(check = TRUE), "\\dir3\\")
dir.create(dir3)


test_that("Only first value used for certain arguments", {

  x5 <- suppressWarnings(scs_filepath(filename = c("test1", "test2"),
                                      survey = c("20A", "20B"),
                                      fileext = c(".csv", ".xlsx"),
                                      data_folder = c(dir2, dir3),
                                      registration = c(TRUE, FALSE),
                                      create_folder = c(FALSE, TRUE)))

  expect_equal(
    x5,
    paste0(dir2, "/registration-data/20A_", c("test1", "test2"), ".csv")
  )
  expect_false(dir.exists(paste0(dir2, "/registration-data/")))

})

# Delete temp directory
unlink(c(dir1, dir2, dir3), recursive = TRUE)

