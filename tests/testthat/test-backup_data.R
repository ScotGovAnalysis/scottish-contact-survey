
# Create filepaths for test files
folder <- tempdir(check = TRUE)
zip1 <- tempfile(tmpdir = folder, fileext = ".zip")
zip2 <- tempfile(tmpdir = folder, fileext = ".zip")
file1 <- tempfile(tmpdir = folder)
file2 <- tempfile(tmpdir = folder)

test_that("Error if file_to_backup doesn't exist", {
  expect_error(backup_data(zip1, file1))
})

# Create temp files to backup
file.create(file1, file2)

test_that("Error if zip_file does not have .zip file extention", {
  expect_error(backup_data(tempfile(), file1))
})

test_that("Zip file created containing file", {
  backup_data(zip1, file1)
  expect_true(file.exists(zip1))
  expect_true(basename(file1) == zip::zip_list(zip1)$filename)
})

test_that("Able to add file to existing zip file", {
  backup_data(zip2, file1)
  backup_data(zip2, file2)
  expect_true(all(basename(c(file1, file2)) == zip::zip_list(zip2)$filename))
})

unlink(folder, recursive = TRUE)
