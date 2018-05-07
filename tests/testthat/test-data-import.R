## test-data-import.R

context("Data import")

test_that("an 'excelFile' object can be created", {
  tstFile <- "test-dir/test-file-2.xlsx"
  xlObj <- excelFile(tstFile)

  expect_is(xlObj, "excelFile")
  expect_s3_class(xlObj, "excelFile")
  expect_equal(xlObj$filename, basename(tstFile))
  expect_type(xlObj$file.size, "double")
  expect_type(xlObj$created, "double")
  expect_type(xlObj$modified, "double")
  expect_type(xlObj$accessed, "double")
  expect_type(xlObj$no.of.sheets, "integer")
  expect_type(xlObj$sheet.names, "character")
  expect_type(xlObj$contents, "list")
  expect_type(xlObj$contents[[1]], "list")
  expect_is(xlObj$file.size, "numeric")
  expect_is(xlObj$created, "POSIXct")
  expect_is(xlObj$modified, "POSIXct")
  expect_is(xlObj$accessed, "POSIXct")
  expect_is(xlObj$no.of.sheets, "integer")
  expect_is(xlObj$sheet.names, "character")
  expect_is(xlObj$contents, "list")
  expect_is(xlObj$contents[[1]], "data.frame")
})

test_that("input is validated for 'excelFile' constructor", {
  expect_error(excelFile(NULL), "argument is of length zero")
  expect_error(excelFile("file.doc"))
  expect_error(excelFile("anothefile.txt"), "Expected an MS Excel file ")
})
