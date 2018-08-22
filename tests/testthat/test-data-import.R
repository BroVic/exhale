## test-data-import.R

# context("Data import")
#
# test_that("an 'excelFile' object can be created", {
#   tstFile <- "test-dir/test-file-2.xlsx"
#   xlObj <- excelFile(tstFile)
#
#   expect_is(xlObj, "excelFile")
#   expect_s3_class(xlObj, "excelFile")
#   expect_equal(xlObj$filename, basename(tstFile))
#   expect_type(xlObj$file.size, "double")
#   expect_type(xlObj$created, "double")
#   expect_type(xlObj$modified, "double")
#   expect_type(xlObj$accessed, "double")
#   expect_type(xlObj$no.of.sheets, "integer")
#   expect_type(xlObj$sheet.names, "character")
#   expect_type(xlObj$contents, "list")
#   expect_type(xlObj$contents[[1]], "list")
#   expect_is(xlObj$file.size, "numeric")
#   expect_is(xlObj$created, "POSIXct")
#   expect_is(xlObj$modified, "POSIXct")
#   expect_is(xlObj$accessed, "POSIXct")
#   expect_is(xlObj$no.of.sheets, "integer")
#   expect_is(xlObj$sheet.names, "character")
#   expect_is(xlObj$contents, "list")
#   expect_is(xlObj$contents[[1]], "data.frame")
# })
#
# test_that("input is validated for 'excelFile' constructor", {
#   expect_error(excelFile(NULL), "argument is of length zero")
#   expect_error(excelFile("file.doc"))
#   expect_error(excelFile("anothefile.txt"), "Expected an MS Excel file ")
# })

############################################
context("Data importation")

xl.files <- c('test-file-1.xls', 'test-file-2.xlsx')

test_that("excelfile constructor input is validated", {
  expect_error(excelfile(),
               'argument "file" is missing, with no default')
  expect_error(excelfile(42),
               'Expected a character vector')
  expect_error(excelfile('nopath'),
               "Path 'file' does not exist")
  expect_error(excelfile('test-spreadsheet.R'),
               "Expected a file with extension '.xls' or '.xlsx'")
  expect_warning(excelfile(xl.files),
                 'Only the first element in "file" was used')
})

samplFile1 <- excelfile(xl.files[1])
samplFile2 <- excelfile(xl.files[2])

test_that("Objects are properly instantiated", {
  expect_is(samplFile1, "excelfile")
  expect_is(samplFile2, "excelfile")
  expect_s3_class(samplFile1, "excelfile")
  expect_s3_class(samplFile2, "excelfile")
  expect_type(samplFile1$fileName, "character")
  expect_type(samplFile1$fileSize, "double")
  expect_type(samplFile1$created, "double")
  expect_type(samplFile1$modified, "double")
  expect_type(samplFile1$noOfSheets, "integer")
  expect_type(samplFile1$sheets, "character")
  expect_type(samplFile2$data, "list")
  expect_type(samplFile2$fileName, "character")
  expect_type(samplFile2$fileSize, "double")
  expect_type(samplFile2$created, "double")
  expect_type(samplFile2$modified, "double")
  expect_type(samplFile2$noOfSheets, "integer")
  expect_type(samplFile2$sheets, "character")
  expect_type(samplFile2$data, "list")
})
