#' excelfile S3 Object
#'
#' Constructs S3 objects of class 'excelfile', which contain properties
#' associated with .xls/.xlsx files.
#'
#' @param file A character vector of length \code{1} representing a path to an
#' MS Excel file.
#'
#' @return An object of class \emph{excelfile}. This object is essentially a
#' list with the following elements:
#' \itemize{
#'   \item fileName: The full file name (including extension)
#'   \item fileSize: The size of the file (in bytes)
#'   \item created: The time the file was created
#'   \item modified: The time the file was last modified
#'   \item noOfSheets: The number of (active) spreadsheets
#'   \item sheets: Names of the spreadsheets
#'   \item data: A list of data frames, one for each spreadsheet
#' }
#'
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#'
#' @export
excelfile <- function(file) {
  if (!is.character(file))
    stop("Expected a character vector")
  if (length(file) > 1) {
    file <- file[1]
    warning('Only the first element in "file" was used and the rest discarded')
  }
  if (!file.exists(file))
    stop("Path 'file' does not exist")
  if (!grepl('.xls$|.xlsx$', file))
    stop("Expected a file with extension '.xls' or '.xlsx'")

  ## Identify individual spreadsheets
  sheetNames <-  readxl::excel_sheets(file)
  sheetList <- lapply(sheetNames, function(sht) {    # TODO: Use sheet names
    readxl::read_excel(path = file, sheet = sht, col_types = "text")})
  prop <- file.info(file)
  structure(
    list(
      fileName = basename(file),
      location = dirname(file),
      fileSize = prop$size,
      created = prop$ctime,
      modified = prop$mtime,
      imported = Sys.time(),
      noOfSheets = length(sheetNames),
      sheets = sheetNames,
      data = sheetList
    ),
    class = "excelfile"
  )
  # TODO: Apply some limits.
}









#' S3 method for class 'excelFile'
#'
#' @param x An object of class 'excelfile'
#' @param ... Other parameters
#' @export
#' @rdname excelfile
print.excelfile <- function(x, ...) {
  cat(sprintf(
    "Filename: %s\nNo. of sheets: %d\n",
    x$fileName,
    x$noOfSheets
  ))
}



## TODO: S3 method to provide a summary of the object
# summary.excelfile <- function(xlobj) {
#     cat(paste0(
#         "File: ",
#         sQuote(xlObj$fileName),
#         ". Size:",
#         xlObj$fileSize,
#         "B\n"
#     ))
#     cat("Spreadsheet(s):\n")
#     for (i in 1:xlObj$noOfSheets) {
#         cat(paste0(i, ". ", sQuote(xlObj$sheets), "\n"))
#     }
# }
