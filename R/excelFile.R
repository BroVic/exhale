#' Excel file objects
#'
#' Constructs S3 objects of class 'excelFile', which contain
#' properties associated with .xls/.xlsx files
#'
#' @param file Path to an MS Excel file
#'
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#'
#' @export
excelFile <- function(file)
{
  if(!grepl(".xls$|.xlsx$", file))
    stop("Expected an MS Excel file with '.xls' or '.xlsx' extension.")
  sheetNames <- excel_sheets(file)
  sheetList <- lapply(sheetNames, function(sht) {
    read_excel(path = file,
               sheet = sht,
               col_types = "text")
  })
  prop <- file.info(file)
  invisible(structure(
    list(
      filename = basename(file),
      file.size = prop$size,
      created = prop$ctime,
      modified = prop$mtime,
      accessed = prop$atime,
      no.of.sheets = length(sheetNames),
      sheet.names= sheetNames,
      contents = sheetList
    ),
    class = "excelFile"
  ))
  # TODO: Apply some limits.
}
