#' Find a Header
#'
#' Discover the likely location of a header in an Excel spreadsheet
#'
#' @details Finds the row that likely contains the actual header
#' @return Returns an S3 object that is a marker to the row it occupies
#'
#' @param df An object of class \code{data.frame}
#' @param hdr A character vector containing the expected header values
#' @param quietly logical; whether to print out progress or not
#'
#' @export
locate_header <- function(df, hdr, quietly = TRUE) {
  if (!inherits(df, "data.frame"))
    stop("'df' is not a valid data frame")
  if (!is.character(hdr))
    stop("'hdr' is not a character vector")

  ## Iterate row-wise
  val <- NULL
  for (i in 1:nrow(df)) {
    ## Check whether we hit something that looks like column names
    ## and when we do, stop looking.
    if (any(tolower(hdr) %in% tolower(df[i, ]))) {
      if (!quietly) {
        cat(
          paste0(
            "\tA header candidate was found on row ",
            i,
            ":\n\t"
          ),
          sQuote(df[i, ]),
          "\n"
        )
      }
      hdr <- as.character(df[i,])
      val <- structure(list(
        header = hdr,
        rownum = i,
        nextrow = i + 1
      ),
      class = "header-locator")
      break
    }
  }
  invisible(val)
}
