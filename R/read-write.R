#' Import GSLIB's simplified GeoEase data format to a data frame.
#'
#' \code{read_gslib} imports from GeoEase format. All columns are read as
#' doubles as GSLIB does not support non-numeric data columns. The title is
#' appended to the data as a comment. Will import a GSLIB grid file but does
#' nothing with the grid definition information for now.
#'
#' @param path Path or connection to read from.
#' @return A data frame.
#' @export
#' @importFrom data.table fread
#' @importFrom readr read_lines
#' @examples
#' read_gslib(rgslib_example("samples-na.dat"))
read_gslib <- function(path) {

  # For future support of importing grid files that have additional information
  # about the grid definition on the second line. Get the definition and remove
  # any leading spaces.
  datadef <- as.integer(unlist(strsplit(
    sub("^ +", "", read_lines(path, skip = 1, n_max = 1)),
    split = " +"
  )))
  nvar <- datadef[1]

  # Read the column names from line 3 to nvar + 2.
  # GSLIB allows spaces in names, remove these.
  names <- gsub(" +", "", read_lines(path, skip = 2, n_max = nvar))

  # Read in the data. GSLIB column types are always numeric.
  # `fread` is much better at reading this format than `readr::read_table2`,
  # `readr` tries to create a column for training spaces, it doesn't but throws
  # lots messy warnings.
  data <- fread(
    path,
    col.names = names,
    skip = 2 + nvar,
    na.strings = c("", "-1.0e21", "-999.00000", "-999"),
    data.table = FALSE
    )

  # Read the title to be added as a comment to the output data.
  comment(data) <- as.character(read_lines(path, skip = 0, n_max = 1))

  return(data)

}

#' Export a Data Frame to Simplified GeoEase Format Used by GSLIB.
#'
#' \code{write_gslib} writes a data frame to the GeoEase format used by GSLIB
#' programes. GeoEase has a standard header with a title and number and names of
#' the columns. The title can be supplied or is built automatically from meta-
#' data in the data frame or the name of the data fram object. Only numeric
#' columns are written to the system file and they are rounded. This version
#' not support wrting of the grid-format GSLIB data file.
#'
#' @param data Data frame to write to disk, only numeric columns are exported.
#' @param path Path or connection to write to.
#' @param round Integer scalar, number of digits to round data before export.
#' @param title Character title to be appended to GeoEase file. If not supplied
#'   it will be determined from \code{data} meta-data or it's name.
#' @return \code{write_gslib} returns the input \code{data} invisibly.
#' @export
#' @importFrom data.table fwrite
#' @examples
#' samples_na <- samples_2d
#' samples_na[3:7, 5] <- NA
#' write_gslib(samples_na, "samples-na.dat", round = 3, title = "sample data")
write_gslib <- function(data, path, round = 4, title = NULL) {

  # If no title is supplied try to make one.
  if(is.null(title)) {
      title <- comment(data)
      if(is.null(title)) {
          title <- deparse(substitute(data))
      }
  }

  # GSLIB only supports numeric data types.
  # Also round numbers to prevent messy output.
  nums <- unlist(lapply(data, is.numeric))
  data <- round(data[,nums], round)

  # Write GeoEase header.
  ncols <- ncol(data)
  write(title, path)
  write(ncols, path, append = TRUE)
  for(i in 1:ncols) {
      write(colnames(data[i]), path, append = TRUE)
  }

  # Write the data.
  fwrite(data, path, append = TRUE, sep = " ", na = "-1.0e21",
    col.names = FALSE)

  invisible(data)

}
