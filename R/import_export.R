#' Import GSLIB's simplified GeoEase data format to a tibble.
#'
#' Import the data from Geo-EAS format. All columns are read as doubles
#' GSLIB does not support non-numeric data columns. The titla is appended to
#' the data as a comment.
#'
#' @param file character, name and path of data file.
#'
#' @return a tibble
#' @export
#' @importFrom readr read_lines read_table2
#'
#' @examples
#' read_gslib("test.dat")
read_gslib <- function(file) {

  # Read the number of columns from the second line.
  nvar <- as.integer(
      sub("^(\\d+).+", "\\1", read_lines(file, skip = 1, n_max = 1))
      )

  # Read the column names from line 3 to nvar + 2.
  names <- as.character(
    sub(
      "(\\w+).*",
      "\\1",
      read_lines(file, skip = 2, n_max = nvar)
      )
  )
  # Fix illegal column names.
  names <- make.names(names, unique = TRUE, allow_ = TRUE)

  # Read in the data. GSLIB column types are always numeric.
  data <- read_table2(
    file,
    col_names = names,
    col_types = paste(rep("d", nvar), collapse = ""),
    skip = 2 + nvar
    )

  # Read the title to be added as a comment to the output data.
  comment(data) <- as.character(read_lines(file, skip = 0, n_max = 1))

  return(data)

}

#' Export a tibble to GeoEase format.
#'
#' @param data a data frame to export.
#' @param file character, name and path of Geo-EAS format data file.
#'
#' @export
#' @importFrom dplyr select_if
#' @importFrom readr write_delim
#' @importFrom rland quo_name
#'
#' @examples
write_gslib <- function(df, file, title = NA) {

  # If no title is supplied try to make one.
  if(is.na(title)) {
      title <- comment(df)
      if(is.null(title)) {
          title <- deparse(substitute(df))
      }
  }

  # GSLIB only supports numeric data types.
  data <- select_if(df, is.numeric)

  # Build header.
  ncols <- ncol(data)
  write(title, file)
  write(ncols, file, append = TRUE)
  for (i in 1:ncols) {
      write(colnames(data[,i]), file, append = TRUE)
  }

  # Write the data.
  write_delim(data, file, delim = " ", append = TRUE)

}
