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

#' Import GSLIB Variogram Model Format Data to a Data Frame.
#'
#' \code{read_gslib_mvario} imports from GSLIB variogram model format. All
#' columns are read as doubles as GSLIB does not support non-numeric data
#' columns.
#'
#' @param path Path or connection to read from.
#' @return A data frame.
#' @export
#' @importFrom dplyr left_join select
#' @importFrom magrittr %>% %<>%
#' @importFrom readr read_lines
#' @importFrom rlang .data
read_gslib_mvario <- function(path) {

  # Get number of structures and nugget effect.
  datadef <- unlist(strsplit(
    sub("^ +", "", read_lines(path, n_max = 1)), split = " +"))
  nst <- as.integer(datadef[1])
  c0 <- as.numeric(datadef[2])

  # Create variogram model data frame.
  mvario <- data.frame(
    type=numeric(length = nst + 1),
    psill=numeric(length = nst + 1),
    range=numeric(length = nst + 1),
    anis1=numeric(length = nst + 1),
    anis2=numeric(length = nst + 1),
    ang1=numeric(length = nst + 1),
    ang2=numeric(length = nst + 1),
    ang3=numeric(length = nst + 1)
  )
  mvario[mvario == 0] <- NA

  # Populate nugget.
  mvario[1,"type"] <- 0
  mvario[1,"psill"] <- c0
  mvario[1,"range"] <- 0

  # Populate structures.
  record <- 1
  structures <- sub("^ +", "", read_lines(path, skip = 1))
  for(st in seq(1, length(structures) - 1, by = 2)) {
    record <- record + 1
    line1 <- as.numeric(unlist(strsplit(structures[st], split = " +"))[1:5])
    line2 <- as.numeric(unlist(strsplit(structures[st + 1], split = " +"))[1:3])
    if(st == 1) {
      mvario[1, "ang1"] <- line1[3]
      mvario[1, "ang2"] <- line1[4]
      mvario[1, "ang3"] <- line1[5]
    }
    mvario[record, "type"] <- line1[1]
    mvario[record, "psill"] <- line1[2]
    mvario[record, "range"] <- line2[1]
    mvario[record, "anis1"] <- ifelse(
      line2[2] / line2[1] > 1, 1, line2[2] / line2[1])
    mvario[record, "anis2"] <- ifelse(
      line2[3] / line2[1] > 1, 1, line2[3] / line2[1])
    mvario[record, "ang1"] <- line1[3]
    mvario[record, "ang2"] <- line1[4]
    mvario[record, "ang3"] <- line1[5]
  }

  # Create data frame of class "gstat::variogramModel".
  mvario %<>%
    left_join(structure_types(), by = "type") %>%
    select(.data$model, .data$psill, .data$range, .data$ang1, .data$ang2,
      .data$ang3, .data$anis1, .data$anis2, .data$type)

  class(mvario) <- c("variogramModel", "data.frame")

  return(mvario)

}

#' Valid Variogram Model Structure Types
#'
#' Links GSLIB numeric structure types to \code{gstat} short alphanumeric
#' structure types.
#'
#' @return Data frame of structure types.
structure_types <- function() {
  x <- data.frame(
    type = c(0, 1, 2, 3, 4),
    model = c("Nug", "Sph", "Exp", "Gau", "Hol")
  )
  return(x)
}

#' Import \code{usgsim} regular output format and add coordinates.
#'
#' \code{read_gslib_usgsim} imports the regular output format of
#'   \code{usgsim}. This format has no grid coordinates. Coordinates are
#'   added based on the grid definition on line 2 of the \code{usgsim} output.
#'
#' @param path Path or connection to read from.
#' @param vars Character vector of simulated column names.
#' @return A data frame containing realization number, cartesian coordinates,
#'   and simulated data.
#' @export
#' @importFrom data.table fread
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom readr read_lines
read_gslib_usgsim <- function(path, vars) {

  # Get the grid definition..
  datadef <- as.numeric(unlist(strsplit(
    sub("^ +", "", read_lines(path, skip = 1, n_max = 1)),
    split = " +"
  )))
  nsimvar <- datadef[1]
  n_x <- datadef[2]
  n_y <- datadef[3]
  n_z <- datadef[4]
  min_x <- datadef[5]
  min_y <- datadef[6]
  min_z <- datadef[7]
  dim_x <- datadef[8]
  dim_y <- datadef[9]
  dim_z <- datadef[10]
  n_realz <- datadef[11]
  grid_x <- seq(min_x, min_x + (dim_x * (n_x - 1)), dim_x)
  grid_y <- seq(min_y, min_y + (dim_y * (n_y - 1)), dim_y)
  grid_z <- seq(min_z, min_z + (dim_z * (n_z - 1)), dim_z)
  n_grid_points <- n_x * n_y * n_z

  # Read in the data. GSLIB column types are always numeric.
  # `fread` is much better at reading this format than `readr::read_table2`,
  # `readr` tries to create a column for trailing spaces, it doesn't but
  # throws lots messy warnings.
  data <- fread(
    path,
    col.names = vars,
    skip = 2 + nsimvar,
    na.strings = c("", "-1.0e21", "-999.00000", "-999"),
    data.table = FALSE
  )
  data[, "r"] <- rep(1:n_realz, each = n_grid_points)
  data[, "x"] <- rep(grid_x, times = n_realz, each = 1)
  data[, "y"] <- rep(grid_y, times = n_realz, each = n_x)
  data[, "z"] <- rep(grid_z, times = n_realz, each = n_x * n_y)
  data <- data[, c("r", "x", "y", "z", vars)]

  # Read the title to be added as a comment to the output data.
  comment(data) <- as.character(read_lines(path, skip = 0, n_max = 1))

  return(data)

}
