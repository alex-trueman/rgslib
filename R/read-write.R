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

  # Read the header.
  header <- read_gslib_header(path)

  # Read in the data. GSLIB column types are always numeric.
  # `fread` is much better at reading this format than `readr::read_table2`,
  # `readr` tries to create a column for training spaces, it doesn't but throws
  # lots messy warnings.
  data <- fread(
    path,
    col.names = header$col_names,
    skip = 2 + header$col_count,
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
#' data in the data frame or the name of the data frame object. Only numeric
#' columns are written to the system file and they are rounded.
#'
#' If grid argument \code{griddim} is given a grid definition will be written to
#' the output GeoEase file. If there is more than 1 realization in the grid data
#' the the input data frame (\code{data}) must be sorted by realization and must
#' have the same number of records for each realization.
#'
#' @param data Data frame to write to disk, only numeric columns are exported.
#' @param path Path or connection to write to.
#' @param round Integer scalar, number of digits to round data before export.
#' @param title Character title to be appended to GeoEase file. If not supplied
#'   it will be determined from \code{data} meta-data or it's name.
#' @param griddim Numeric vector of xyz grid dimensions. Length 2 for 2D.
#' @param gridxyz Character vector of grid xyz coordinate columns.
#' @param gridrealz Scalar integer number of realizations if grid file.
#' @return \code{write_gslib} returns the input \code{data} invisibly. The
#'   function side-effect is a GeoEase-format system file.
#' @export
#' @importFrom data.table fwrite
#' @examples
#' samples_na <- samples_2d
#' samples_na[3:7, 5] <- NA
#' write_gslib(samples_na, "samples-na.dat", round = 3, title = "sample data")
write_gslib <- function(
  data, path, round=4, title=NULL, griddim=NULL, gridxyz=NULL, gridrealz=NULL) {

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
  data_gslib <- round(data[,nums], round)

  # Write GeoEase header.
  ncols <- ncol(data_gslib)
  write(title, path)
  # Grid definition if grid arguments are given.
  if(!is.null(griddim)) {
    grid_def <- create_gslib_griddef(data_gslib, griddim, gridxyz, gridrealz)
    header <- paste0(ncols, "  ", paste(grid_def, collapse = "  "))
    write(header, path, append = TRUE)
  } else {
    write(ncols, path, append = TRUE)
  }
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
#' @param grid A named numeric vecotor grid definition. Only used if there
#'   is no definition in the GeoEase file.
#' @return A data frame containing realization number, cartesian coordinates,
#'   and simulated data.
#' @export
#' @importFrom data.table fread
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom readr read_lines
read_gslib_usgsim <- function(path, vars, grid=NULL) {

  # Read the header.
  header <- read_gslib_header(path)
  # Build grid definition if none in file.
  if(length(header) < 3 & !is.null(grid)) {
    grid_def <- grid
  } else {
    grid_def <- header$grid_def
  }

  # Read the data.
  data <- read_gslib(path)
  colnames(data) <- vars

  # Create coordinates and realizations.
  n_grid_points <- grid_def["n_x"] * grid_def["n_y"] * grid_def["n_z"]
  grid_x <- seq(grid_def["min_x"], grid_def["min_x"] + (grid_def["dim_x"] *
      (grid_def["n_x"] - 1)), grid_def["dim_x"])
  grid_y <- seq(grid_def["min_y"], grid_def["min_y"] + (grid_def["dim_y"] *
      (grid_def["n_y"] - 1)), grid_def["dim_y"])
  grid_z <- seq(grid_def["min_z"], grid_def["min_z"] + (grid_def["dim_z"] *
      (grid_def["n_z"] - 1)), grid_def["dim_z"])
  data[, "r"] <- rep(1:grid_def["realz"], each = n_grid_points)
  data[, "x"] <- rep(grid_x, times = grid_def["realz"], each = 1)
  data[, "y"] <- rep(grid_y, times = grid_def["realz"], each = grid_def["n_x"])
  data[, "z"] <- rep(grid_z, times = grid_def["realz"], each = grid_def["n_x"] *
      grid_def["n_y"])

  # Arrange columns.
  data <- data[, c("r", "x", "y", "z", vars)]

  # Read the title to be added as a comment to the output data.
  comment(data) <- as.character(read_lines(path, skip = 0, n_max = 1))

  return(data)

}

#' Create a GSLIB Grid Definition from a Grid Data Frame.
#'
#' @param data Data frame representing grid.
#' @param dims Numeric vector of xyz grid node spacing. Length 2 for 2D.
#' @param xyz Character vector of xyz coordinate column names. Length 2 for 2D.
#' @param realz Number of realizations.
#'
#' @return Named numeric vector of grid definition.
create_gslib_griddef <- function(data, dims=c(1, 1), xyz=c("x", "y"), realz=1) {

  dim_x <- dims[1]
  min_x <- min(data[,xyz[1]])
  n_x <- round((max(data[,xyz[1]]) - min_x) / dim_x, 0)
  dim_y <- dims[2]
  min_y <- min(data[,xyz[2]])
  n_y <- round((max(data[,xyz[2]]) - min_y) / dim_y, 0)

  if(length(xyz) == 3) {
    dim_z <- dims[3]
    min_z <- min(data[,xyz[3]])
    n_z <- round((max(data[,xyz[3]]) - min_z) / dim_z, 0)
  } else {
    dim_z <- 1
    min_z <- 0
    n_z <- 1
  }

  griddef <- c(n_x=n_x, n_y=n_y, n_z=n_z, min_x=min_x, min_y=min_y, min_z=min_z,
    dim_x=dim_x, dim_y=dim_y, dim_z=dim_z, realz=realz)

  return(griddef)

}

#' Get Vector of Column Indices for a Data Frame.
#'
#' @param data Data frame
#' @param vars Character vector of column names in \code{data}.
#'
#' @return Numeric vector of column indices.
get_column_indices <- function(data, vars) {

  col_i <- which(colnames(data) %in% vars)
  return(col_i)

}



#' Get Header from GSLIB Simplified GeoEase File
#'
#' \code{read_gslib_header} Reads the header of a GSLIB simplified GeoEase format
#' file. It returns information about the columns in the file and, if it is a
#' grid file, it returns the grid definition.
#'
#' @param path Character string name of system file in GeoEase format.
#'
#' @return List containing the column definition and, if applicable, a grid
#' definition.
#' @importFrom readr read_lines
read_gslib_header <- function(path){

  # Read the header.
  header <- as.numeric(unlist(strsplit(
    sub("^ +", "", read_lines(path, skip = 1, n_max = 1)),
    split = " +"
  )))

  # Create a column definition.
  n_cols <- header[1]
  names <- gsub(" +", "", read_lines(path, skip = 2, n_max = n_cols))

  # Create a grid definition.
  if(length(header) > 1) {
    def <- c(
      n_x=header[2], n_y=header[3], n_z=header[4],
      min_x=header[5], min_y=header[6], min_z=header[7],
      dim_x=header[8], dim_y=header[9], dim_z=header[10],
      realz=header[11]
    )
    header <- list(col_count=n_cols, col_names=names, grid_def=def)
  } else {
    header <- list(col_count=n_cols, col_names=names)
  }

  return(header)

}
