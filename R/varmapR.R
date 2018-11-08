#' Run the CCG \code{varmap} Program to Create a Variogram Map.
#'
#' \code{varmapR} creates variogram maps for analysis of spatial continuity.
#' The number of variogram maps produced is fixed at 1.
#'
#' @param data Data frame with value and optional weight and category columns.
#' @param vars Character vector, column names in \code{data} for transformation.
#' @param regular Scalar integer either 1 or 0. 1: regular grid; 0: scattered
#'   values.
#' @param grid Numeric vector of length 6. Defines the grid parameters if
#'   \code{regular} is 1. The parameter are , in order, number of x, y, and z
#'   nodes in the grid and size of x, y, and z nodes in the grid.
#' @param xyz Character vector of lenght 2 for 2D samples or 3 for 3D samples.
#'   If \code{regular} is 0, \code{xyz} gives the x, y, and z coordinate
#'   column names of \code{data}.
#' @param nlag Numeric vector of length 3 for number of lags in each direction.
#' @param dlag Numeric vector of length 3 for size of lag in each direction.
#' @param minpair Scalar numeric. Minimum pairs per lag to report a result.
#' @param standardize Scalar integer either 1 or 0. 1: standardize the sill.
#' @param type Numeric vector length 3 for the tail, head, and variogram type.
#' @param debug Scalar boolean. If \code{TRUE} don't delete temporary system
#'   files.
#'
#' @return A data frame suitable for plotting with \code{ggplot2::geom_raster}.
#' @export
#' @examples
#' test <- unscoreR(samples_2d, c("thk", "accum"))
#' data <- varmapR(test$data, vars=c("NS_thk"), minpair = 1,
#'   nlag = c(100, 100, 1), dlag = c(20, 20, 1))

#'
varmapR <- function(
  data, vars, regular=0, grid=c(0, 0, 0, 0, 0, 0), xyz=c("x", "y"),
  nlag=c(50, 50, 0), dlag=c(5, 5, 5), minpair=5, standardize=0,
  type=c(1, 1, 1), debug=FALSE
  ) {

  # Get column indices.
  vars_i <- paste(which(colnames(data) %in% vars), collapse = "  ")
  if(length(xyz) == 3) {
    # 3D data with z-coordinate.
    xyz_i <- paste(which(colnames(data) %in% xyz), collapse = "  ")
  } else if(length(xyz) == 2) {
    # 2D data, no z-coordinate.
    xyz_i <- paste(c(
      which(colnames(data) %in% xyz),
      0),
      collapse = "  ")
  } else {
    # Vector of coordinate names must be either length 2 or 3.
    return(FALSE)
  }

  # Regular or scatter samples.
  if(regular == 0) {
    grid_n <- "50  50  1 \n"
    grid_d <- "1  1  1  \n"
    xyz_def <- paste0(xyz_i, "  \n")
  } else {
    grid_n <- paste(c(grid[1:3], "\n"), collapse = "  ")
    grid_d <- paste(c(grid[4:6], "\n"), collapse = "  ")
    xyz_def <- "1  2  0  \n"
  }

  # Export data for external program.
  write_gslib(data, "varmap-in.dat", title="Sample data")

  # Build a parameter file for the `varmap` executable.
  file_conn <- file("varmap.par")
  writeLines(
    paste0(
      "START OF PARAMETERS:\n",
      "varmap-in.dat \n",
      length(vars), "  ", vars_i, "  \n",
      "-1.0e21  1.0e21  \n",
      regular, "  \n",
      grid_n,
      grid_d,
      xyz_def,
      "varmap-out.dat  \n",
      paste(nlag, collapse = "  "), "  \n",
      paste(dlag, collapse = "  "), "  \n",
      minpair, "  \n",
      standardize, "  \n",
      "1  \n",
      paste(type, collapse = "  "), "  \n"
      ),
    file_conn)
  close(file_conn)

  # Run the unscore program.
  shell("varmap varmap.par")

  # Add coordinates to the output.
  addcoordR(
    "varmap-out.dat",
    realization = 1,
    xdef = c(nlag[1] * 2 + 1, 0, 1),
    ydef = c(nlag[2] * 2 + 1, 0, 1),
    zdef = c(nlag[3] * 2 + 1, 0, 1),
    dec = c(1, 1, -1))
  # Read the output.
  varmap_data <- read_gslib("addcoord-out.dat")
  varmap_data <- varmap_data[!is.na(varmap_data$variogram),]

  # Clean up.
  if(!debug) {
    shell("del varmap.par varmap-in.dat varmap-out.dat addcoord-out.dat")
  }

  return(varmap_data)

}
