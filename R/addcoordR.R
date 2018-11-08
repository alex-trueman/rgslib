#' Run the CCG \code{addcoord} Program to Create a Variogram Map.
#'
#' \code{addcoordR} creates variogram maps for analysis of spatial continuity.
#'
#' @param path GeoEase format system file.
#' @param realization Scalar numeric. Number of the realization to affect.
#' @param xdef,ydef,zdef Numeric vectors of length 3 defining the number of
#'   nodes, grid origin coordinate, and node spacing for x, y, and z axes.
#' @param dec Rounding of coordinates for x, y, and z.
#' @param debug Scalar boolean. If \code{TRUE} don't delete temporary system
#'   files. Default is \code{FALSE}.
#' @return Silently return \code{path}. Side effect is creation of system file
#'   called 'addcoord-out.dat' which is the input \code{path} file with the
#'   addition of coordinates.
#' @export
addcoordR <- function(
  path, realization=1, xdef=c(50, 2.5, 5.0), ydef=c(50, 2.5, 5.0),
  zdef=c(1, 0.5, 1.0), dec=c(3, 3, 3), debug=FALSE
  ) {

  # Build a parameter file for the `addcoord` executable.
  file_conn <- file("addcoord.par")
  writeLines(
    paste0(
      "START OF PARAMETERS:\n",
      path, " \n",
      "addcoord-out.dat  \n",
      realization, "  \n",
      paste(xdef, collapse = "  "), "  \n",
      paste(ydef, collapse = "  "), "  \n",
      paste(zdef, collapse = "  "), "  \n",
      paste(dec, collapse = "  "), "  \n"
      ),
    file_conn)
  close(file_conn)

  # Run the `addcoord` program.
  shell("addcoord addcoord.par")

  # Clean up.
  if(!debug) {
    shell("del addcoord.par")
  }

  invisible(path)

}
