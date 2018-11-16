#' Run the CCG \code{varcalc} Program to Calculate Experimental Variograms.
#'
#' \code{varcalcR} calculates experimental variograms. This process is suited to
#' irregulalry spaced data such as samples. If more than one variable is input
#' all possible semi-variograms and cross-variograms are calculated. Default
#' arguments will create omni-directional 2D variograms for each variable.
#'
#' Variogram calculation parameters are provided as a list of vectors for each
#' direction. The default list provided in the function argument list uses a
#' named vector. The vector does not need ot me named but values must be
#' provided in the order shown.
#'
#' @param data Data frame of sample data.
#' @param vars Character vector, column names in \code{data} for variograms.
#' @param sysout Character: name of GSLIB-format experimental variogram file
#'   without the extension part, and extension of ".out" is added for
#'   compatibility with \code{vl_lmc}.
#' @param xyz Character vector, column names in \code{data} for coordinate
#'   fields, 2 for 2D and 3 for 3D.
#' @param vpar list of named vectors containing parameters for each variogram
#'   direction to be calculated. See function argument default for spec.
#' @param variostd Scalar boolean, if \code{TRUE} standardize the sill.
#' @param variosills Numeric scalar. Sill to standardize to if \code{variostd}
#'   is \code{TRUE}. Default \code{NULL} means let program decide.
#' @param legacy Scalar boolean, if \code{TRUE} output legacy format.
#' @param strict Scalar boolean, if \code{TRUE} run input checks.
#' @param single Boolean: produce a single variogram rather than direct and
#'   cross variograms.
#' @param debug Scalar boolean. If \code{TRUE} don't delete temporary system
#'   files.
#' @return A data frame of class "gstat::gstatVariogram" with calculated
#'   variogram data. Side effect is output variogram calculation file from
#'   \code{varcalc} program. This file is used by other programs such as
#'   \code{varmod}.
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' test <- unscoreR(samples_2d, c("thk", "accum"))
#' data <- varcalcR(test$data, vars=c("NS_thk"), sysout="varcalc")
varcalcR <- function(
  data, vars, sysout, xyz=c("x", "y"),
  vpar=list(dir1=c(azm=0, azmtol=90,
    bandhorz=1.0e+21, dip=0, diptol=90, bandvert=1.0e+21, tilt=0, nlags=10,
    lagdist=30, lagtol=0.5)),
  variostd=FALSE, variosills=NULL, legacy=FALSE, strict=TRUE, single=TRUE,
  debug=TRUE
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

  # Number of variables.
  nvars <- length(vars)
  # Number of variograms.
  if(nvars == 1 | single) {
    nvarios <- 1
  } else {
    nvarios <- (factorial(nvars) / (factorial(2) *
        factorial(nvars - 2))) + nvars
  }

  # Build a parameter string.
  parstring <- c(
    "START OF PARAMETERS:",
    "varcalc-in.dat ",
    paste0(xyz_i, "  "),
    paste0(nvars, "  ", vars_i, "  "),
    "-999  1.0e21  ",
    paste0(length(vpar), "  ")
  )
  # Add variogram calculation parameters for each direction.
  for(dir in seq_along(vpar)) {
    parstring <- c(
      parstring,
      paste(vpar[[dir]][1:7], collapse = " "),
      paste(vpar[[dir]][8:9], vpar[[dir]][9] * vpar[[dir]][10], collapse = " "))
  }
  parstring <- c(
    parstring,
    paste0(sysout, ".out  "),
    paste0(as.numeric(legacy), "  "),
    paste0(as.numeric(strict), "  "),
    paste0(as.numeric(variostd), "  "),
    paste0(nvarios, "  ")
  )
  # Add variogram type definition for the variograms.
  if(single & nvars == 2) {
    parstring <- c(
      parstring,
      paste0("1  2  2  ", variosills, "  ")
    )
  } else {
    for(v in 1:nvars) {
      parstring <- c(
        parstring,
        paste0(v, "  ", v, "  1  ", variosills, "  ")
      )
    }
    # Add variogram type definition for the cross-variograms.
    t1start <- 1
    t2start <- 2
    nruns <- nvarios - nvars
    while(nruns > 0) {
      t2 <- t2start
      for(cv in 1:(nvars - t1start)) {
        parstring <- c(
          parstring,
          paste0(t1start, "  ", t2, "  2  ", variosills, "  ")
        )
        t2 <- t2 + 1
        nruns <- nruns - 1
      }
      t1start <- t1start + 1
      t2start <- t2start + 1
    }
  }

  # Write parameters to a file.
  file_conn <- file("varcalc.par")
  writeLines(parstring, file_conn)
  close(file_conn)

  # Export sample data for external program.
  write_gslib(data, "varcalc-in.dat")

  # Run the `varcalc` program.
  shell("varcalc varcalc.par")

  # Read the output of `varcalc` and format for return.
  vario_data <- read_gslib(paste0(sysout, ".out")) %>%
    mutate(id = paste0(
      .data$VariogramTailIndex, "-", .data$VariogramHeadIndex)) %>%
    select(np = .data$NumberofPairs, dist = .data$LagDistance,
      gamma = .data$VariogramValue, dir.hor = .data$CalculationAzimuth,
      dir.ver = .data$CalculationDip, .data$id)
  class(vario_data) <- c("gstatVariogram", "data.frame")

  # Clean up.
  if(!debug) {
    shell("del varcalc.par varcalc-in.dat")
  }

  return(vario_data)

}
