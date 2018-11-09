#' Run the CCG \code{varfit_lmc} Program to Calculate Experimental Variograms.
#'
#' \code{varfit_lmcR} calculates experimental variograms. THis process is suited to
#' irregulalry spaced data such as samples. If more than one variable is input
#' all possible semi-variograms and cross-variograms are calculated. Trimming
#' limits are fixed to -1.0e21 and +1.0e21.
#'
#' @param data Data frame of sample data.
#' @param vars Character vector, column names in \code{data} for variograms.
#' @param xyz Character vector, column names in \code{data} for coordinate
#'   fields, 2 for 2D and 3 for 3D.
#' @param vpar list of named vectors containing parameters for each variogram
#'   direction to be calculated. See function argument default for spec.
#' @param standardize Scalar integer either 0 or 1. 1: standardize the sill.
#' @param legacy Scalar integer either 0 or 1. 1: output legacy format.
#' @param check Scalar integer either 0 or 1. 1: run input checks.
#' @param debug Scalar boolean. If \code{TRUE} don't delete temporary system
#'   files.
#'
#' @return A data frame with calculated variogram data.
#' @export
#' @examples
#' test <- unscoreR(samples_2d, c("thk", "accum"))
#' data <- varfit_lmcR(test$data, vars=c("NS_thk"))
varfit_lmcR <- function(
  data, vars, xyz=c("x", "y"), vpar=list(
    dir1=c(azm=0, azmtol=22.5, bndhorz=1000, dip=0, diptol=22.5, bndvert=1000,
      tilt=0, nlags=10, dlag=30, lagtol=15),
    dir2=c(azm=90, azmtol=22.5, bndhorz=1000, dip=0, diptol=22.5, bndvert=1000,
      tilt=0, nlags=10, dlag=30, lagtol=15),
    dir3=c(azm=0, azmtol=22.5, bndhorz=1000, dip=90, diptol=22.5, bndvert=1000,
      tilt=0, nlags=10, dlag=30, lagtol=15)
    ),
    standardize=0, legacy=0, check=1, debug=FALSE
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
  if(nvars == 1) {
    nvarios <- 1
  } else {
    nvarios <- (factorial(nvars) / (factorial(2) *
        factorial(nvars - 2))) + nvars
  }

  # Build a parameter string.
  parstring <- c(
    "START OF PARAMETERS:",
    paste0(nvars, "  "),
    paste0(varcalcs, "  "),
    paste0(varfits, "  ")
  )
  # Build correaltion matrix.

  parstring <- c(
    parstring,
    paste0(crossnugget, "  "),
    paste0(idw, "  ", pweight, "  ", minpair, "  "),
    paste0(maxiter, "  "),
    paste0(varfitslmc, "  "),
    "corrs.txt  "
  )




    # Add variogram calculation parameters for each direction.
  for(dir in seq_along(vpar)) {
    parstring <- c(
      parstring,
      paste(vpar[[dir]][1:7], collapse = " "),
      paste(vpar[[dir]][8:10], collapse = " "))
  }
  parstring <- c(
    parstring,
    "varfit_lmc-out.dat  ",
    paste0(legacy, "  "),
    paste0(check, "  "),
    paste0(standardize, "  "),
    paste0(nvarios, "  ")
  )
  # Add variogram type definition for the variograms.
  # Also make a 'dictionary' for merging character variable snames on return.
  vars_df <- data.frame(
    head = numeric(), tail = numeric(),
    head_a = character(), tail_a = character())
  for(v in 1:nvars) {
    parstring <- c(
      parstring,
      paste0(v, "  ", v, "  1  ")
    )
    vars_df <- rbind(vars_df, data.frame(head = v, tail = v,
      head_a = vars[v], tail_a = vars[v]))
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
        paste0(t1start, "  ", t2, "  2  ")
      )
      t2 <- t2 + 1
      nruns <- nruns - 1
    }
    t1start <- t1start + 1
    t2start <- t2start + 1
  }

  # Write parameters to a file.
  file_conn <- file("varfit_lmc.par")
  writeLines(parstring, file_conn)
  close(file_conn)

  # Export sample data for external program.
  write_gslib(data, "varfit_lmc-in.dat")

  # Run the `varfit_lmc` program.
  shell("vl_lmc varfit_lmc.par")

  # Read the output of `varfit_lmc` and format for return.
  vario_data <- read_gslib("varfit_lmc-out.dat")
  new_col_names <- c("set", "h", "np", "gamma", "vario", "azi", "dip", "type",
    "tail", "head")
  names(vario_data) <- new_col_names
  vario_data <- merge(x = vario_data, y = vars_df[,c(2, 4)],
    by = "tail", all.x = TRUE)
  vario_data <- merge(x = vario_data, y = vars_df[,c(1, 3)],
    by = "head", all.x = TRUE)
  vario_data[,"vars"] <- paste0(vario_data[,"tail_a"], "-",
    vario_data[,"head_a"])
  vario_data <- vario_data[,-c(1:2, 11:12)]

  # Clean up.
  if(!debug) {
    shell("del varfit_lmc.par")
  }

  return(vario_models)

}

