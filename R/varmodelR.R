#' Run the CCG \code{varmodel} Program to Auto-Fit Variograms.
#'
#' \code{varmodelR} auto fits variogram models. This version is less flexible
#'   the pure Fortran program. It is designed to support modelling of univariate
#'   variogram models. For multivariate models it is designed to model the
#'   direct variograms with the cross-variograms modeled in the \code{vl_lmc}
#'   function. A large number of structures can be modelled (in additiona to the
#'   nugget effect), but they all must have the same structure type, orientation,
#'   and range parameters. If these parametrs are 'wildcards' they may produce
#'   structures with different type and range.
#'
#'   Wildcards are character strings of the form '?': unconstrained; 'min:max':
#'   constrained minimum and maximum; 'min:': constrained minimum; ':max':
#'   constrained maximum. The arguments \code{c0}, \code{it}, \code{cc},
#'   \code{azm}, \code{dip}, \code{tilt}, \code{ahmax}, \code{ahmin}, and
#'   \code{avert} all support wildcards. The argument \code{c0} does not
#'   support the '?' wildcard due to a possible bug in the Frotran program.
#'
#' @param expdata Character: name of input GSLIB-format experiemental variogram
#'   file.
#' @param sysout Character: name of output GSLIB-format variogram model file.
#' @param ndir Numeric integer: number of directions to model.
#' @param nst Scalar integer: number of structures to model. Default 10.
#' @param c0 Scalar numeric: nugget effect or wildcard.
#' @param it Scaler integer: structure type in range 1:5 or wildcard.
#' @param cc Scalar numeric: variance contribution or wildcard.
#' @param azm Numeric vector: azimuth in GSLIB convention or wildcard. Wildcard
#'   not recommended. Must be length \code{ndir} with first value being major
#'   axis azimuth.
#' @param dip Numeric vector: dip in GSLIB convention or wildcard. Wildcard
#'   not recommended. Must be length \code{ndir} with first value being major
#'   axis dip.
#' @param tilt Scalar numeric: tilt or major axis in GSLIB convention or
#'   wildcard. Wildcard not recommended.
#' @param ahmax Scalar numeric: major axis range or wildcard.
#' @param ahmin Scalar numeric: semi-major axis range or wildcard.
#' @param avert Scalar numeric: minor axis range or wildcard.
#' @param fit Boolean: apply autofit.
#' @param maxiter Scalar integer: Number of fitting iterations.
#' @param vargsill Scalar numeric: sill to fit. Can be wildcard but not
#'   recommended.
#' @param vario Numeric vector: variogram numbers in \code{expdata} to use.
#' @param npairswt Boolean: use number of pairs weighting.
#' @param invdistwt Boolean: use inverse distance weighting.
#' @param minpairs Scalar integer: minimum number of pairs.
#' @param fixhmaxvertanis Boolean: used fixed ahmax:avert anisotropy.
#' @param hmaxvertanis Scalar numeric: fixed ahmax:avert anisotropy.
#' @param fixhmaxhminanis Boolean: used fixed ahmax:ahmin anisotropy.
#' @param hmaxhminanis Scalar numeric: fixed ahmax:ahmin anisotropy.
#' @return A data frame with discretized variogram models for display. The side
#'   effect of this function is a system file {\code{sysout})} containing the
#'   variogram model parameters for the major, semi-major, and minor axes. In
#'   the case of a 2D variogram the semi-major and minor axes have the same
#'   parameters.
#' @export
#' @examples
varmodelR <- function(
  expdata, sysout, ndir=1, nst=10,
  c0="0.05:0.25", it=1, cc="?", azm=0, dip=0, tilt=0, ahmax="?", ahmin="?",
  avert="?", fit=TRUE, maxiter=100000, vargsill=1.0, vario=c(1, 2),
  npairswt=TRUE, invdistwt=TRUE, minpairs=10, fixhmaxvertanis=FALSE,
  hmaxvertanis=10, fixhmaxhminanis=FALSE, hmaxhminanis=1
  ) {

  # Build a parameter string.
  parstring <- c(
    "START OF PARAMETERS:",
    paste0(
      "disc-mod.data  \n",
      ndir, "  "
    )
  )
  for(dir in 1:ndir) {
    parstring <- c(
      parstring,
      paste0(azm[dir], "  ", dip[dir], "  ", "1000  0.5  ")
    )
  }
  parstring <- c(
    parstring,
    paste0(nst, "  ", c0, "  ")
  )
  # Build structure parameters.
  for(st in 1:nst) {
    parstring <- c(
      parstring,
      paste0(
        it, "  ", cc, "  ", azm[1], "  ", dip[1], "  ", tilt, "  \n",
        ahmax, "  ", ahmin, "  ", avert, "  "
      )
    )
  }
  # Additional fitting parameters.
  parstring <- c(
    parstring,
    paste0(
      as.numeric(fit), "  ", maxiter, "  \n",
      vargsill, "  \n",
      "1  \n",
      expdata,"  \n",
      ndir, "  ", paste(vario, collapse = "  "), "  \n",
      as.numeric(npairswt), "  ", as.numeric(invdistwt), "  ", minpairs, "  \n",
      as.numeric(fixhmaxvertanis), "  ", hmaxvertanis, "  \n",
      as.numeric(fixhmaxhminanis), "  ", hmaxhminanis, "  \n",
      sysout, "  "
    )
  )

  # Write parameters to a file.
  file_conn <- file("varmodel.par")
  writeLines(parstring, file_conn)
  close(file_conn)

  # Run the external program.
  shell("varmodel varmodel.par")

  # Import the discretized model for plotting.
  disc_vmod <- read_gslib("disc-mod.data")
  new_col_names <- c("set", "h", "np", "gamma", "vario", "azi", "dip")
  names(disc_vmod) <- new_col_names
  # Clean up some files.
  shell("del disc-mod.data")

  return(disc_vmod)

}
