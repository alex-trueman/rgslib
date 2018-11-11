#' Run the CCG \code{vl_lmc} to Automatically Model Multivariate Variograms
#'
#' \code{vl_lmcR} is an interface to the Fortran program \code{varfit_lmc}.
#' It auotmaticaly models direct and cross variograms given experimental points
#' and models fo the direc variograms.
#'
#'
#' @param nvars Scalar numeric: number of variables being modelled.
#' @param corr0 Numeric vector: cross-variogram nugget effects.
#' @param corrmatrix Numeric matrix: correlation matrix (e.g., from \code{cor}).
#' @param varcalcs Character: prefix of experimental variogram files.
#' @param varfits Character: prefix of direct variogram model files.
#' @param invdistwt Boolean: inverse distance weighting.
#' @param npairswt Boolean: number of pairs weighting.
#' @param minpairs Scalar integer: minimum pairs.
#' @param maxiter Scalar integer: number of fitting iterations.
#' @param varfitslmc Character: prefix of output variogram model files.
#' @param corrs Character: output correlation matrix system file.
#'
#' @return A list with two data frames: `model` contains the variogram models
#'   and `corrs` contains the model cross-variogram sills for validation.
#' @export
#' @examples
vl_lmcR <- function(nvars, corr0, corrmatrix, varcalcs="vg_", varfits="vm_",
  invdistwt=TRUE, npairswt=TRUE, minpairs=10, maxiter=100, varfitslmc="vl_",
  corrs="corrs.txt") {

  # Build a parameter string.
  parstring <- c(
    "START OF PARAMETERS:",
    paste0(
      nvars, "  \n",
      varcalcs, "  \n",
      varfits, "  \n",
      paste(corrmatrix, collapse = "  "),
      paste(corr0, collapse = "  "),
      invdistwt, "  ", npairswt, "  ", minpairs, "  \n",
      maxiter, "  \n",
      varfitslmc, "  \n",
      corrs, "  "
    )
  )

  # Write parameters to a file.
  file_conn <- file("vl_lmc.par")
  writeLines(parstring, file_conn)
  close(file_conn)

  # Run the `vl_lmc` program.
  shell("vl_lmc vl_lmc.par")

  invisible(nvars)

}
