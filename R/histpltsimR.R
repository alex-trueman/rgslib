#' Generate Histogram Data for Simulations
#'
#' \code{histpltsimR} is an interface to CCG Fortran program \code{histpltsim}.
#' It generates histogram data for multiple simulations and an optional
#' refernece dataset (e.g., samples). It is most useful for comparing the
#' simulated distribution with the input sample distribution as validation.
#'
#' Functionality is different compared with the Fortran program. The following
#' differences are present:
#' \itemize{
#'   \item Support for a file with lithology information.
#'   \item Trimming limits are fixed.
#'   \item Plot specific parameters title, stats position, and refernece value
#'     for box plot are fixed.
#'   \item Input data must be simulations, this is fixed.
#'   \item Output always includes the numeric data, this is fixed.
#'   \item Histograms always start at simulation 1 and end at \code{realz}.
#'   \item Attribute minimum and maximum are derived form the data.}
#'
#' These differences may be changed in future updates.
#'
#'
#' @param sims fff
#' @param simvar fff
#' @param samples fff
#' @param sampvar fff
#' @param simweight fff
#' @param sampweight fff
#' @param realz fff
#' @param ngrid fff
#' @param nquant fff
#' @param nvalues fff
#' @param logscale fff
#' @param round fff
#'
#' @return
#' @export
#'
#' @examples
histpltsimR <- function(sims, simvar, samples, sampvar, simweight=NULL,
    sampweight=NULL, realz=1, ngrid=c(20, 20, 20), nquant=1000, nvalues=5000,
    logscale=FALSE, round=2){

    print("function histpltsimR not implimented")

}
