#' 2D Point Sample Data.
#'
#' A dataset containing sample point data for a 2D (xy) domain. The samples
#'     record a concentration of metal the domain thickness and the accumulation
#'     (metal concentration × thickness).
#'
#' @docType data
#'
#' @usage data(samples_2d)
#'
#' @format A data frame with 44 rows and 6 variables:
#' \describe{
#'   \item{id}{unique point ID}
#'   \item{x}{x coordinate in metres}
#'   \item{y}{y coordinate in metres}
#'   \item{value}{value metal concentration in ppm}
#'   \item{thk}{domain thickness in metres}
#'   \item{accum}{metal accumulation in ppm·m}
#' }
"samples_2d"
