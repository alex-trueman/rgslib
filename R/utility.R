#' Calculate Unit Vector Given Azimuth and Dip
#'
#' This function produces unit vectors for use in programs such as \code{varsim}
#' where a unit vecotr is required to define variogram directions in gridded
#' data.
#'
#' I have validated this function against the examples given in the GSLIB
#' book (2nd ed.) on p.48. All except the last example in the book work. The
#' examples are reproduced here as part of the function docs. I am not sure why
#' this doesn't work for the last example.
#'
#' For now this function appears robust 2D (azimuth-only) directions and
#' vertical dip.
#'
#' @param azm Azimuth in degrees clockwise from north looking down z-axis.
#' @param dip Dip in degrees from horizontal, negative down.
#'
#' @return Numeric unit vector of length three.
#' @export
#' @examples
#' unit_vector(90) # Aligned along x-axis.
#' unit_vector(0) # Aligned along the y-axis.
#' unit_vector(0,-90) # Aligned along the z-axis.
#' unit_vector(45) # Horizontal at 45 degrees from y.
#' unit_vector(135) # Horizontal at 135° from y.
#' # This one doesn't reproduce the resultin the GSLIB book.
#' unit_vector(225, -45) # Dipping at -45°, 225° clockwise from y.
unit_vector <- function(azm=0, dip=0) {
    uv <- c(
        round(sin(pi * azm / 180) * cos(pi * dip / 180), 2),
        round(cos(pi * azm / 180) * cos(pi * dip / 180), 2),
        round(-sin(pi * dip / 180), 2)
        )
    uv <- uv / max(abs(uv))
    return(uv)
}

#' Get Vector of Specific Column Indices of from a Data Frame.
#'
#' @param data Data frame
#' @param vars Character vector of column names in \code{data}.
#'
#' @return Numeric vector of column indices.
get_column_indices <- function(data, vars) {
    col_i <- which(colnames(data) %in% vars)
    return(col_i)
}

#' Make Data Frame Containing Valid GSLIB Variogram Model Structure Types
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
