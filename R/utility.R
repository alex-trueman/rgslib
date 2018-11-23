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


#' Create GSLIB Parameter File Variogram Model String.
#'
#' @param mvario A data frame of class variogramModel (`gstat` package).
#' @return A character string suitable for use in GSLIB parameter files.
mvario_parstring <- function(mvario){
    sill <- sum(mvario$psill)
    c0 <- mvario[mvario$model == "Nug",]$psill
    if(is.na(c0)) {c0 <- 0}
    xc0 <- mvario[mvario$model != "Nug",]
    nst <- nrow(xc0)
    parstring <- paste0(nst, "  ", c0, "  ", sill, "  ")
    for(st in 1:nst){
        xc0st <- xc0[st,]
        parstring <- c(
            parstring,
            paste(xc0st[c(9, 2, 4, 5, 6)], collapse = "  "),
            paste0(xc0st[3], "  ", xc0st[3] * xc0st[7], "  ",
                xc0st[3] * xc0st[7], "  ")
        )
    }
    return(parstring)
}

#' Create Coordinates for GSLIB-style Grid file.
#'
#' @param data Data frame with grid arrangements but no coordiates. Imported
#'   from a GeoEase system file with function like \code{read_gslib}.
#' @param grid_def Standard grid definition, which is a named numeric vectoor
#'   with elements (in order): n_x, n_y, n_z, min_x, min_y, min_z, dim_x,
#'   dim_y, dim_z, realz.
#'
#' @return Data frame same as input but with fields: r, x, y, z added.
add_coords <- function(data, grid_def) {

    # Get names of columns already in `data`.
    vars <- colnames(data)

    n_grid_points <- grid_def["n_x"] * grid_def["n_y"] * grid_def["n_z"]

    # All coordinates.
    grid_x <- seq(grid_def["min_x"], grid_def["min_x"] + (grid_def["dim_x"] *
            (grid_def["n_x"] - 1)), grid_def["dim_x"])
    grid_y <- seq(grid_def["min_y"], grid_def["min_y"] + (grid_def["dim_y"] *
            (grid_def["n_y"] - 1)), grid_def["dim_y"])
    grid_z <- seq(grid_def["min_z"], grid_def["min_z"] + (grid_def["dim_z"] *
            (grid_def["n_z"] - 1)), grid_def["dim_z"])

    # Calculate grids and realizations.
    data[, "r"] <- rep(1:grid_def["realz"], each = n_grid_points)
    data[, "x"] <- rep(grid_x, times = grid_def["realz"], each = 1)
    data[, "y"] <- rep(grid_y, times = grid_def["realz"],
        each = grid_def["n_x"])
    data[, "z"] <- rep(grid_z, times = grid_def["realz"],
        each = grid_def["n_x"] * grid_def["n_y"])

    # Arrange columns.
    data <- data[, c("r", "x", "y", "z", vars)]

    return(data)

}

#' Build a Variogram Calculation Parameter List from Stored Parameters
#'
#' @param data Data frame of stored parameters (see example for strict
#'   structure of the data frame).
#' @param domain Scalar numeric or character (matched to type in \code{data})
#'   domain, category, or zone code.
#' @param var Scalar character variable name for variogram.
#' @param ndims Scalar numeric 1--3: number of variogram dimensions (axes) with
#'   1 being major; 2 semi-major; and 3 minor.
#'
#' @return A list of axis variogram parameters suitable for input into variogram
#'   calculation function \link{varcalcR}.
#' @export
#'
#' @examples
#' vario_calc <- tibble::tribble(
#' ~domain, ~vars, ~axis, ~azm, ~azmtol, ~bndhorz, ~dip, ~diptol, ~bndvert,
#' ~tilt, ~nlags, ~dlag, ~lagtol,
#' 38, "NS_thk", 1, 90, 45, 1.0e21, 0, 90, 1.0e21, 0, 15, 10, 0.5,
#' 38, "NS_thk", 2, 180, 45, 1.0e21, 0, 90, 1.0e21, 0, 10, 10, 0.5,
#' 38, "NS_accum", 1, 90, 45, 1.0e21, 0, 90, 1.0e21, 0, 15, 10, 0.5,
#' 38, "NS_accum", 2, 180, 45, 1.0e21, 0, 90, 1.0e21, 0, 10, 10, 0.5,
#' 38, "NS_thkaccum", 1, 90, 45, 1.0e21, 0, 90, 1.0e21, 0, 15, 10, 0.5,
#' 38, "NS_thkaccum", 2, 180, 45, 1.0e21, 0, 90, 1.0e21, 0, 10, 10, 0.5
#' )
#' build_vario_par_list(vario_calc, 38, "NS_accum", 2)
build_vario_par_list <- function(data, domain, var, ndims) {
    pars <- data[data$domain == domain & data$vars == var,]
    pars <- subset(pars, select = -c(domain, vars))
    calcpars <- list()
    for(ax in 1:ndims) {
        pax <- unlist(pars[pars$axis == ax,])
        if(ax == 1) {
            calcpars[["major"]] <- pax
        } else if(ax == 2) {
            calcpars[["semi_major"]] <- pax
        } else {
            calcpars[["minor"]] <- pax
        }
    }
    return(calcpars)
}


#' Build a Grid Defintion from Stored Parameters
#'
#' @param data Data frame of stored parameters with specific numeric columns:
#'  domain, nx, ny, nz, min_x, min_y, min_z, dim_x, dim_y, dim_z, realz
#' @param domain Scalar numeric or character (matched to domain in \code{data})
#'   domain, category, or zone code.
#'
#' @return A named numeric vector defining a grid.
#' @export
build_grid_def <- function(data, domain) {
    pars <- unlist(data[data$domain == domain,])
    return(pars)
}



#' Populate a Data Fram Grid Representation Using a Grid Definition
#'
#' @param grid_def Standard grid definition, which is a named numeric vector
#'   with elements (in order): n_x, n_y, n_z, min_x, min_y, min_z, dim_x,
#'   dim_y, dim_z, realz.
#'
#' @return A data frame with grid coordinates.
#' @export
create_grid <- function(grid_def) {

    n_grid_points <- grid_def["n_x"] * grid_def["n_y"] * grid_def["n_z"]

    data <- data.frame(
        r = integer(length = n_grid_points),
        x = numeric(length = n_grid_points),
        y = numeric(length = n_grid_points),
        z = numeric(length = n_grid_points)
    )

    # All coordinates.
    grid_x <- seq(grid_def["min_x"], grid_def["min_x"] + (grid_def["dim_x"] *
            (grid_def["n_x"] - 1)), grid_def["dim_x"])
    grid_y <- seq(grid_def["min_y"], grid_def["min_y"] + (grid_def["dim_y"] *
            (grid_def["n_y"] - 1)), grid_def["dim_y"])
    grid_z <- seq(grid_def["min_z"], grid_def["min_z"] + (grid_def["dim_z"] *
            (grid_def["n_z"] - 1)), grid_def["dim_z"])

    # Calculate grids and realizations.
    data[, "r"] <- rep(1:grid_def["realz"], each = n_grid_points)
    data[, "x"] <- rep(grid_x, times = grid_def["realz"], each = 1)
    data[, "y"] <- rep(grid_y, times = grid_def["realz"],
        each = grid_def["n_x"])
    data[, "z"] <- rep(grid_z, times = grid_def["realz"],
        each = grid_def["n_x"] * grid_def["n_y"])

    # Arrange columns.
    data <- data[, c("r", "x", "y", "z")]

    return(data)

}


#' Extract Regulalarly Spaced Samples from a Grid
#'
#' @param grid Data fram input grid.
#' @param sample_def Standard grid definition, which is a named numeric vector
#'   with elements (in order): n_x, n_y, n_z, min_x, min_y, min_z, dim_x,
#'   dim_y, dim_z, realz. Used to define regular sample pattern.
#' @param xyz Coordinates in \code{grid}.
#'
#' @return Data frame of points sampled from \code{grid}.
#' @importFrom dplyr semi_join
#' @export
sample_grid_extract <- function(grid, sample_def, xyz=c("x", "y")) {

    # Creat sample locations.
    samp_grid <- create_grid(sample_def)

    # Sample the input grid.
    samples <- semi_join(grid, samp_grid, by = xyz)

    return(samples)

}
