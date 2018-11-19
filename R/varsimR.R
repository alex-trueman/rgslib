
#' Calculated Variograms for Gridded Data
#'
#' Only supports 2D rotations.
#' Does not support lithology data file.
#' Does not support indicator variograms.
#'
#' @param data Data frame of gridded data.
#' @param vars Character vecotr of variables to calculate variograms.
#' @param xyz Character vecotr of coordinate column names.
#' @param dims Numeric vector of grid node/cell dimensions.
#' @param realz Scalar integer number of realizations.
#' @param standarize Scalar boolean: standardize variograms.
#' @param dirs Numeric vecotr of azimuth rotations.
#' @param nlags Number of lags to calculate.
#' @param single Boolean: produce a single variogram rather than direct and
#'   cross variograms.
#' @return Data frame variograms.
#' @export
varsimR <- function(
    data, vars, xyz=c("x", "y"), dims=c(1, 1), realz=1, standarize=FALSE,
    dirs=c(0, 90), nlags=10, single=FALSE
) {

    # Get information needed for parameter file.
    grid_def <- create_gslib_griddef(data, dims=dims, xyz=xyz, realz=realz)
    vars_i <- get_column_indices(data, vars)
    nvars <- length(vars)
    ndir <- length(dirs)
    # Number of variograms.
    if(nvars == 1 | single) {
        nvarios <- 1
    } else {
        nvarios <- (factorial(nvars) / (factorial(2) *
                factorial(nvars - 2))) + nvars
    }


    # Make parameter file.
    parstring <- c(
        "START OF PARAMETERS:  ",
        "lithology.dat  ",
        "0  0  ",
        "xxdata.dat  ",
        paste0(nvars, "  ", vars_i, "  "),
        "-1.0e21  1.0e21  ",
        "xxvarsims_reals.out  ",
        "xxvarsim_avg.out  ",
        paste(grid_def[c("n_x", "min_x", "dim_x")], collapse = "  "),
        paste(grid_def[c("n_y", "min_y", "dim_y")], collapse = "  "),
        paste(grid_def[c("n_z", "min_z", "dim_z")], collapse = "  "),
        paste0(realz, "  "),
        paste0(ndir, "  ", nlags, "  ")
    )
    # Unit vectors for each variograms.
    for(v in 1:ndir) {
        parstring <- c(
            parstring,
            paste(unit_vector(dirs[v]), collapse = "  ")
        )
    }
    # More parameters.
    parstring <- c(
        parstring,
        paste0(nvarios, "  ")
    )
    # Variogram setup.
    if(single & nvars == 2) {
        parstring <- c(
            parstring,
            paste0("1  2  2  ")
        )
    } else {
        for(v in 1:nvars) {
            parstring <- c(
                parstring,
                paste0(v, "  ", v, "  1  ")
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
                    paste0(t1start, "  ", t2, "  2  ")
                )
                t2 <- t2 + 1
                nruns <- nruns - 1
            }
            t1start <- t1start + 1
            t2start <- t2start + 1
        }
    }

    # Write parameters to a file.
    file_conn <- file("varsim.par")
    writeLines(parstring, file_conn)
    close(file_conn)

}
